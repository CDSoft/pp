{- PP

Copyright (C) 2015, 2016 Christophe Delord

http://www.cdsoft.fr/pp

This file is part of PP.

PP is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

PP is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with PP.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE CPP #-}

import Control.Monad
import System.IO
import qualified System.IO.Strict as SIO
import System.IO.Temp
import System.Environment
import System.Directory
import Data.List
import Data.Char
import Data.Maybe
import System.Process hiding (env)
import System.FilePath
import Data.Time
import Foreign.C.Types
import Foreign.Ptr
import Foreign hiding (void)

-- Set of chars
type Chars = String

-- symbol type of a definition in the environment
data Var = Def String           -- user macro definition
         | EnvVar String        -- environment variable
         | Lang                 -- current language
         | FileFormat           -- current file format
         | MainFile             -- main file name (given on the command line)
         | CurrentFile          -- current file name (can be included in other files)
         | LitFile FilePath     -- literate file name
         | LitMacro String      -- literate macro name
         | LitLang String       -- language of a literate file or macro
         | LitFlush             -- tag to flush literate content
         | ImagePath            -- prefix added to the image path (diagrams)
         deriving (Eq)

data Val = Val String           -- regular (stripped) value
         | Block String         -- code block (unstripped) value
         deriving (Eq)

-- Preprocessor environment (lookup table)
type Env = [(Var, Val)]

-- A macro takes an environment and arguments
-- and returns a new environment and the result of the macro as a string.
type Macro = Env -> [Val] -> IO (Env, String)

-- A preprocessor takes an environment and a string to preprocess
-- and returns a new environment and the preprocessed string.
type Prepro = Env -> String ->  IO (Env, String)

-- Diagram types managed by pp
data DiagramRuntime = Graphviz | PlantUML | Ditaa deriving (Show)

-- The main function builds the initial environment, parses the input
-- and print the output on stdout.
main :: IO ()
main = do
    -- work with UTF-8 documents
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    -- initial environment
    -- get $LANG (en, fr, ...)
    envVars <- getEnvironment
    let lang = case lookup "LANG" envVars of
                    Just ('C':_) -> "en"
                    Just val -> map toLower $ take 2 val
                    Nothing -> ""
    -- get $FORMAT (html, pdf, ...)
    let fmt = map toLower $ fromMaybe "" (lookup "FORMAT" envVars)
    -- the initial environment contains the language, the format and the environment variables
    let env = (Lang, Val lang) : (FileFormat, Val fmt) : [(EnvVar name, Val val) | (name, val) <- envVars]
    -- parse the arguments and produce the preprocessed output
    (env', doc) <- getArgs >>= doArgs env
    -- just write the preprocessed output to stdout
    putStr doc
    -- finally save the literate content (if any)
    saveLiterateContent (filter isLitMacro env') env'

-- "doArgs env args" parses the command line arguments
-- and returns an updated environment and the preprocessed output
doArgs :: Env -> [String] -> IO (Env, String)

-- Parse all the arguments.
doArgs env (arg:args) = do
    (env', doc) <- doArg env arg
    (env'', doc') <- doArgs env' args
    return (env'', doc ++ doc')

-- No more argument
-- mainFileTag is put in the environment only when a file has been preprocessed.
-- This variable is not set when no file is given on the command line.
-- In this case, pp preprocesses stdin.
doArgs env [] = case lookup MainFile env of
    -- nothing has been preprocessed, let's try stdin
    Nothing -> doArg env "-"
    -- something has already been preprocessed
    Just _ -> return (env, "")

-- "doArg env arg" parses one argument
-- and returns an updated environment and the output produced by the argument.
doArg :: Env -> String -> IO (Env, String)

-- "doArg env "-Dname=value"" adds a new definition to the environment.
doArg env ('-':'D':def) = return ((Def name, Val (drop 1 value)) : clean (Def name) env, "")
    where (name, value) = span (/= '=') def

-- "doArg env "-Uname"" removes a definition from the environment.
doArg env ('-':'U':name) = return (clean (Def name) env, "")

-- "doArg env "-fr|-en"" changes the current language
doArg env ('-':lang) | lang' `elem` langs =
    return ((Lang, Val lang') : clean Lang env, "") where lang' = map toLower lang

-- "doArg env "-html|-pdf|-odt|-epub|-mobi"" changes the current format
doArg env ('-':fmt) | fmt' `elem` formats =
    return ((FileFormat, Val fmt') : clean FileFormat env, "") where fmt' = map toLower fmt

-- "doArg env "-img"" changes the output image path prefix
doArg env ('-':'i':'m':'g':'=':prefix) =
    return ((ImagePath, Val prefix) : clean ImagePath env, "")

-- Other arguments starting with "-" are invalid.
doArg _ ('-':arg) | not (null arg) = error $ "Unexpected argument: " ++ arg

-- "doArg env filename" preprocessed the content of a file using the current environment.
-- The mainFileTag variable is added to the environment.
-- It contains the name of the file being preprocessed.
doArg env name = ppFile ((MainFile, Val name) : env) name

-- "saveLiterateContent macros env" saves all the literate contents recorded in the environment.
-- "macros" is the list of literate content that must not be saved
-- (they are used as macros to build other literate contents).
saveLiterateContent :: Env -> Env -> IO ()

-- ignore files that have not been modified since the last call to flushLit
saveLiterateContent _ ((LitFlush, _) : _) =
    return ()

-- macros shall not be saved
saveLiterateContent macros ((mac@(LitMacro _), _) : env) =
    saveLiterateContent macros (clean mac env)

-- file content shall be expanded with macro contents
-- previous definitions of the same file are removed from the list to avoid writing old intermediate definitions
saveLiterateContent macros ((file@(LitFile name), content) : env) = do
    writeFileUTF8 name (expandLit macros (fromVal content))
    saveLiterateContent macros (clean file env)

-- other definitions are not literate things
saveLiterateContent macros (_ : env) =
    saveLiterateContent macros env

saveLiterateContent _ [] =
    return ()

-- "isLitMacro (name, value)" tells if an environment entry is a literate content macro.
-- An entry is a literate content if its name is the name of a literate content macro.
isLitMacro :: (Var, Val) -> Bool
isLitMacro = isLitMacroName . fst

-- "isLitMacroName name" tells if a name is a literate content macro name.
isLitMacroName :: Var -> Bool
isLitMacroName (LitMacro _) = True
isLitMacroName _ = False

-- "ppFile env name" preprocess a file using the current environment
-- env returns an updated environment and the preprocessed output.
-- The environment contains the name of the file in currentFileTag.
ppFile :: Env -> FilePath -> IO (Env, String)
ppFile env name = do
    -- file name of the caller
    let caller = getSymbol env CurrentFile
    -- read the file to preprocess
    content <- readFileUTF8 name
    -- preprocess the file in an environment containing the filename
    (env', doc) <- pp ((CurrentFile, Val name) : clean CurrentFile env) content
    -- return the environment (with the file name of the caller) and the preprocessed output
    return ((CurrentFile, caller) : clean CurrentFile env', doc)

-- "readFileUTF8 name" reads an UTF-8 file.
-- If name is "-", it reads stdin.
readFileUTF8 :: FilePath -> IO String
readFileUTF8 "-" = getContents
readFileUTF8 name = do
    h <- openFile name ReadMode
    hSetEncoding h utf8
    -- the file must not be read lazily
    -- (in some case we want to be able to read files
    -- that have been previously produced by the same document)
    content <- SIO.hGetContents h
    hClose h
    return content

-- "writeFileUTF8 name content" writes an UTF-8 file.
-- If name is "-", it writes to stdout.
writeFileUTF8 :: FilePath -> String -> IO ()
writeFileUTF8 "-" content = putStr content
writeFileUTF8 name content = do
    handle <- openBinaryFile name WriteMode
    hWriteFileUTF8 handle content
    hClose handle

-- "hwriteFileUTF8 handle content" writes an UTF-8 file.
hWriteFileUTF8 :: Handle -> String -> IO ()
hWriteFileUTF8 handle content = do
    hSetEncoding handle utf8
    hPutStr handle content

-- "readProcessUTF8 cmd arg" executes "cmd args"
-- and returns the standard output produced by the command.
readProcessUTF8 :: String -> [String] -> IO String
readProcessUTF8 cmd args = do
    (_, Just hOut, _, hProc) <- createProcess (proc cmd args) { std_out = CreatePipe }
    hSetEncoding hOut utf8
    out <- SIO.hGetContents hOut
    _ <- waitForProcess hProc
    return out

-- language list
langs :: [String]
langs = ["fr", "en"]

-- format list
formats :: [String]
formats = ["html", "pdf", "odt", "epub", "mobi"]

-- literate programming macros
litMacroTagChar :: Char
litMacroTagChar = '@'

-- Graphiviz diagrams
graphvizDiagrams :: [String]
graphvizDiagrams = words "dot neato twopi circo fdp sfdp patchwork osage"

-- PlantUML diagrams
plantumlDiagrams :: [String]
plantumlDiagrams = words "uml"

-- Ditaa diagrams
ditaaDiagrams :: [String]
ditaaDiagrams = words "ditaa"

-- list of builtin macros
builtin :: [(String, Macro)]
builtin = [ ("def", define)         , ("undef", undefine)
          , ("define", define)      , ("undefine", undefine)
          , ("ifdef", ifdef)        , ("ifndef", ifndef)
          , ("ifeq", ifeq)          , ("ifne", ifne)
          , ("rawdef", rawdef)

          , ("inc", include)        , ("include", include)
          , ("raw", raw)
          , ("rawinc", rawinc)      , ("rawinclude", rawinc)

          , ("exec",    macropp (script "exec"    "sh" "" ".sh"))
          , ("rawexec", script "rawexec" "sh" "" ".sh")

          , ("mdate", mdate)

          , ("env", readEnv)

          , ("main", mainFile)
          , ("file", currentFile)
          , ("lang", currentLang)
          , ("format", currentFormat)

          , ("add", add)

          , ("lit", lit)            , ("literate", lit)
          , ("flushlit", flushlit)  , ("flushliterate", flushlit)
          , ("src", source)         , ("source", source)

          ]
          ++ [ (diag, diagram Graphviz diag ""          "")        | diag <- graphvizDiagrams]
          ++ [ (diag, diagram PlantUML diag "@startuml" "@enduml") | diag <- plantumlDiagrams]
          ++ [ (diag, diagram Ditaa    diag ""          "")        | diag <- ditaaDiagrams]
          ++ [ ("sh",      script "sh"      "sh"         ""          ".sh")
             , ("bash",    script "bash"    "bash"       ""          ".sh")
             , ("bat",     script "bat"     cmdexe       "@echo off" ".bat")
             , ("python",  script "python"  "python"     ""          ".py")
             , ("haskell", script "haskell" "runhaskell" ""          ".hs")
          ]
          ++ [ (lang, language lang) | lang <- langs]
          ++ [ (fmt, format fmt) | fmt <- formats]

-- shell command interpretor for Windows .bat scripts
cmdexe :: String
cmdexe =
#if linux_HOST_OS || darwin_HOST_OS
    "wine cmd /c"
#else
    "cmd /c"
#endif

fromVal :: Val -> String
fromVal (Val s) = s
fromVal (Block s) = s

ppAndStrip :: Env -> Val -> IO (Env, String)
ppAndStrip env (Val s) = do
    (env', doc) <- pp env s
    return (env', strip doc)
ppAndStrip env (Block s) =
    pp env s

ppAndStrip' :: Env -> Val -> IO String
ppAndStrip' env val = fmap snd (ppAndStrip env val)

-- \define(name)(value) adds (Def name, value) to the environment.
-- name is preprocessed but not value. The value of the macro is preprocessed
-- when the macro is evaluated (to allow macros with parameters).
define :: Macro
define env [name, value] = do
    name' <- ppAndStrip' env name
    return ((Def name', value) : clean (Def name') env, "")
define env [name] = define env [name, Val ""]
define _ _ = arityError "define" [1,2]

-- \undefine(name) removes (Def name) from the environment
undefine :: Macro
undefine env [name] = do
    name' <- ppAndStrip' env name
    return (clean (Def name') env, "")
undefine _ _ = arityError "undefine" [1]

-- `clean name env` removes an outdated variable from the environment
clean :: Var -> Env -> Env
clean var = filter ((/=var) . fst)

-- \ifdef(name)(t)(e) preprocesses name. If the result is the name of an
-- already defined symbol in the environment, it preprocessed t, otherwise e.
-- e is optional.
ifdef :: Macro
ifdef env [name, t, e] = do
    name' <- ppAndStrip' env name
    ppAndStrip env $ case lookup (Def name') env of
                Just _ -> t
                Nothing -> e
ifdef env [name, t] = ifdef env [name, t, Val ""]
ifdef _ _ = arityError "ifdef" [1, 2]

-- \ifndef(name)(t)(e) is equivalent to \ifdef(name)(e)(t)
ifndef :: Macro
ifndef env [name, t, e] = ifdef env [name, e, t]
ifndef env [name, t] = ifdef env [name, Val "", t]
ifndef _ _ = arityError "ifndef" [1, 2]

-- \ifeq(x)(y)(t)(e) preprocesses x and y. If they are equal
-- (spaces are ignored), it preprocessed t, otherwise e.
-- e is optional.
ifeq :: Macro
ifeq env [x, y, t, e] = do
    x' <- ppAndStrip' env x
    y' <- ppAndStrip' env y
    ppAndStrip env (if noSpace x' == noSpace y' then t else e)
        where noSpace = filter (not . isSpace)
ifeq env [x, y, t] = ifeq env [x, y, t, Val ""]
ifeq _ _ = arityError "ifeq" [3, 4]

-- \ifne(x)(y)(t)(e) is equivalent to \ifeq(x)(y)(e)(t)
ifne :: Macro
ifne env [x, y, t, e] = ifeq env [x, y, e, t]
ifne env [x, y, t] = ifeq env [x, y, Val "", t]
ifne _ _ = arityError "ifne" [3, 4]

-- \rawdef(name) preprocesses name and emits the raw definition
-- (no preprocessing)
rawdef :: Macro
rawdef env [name] = do
    name' <- ppAndStrip' env name
    return (env, fromVal (getSymbol env (Def name')))
rawdef _ _ = arityError "rawdef" [1]

-- \include(name) preprocesses name, locates the file and preprocesses its content
include :: Macro
include env [name] = ppAndStrip' env name >>= locateFile env >>= ppFile env
include _ _ = arityError "include" [1]

-- "locateFile env name" searches for a file in the directory of the main file
-- or in the directory of the current file or in the current directory
locateFile :: Env -> FilePath -> IO FilePath
locateFile env name = do
    let name' = case name of
                    ('~' : '/' : relname) -> fromVal (getSymbol env (EnvVar "HOME")) </> relname
                    _ -> name
    let path = map (takeDirectory . fromVal . getSymbol env) [CurrentFile, MainFile] ++ ["."]
    found <- findFile path name'
    case found of
        Just foundFile -> return foundFile
        Nothing -> fileNotFound name

-- \raw(text) emits text unpreprocessed
raw :: Macro
raw env [src] = return (env, fromVal src)
raw _ _ = arityError "raw" [1]

-- \rawinclude(name) preprocesses name, locates the file and emits it unpreprocessed
rawinc :: Macro
rawinc env [name] = do
    doc <- ppAndStrip' env name >>= locateFile env >>= readFileUTF8
    return (env, doc)
rawinc _ _ = arityError "rawinclude" [1]

-- strip removes spaces at the beginning and the end of a string
strip :: String -> String
strip = halfStrip . halfStrip where halfStrip = dropWhile isSpace . reverse

-- \mdate(file1 ... filen)(file'1 ... file'n)... preprocesses the list of
-- filenames (space separated) and returns the most recent modification date.
-- If no file is given, the date of the main file is returned.
mdate :: Macro
mdate env files = do
    files' <- mapM (ppAndStrip' env) files
    files'' <- mapM (locateFile env) $ concatMap words files' ++ [fromVal (getSymbol env MainFile) | null files]
    times <- mapM getModificationTime files''
    let lastTime = maximum times
    return (env, formatTime (myLocale $ fromVal $ getSymbol env Lang) "%A %-d %B %Y" lastTime)

-- "myLocale lang" returns the date format description for a given language.
myLocale :: String -> TimeLocale
-- french locale date format
myLocale "fr" = TimeLocale {
                    wDays = [("Dimanche","Dim")
                            ,("Lundi","Lun")
                            ,("Mardi","Mar")
                            ,("Mercredi","mer")
                            ,("Jeudi","Jeu")
                            ,("Vendredi","Ven")
                            ,("Samedi","Sam")],
                    months = [("Janvier","Jan")
                             ,("Février","Fev")
                             ,("Mars","Mar")
                             ,("Avril","Avr")
                             ,("Mai","Mai")
                             ,("Juin","Jui")
                             ,("Juillet","Jul")
                             ,("Août","Aou")
                             ,("Septembre","Sep")
                             ,("Octobre","Oct")
                             ,("Novembre","Nov")
                             ,("Décembre","Déc")],
                    amPm = ("AM","PM"),
                    knownTimeZones = [],
                    dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y",
                    dateFmt = "%d/%m/%y",
                    timeFmt = "%H:%M:%S",
                    time12Fmt = "%I:%M:%S %p"
                }
-- English is the default locale
myLocale _ = defaultTimeLocale

-- \env(name) preprocesses name, reads an environment variable (in env)
-- and preprocessed the value of the environment variable.
readEnv :: Macro
readEnv env [name] = do
    name' <- ppAndStrip' env name
    case lookup (EnvVar name') env of
        Just val -> ppAndStrip env val
        Nothing -> return (env, "")
readEnv _ _ = arityError "env" [1]

-- \main returns the name of the main file (given on the command line)
mainFile :: Macro
mainFile env [] = return (env, fromVal (fromMaybe (Val "-") (lookup MainFile env)))
mainFile _ _ = arityError "main" [0]

-- \file returns the name of the current file (\main or any file included from \main)
currentFile :: Macro
currentFile env [] = return (env, fromVal (fromMaybe (Val "-") (lookup CurrentFile env)))
currentFile _ _ = arityError "file" [0]

-- \lang returns the current language ("fr" or "en")
currentLang :: Macro
currentLang env [] = return (env, fromVal (getSymbol env Lang))
currentLang _ _ = arityError "lang" [0]

-- \format returns the current output format ("html", "pdf", "odt", "epub" or "mobi")
currentFormat :: Macro
currentFormat env [] = return (env, fromVal (getSymbol env FileFormat))
currentFormat _ _ = arityError "format" [0]

-- \add(name)(val) preprocesses name and val and adds val to the integer value
-- stored in name. If name is not defined its value is 0.
add :: Macro
add env [name, val] = do
    name' <- ppAndStrip' env name
    let val0 = fromVal $ getSymbol env (Def name')
    val1 <- ppAndStrip' env val
    let env' = (Def name', Val (show (atoi val0 + atoi val1))) : clean (Def name') env
    return (env', "")
add env [name] = add env [name, Val "1"]
add _ _ = arityError "add" [1, 2]

-- atoi s converts s to an integer (0 if empty or not an integer)
atoi :: String -> Integer
atoi s = case reads s of
            [(i, "")] -> i
            _ -> 0

-- literate programming macro
lit :: Macro

-- \lit(name)(lang)(content) appends content to the file or macro name.
-- In the markdown output, the content will be colored according to the language lang.
lit env [name, lang, content] = do
    name' <- ppAndStrip' env name
    lang' <- ppAndStrip' env lang
    content' <- ppAndStrip' env content
    let env' = litAppend env name' (Just lang') content'
    let formatedCode = litShow (Just lang') content'
    return (env', formatedCode)

-- \lit(name)(lang)(content) appends content to the file or macro name.
-- The current language is the previously defined language or the default
-- one according to name.
lit env [name, content] = do
    name' <- ppAndStrip' env name
    content' <- ppAndStrip' env content
    let lang = litLang env name'
    let env' = litAppend env name' lang content'
    let formatedCode = litShow lang content'
    return (env', formatedCode)

-- \lit(name) emits the current content of a literate file or macro.
lit env [name] = do
    name' <- ppAndStrip' env name
    let lang = litLang env name'
    let content = litGet env name'
    let formatedCode = litShow lang content
    return (env, formatedCode)

lit _ _ = arityError "lit" [1, 2, 3]

-- source file inclusion
source :: Macro

-- \src(name)(lang) reads a source file.
-- In the markdown output, the content will be colored according to the language lang.
source env [name, lang] = do
    (env', _) <- flushlit env []
    name' <- ppAndStrip' env' name
    lang' <- ppAndStrip' env' lang
    content <- readFileUTF8 name'
    let formatedCode = litShow (Just lang') content
    return (env', formatedCode)

-- \src(name) reads a source file.
-- The language is the default one according to name.
source env [name] = do
    (env', _) <- flushlit env []
    name' <- ppAndStrip' env' name
    let lang = litLang env' name'
    content <- readFileUTF8 name'
    let formatedCode = litShow lang content
    return (env', formatedCode)

source _ _ = arityError "src" [1, 2]

-- "litGet env name" gets the current content of a literate file or macro
-- in the environment.
litGet :: Env -> String -> String
litGet env name = fromVal $ getSymbol env (litContentTag name)

-- "litAppend env name lang content" appends content to a literate file or macro
-- and record the language lang in the environment
litAppend :: Env -> String -> Maybe String -> String -> Env
litAppend env name lang content = env''
    where
        contentTag = litContentTag name
        oldContent = litGet env name
        env' = (contentTag, Val (oldContent++content)) : (clean contentTag . clean (LitLang name)) env
        env'' = case lang of
            Just lang' -> (LitLang name, Val lang') : env'
            _ -> env'

-- "litShow lang content" format content using the language lang.
-- The real format will be made by Pandoc.
litShow :: Maybe String -> String -> String
litShow lang content = case lang of
            Just lang' -> unlines [codeBlock ++ " {." ++ lang' ++ "}", content, codeBlock]
            Nothing    -> unlines [codeBlock, content, codeBlock]
    where
        codeBlock = replicate 70 '~'

-- litContentTag generates an entry for a literate file or macro
-- in the environment.
-- If name starts with a '@', its a literate macro, not an actual file.
litContentTag :: String -> Var
litContentTag (c:name) | c == litMacroTagChar = LitMacro name
litContentTag name = LitFile name

-- "litLang env name" looks for the language of the literate file or macro
-- in the environment if it has already been defined or tries to guess it
-- from its name.
litLang :: Env -> String -> Maybe String
litLang env name = case (lookup (LitLang name) env, defaultLitLang name) of
    (Just lang, _) -> Just (fromVal lang)
    (_, Just lang) -> Just lang
    _ -> Nothing

-- default language deduced from the name of the file
-- (see "pandoc --version")
defaultLitLang :: String -> Maybe String
defaultLitLang name = case (map toLower (takeBaseName name), map toLower (takeExtension name)) of
    (_, ".adb")         -> Just "ada"
    (_, ".ads")         -> Just "ada"
    (_, ".awk")         -> Just "awk"
    (_, ".bash")        -> Just "bash"
    (_, ".sh")          -> Just "bash"
    (_, ".bib")         -> Just "bibtex"
    (_, ".c")           -> Just "c"
    (_, ".cc")          -> Just "c"
    (_, ".cpp")         -> Just "c"
    (_, ".c++")         -> Just "c"
    (_, ".h")           -> Just "c"
    (_, ".hpp")         -> Just "c"
    (_, ".h++")         -> Just "c"
    (_, ".lisp")        -> Just "commonlisp"
    (_, ".cs")          -> Just "cs"
    (_, ".css")         -> Just "css"
    (_, ".diff")        -> Just "diff"
    (_, ".patch")       -> Just "diff"
    (_, ".dot")         -> Just "dot"
    (_, ".gv")          -> Just "dot"
    (_, ".dtd")         -> Just "dtd"
    (_, ".erl")         -> Just "erlang"
    (_, ".f")           -> Just "fortran"
    (_, ".f90")         -> Just "fortran"
    (_, ".fs")          -> Just "fsharp"
    (_, ".hs")          -> Just "haskell"
    (_, ".html")        -> Just "html"
    (_, ".htm")         -> Just "html"
    (_, ".ini")         -> Just "ini"
    (_, ".java")        -> Just "java"
    (_, ".js")          -> Just "javascript"
    (_, ".json")        -> Just "json"
    (_, ".latex")       -> Just "latex"
    (_, ".tex")         -> Just "latex"
    (_, ".l")           -> Just "lex"
    (_, ".lhs")         -> Just "literatehaskell"
    (_, ".lua")         -> Just "lua"
    (_, ".m4")          -> Just "m4"
    ("makefile", _)     -> Just "makefile"
    (_, ".mak")         -> Just "makefile"
    (_, ".md")          -> Just "markdown"
    (_, ".ml")          -> Just "ocaml"
    (_, ".pas")         -> Just "pascal"
    (_, ".p")           -> Just "pascal"
    --(_, ".pl")          -> Just "perl"
    (_, ".pl")          -> Just "prolog"
    (_, ".pro")         -> Just "prolog"
    (_, ".py")          -> Just "python"
    (_, ".r")           -> Just "r"
    (_, ".rst")         -> Just "rest"
    (_, ".rest")        -> Just "rest"
    (_, ".rb")          -> Just "ruby"
    (_, ".rs")          -> Just "rust"
    (_, ".scm")         -> Just "scheme"
    (_, ".tcl")         -> Just "tcl"
    (_, ".tcsh")        -> Just "tcsh"
    (_, ".csh")         -> Just "tcsh"
    (_, ".texi")        -> Just "texi"
    (_, ".vhdl")        -> Just "vhdl"
    (_, ".xml")         -> Just "xml"
    (_, ".xlst")        -> Just "xlst"
    (_, ".y")           -> Just "yacc"
    (_, ".yaml")        -> Just "yaml"
    (_, ".zsh")         -> Just "zsh"
    _ -> Nothing

-- flushlit writes the literal files that have not been written yet.
-- The tag LitFlush in the environment is used to know which files have not
-- been written yet.
flushlit :: Macro
flushlit env [] = do
    saveLiterateContent (filter isLitMacro env) env
    return ((LitFlush, Val "") : clean LitFlush env, "")
flushlit _ _ = arityError "flushlit" [0]

-- "expandLit macros text" expand literal macros in a string.
-- macros is a lookup table containing all the literal macros
expandLit :: Env -> String -> String
expandLit macros (c0:s)
    | c0 == litMacroTagChar = content' ++ expandLit macros s'
    | otherwise = c0 : expandLit macros s
    where
        (name, s') = span (\c -> isAlphaNum c || c == '_') s
        content' = case lookup (LitMacro name) macros of
                    Nothing -> c0:name
                    Just content -> expandLit macros (fromVal content)
expandLit _ [] = []

-- diagram generates a GraphViz, PlantUML or ditaa diagram.
-- The metadata file associated to the diagram source file contains
-- additional information that can not be in the source file but
-- are required to know if the image shall be regenerated or not
-- (thanks to Vittorio Romeo for the suggestion).
diagram :: DiagramRuntime -> String -> String -> String -> Macro
diagram runtime diag header footer env [path, title, code] = do
    path' <- ppAndStrip' env path
    title' <- ppAndStrip' env title
    code' <- pp' env (fromVal code)
    let code'' = unlines [header, code', footer]
    let (gv, dat, img, url, attrs) = parseImageAttributes env path'
    let metaData = unlines [ "Diagram metadata (generated by pp)"
                           , "Generator: " ++ show runtime
                           , "Type     : " ++ diag
                           , "Source   : " ++ gv
                           , "Image    : " ++ img
                           ]
    oldCodeExists <- doesFileExist gv
    oldCode <- if oldCodeExists then readFileUTF8 gv else return ""
    oldMetaDataExists <- doesFileExist dat
    oldMetaData <- if oldMetaDataExists then readFileUTF8 dat else return ""
    when (code'' /= oldCode || metaData /= oldMetaData) $ do
        writeFileUTF8 dat metaData
        writeFileUTF8 gv code''
        void $ case runtime of
            Graphviz ->
                readProcess diag ["-Tpng", "-o", img, gv] []
            PlantUML -> do
                plantuml <- resource "plantuml.jar" plantumlJar
                readProcess "java" ["-jar", plantuml, "-charset", "UTF-8", gv] []
            Ditaa -> do
                ditaa <- resource "ditaa.jar" ditaaJar
                readProcess "java" ["-jar", ditaa, "-e", "UTF-8", "-o", gv, img] []
    return (env, "!["++title'++"]("++url++")"++attrs)
diagram runtime diag header footer env [path, code] =
    diagram runtime diag header footer env [path, Val "", code]
diagram _ diag _ _ _ _ = arityError diag [2, 3]

-- parseImageAttributes extracts path to generate the scripts and images
-- and path to put in the markdown link.
-- Components put in parents or brackets belongs to the path of images on disk.
-- The function also adds the file extension.
-- An optional text between curly brackets at the end are image attributs that
-- are added after the link in the markdown output.
parseImageAttributes :: Env -> String -> (FilePath, FilePath, FilePath, String, String)
parseImageAttributes env s = ( localPath ++ ".gv"
                             , localPath ++ ".dat"
                             , localPath ++ ".png"
                             , linkPath  ++ ".png"
                             , attrs )
    where
        prefix = fromVal (getSymbol env ImagePath)
        (localPath, linkPath, attrs) = parseLocalAndLink (prefix</>s) "" ""
        parseLocalAndLink [] local link = (strip local, strip link, "")
        parseLocalAndLink ('(':cs) local link =
            let (xs, cs') = extract ')' cs
            in parseLocalAndLink cs' (local++xs) link
        parseLocalAndLink ('[':cs) local link =
            let (xs, cs') = extract ']' cs
            in parseLocalAndLink cs' (local++xs) link
        parseLocalAndLink ('{':cs) local link =
            let (xs, _) = extract '}' cs
            in (strip local, strip link, "{" ++ strip xs ++ "}")
        parseLocalAndLink cs local link =
            let (xs, cs') = span (`notElem` "([{") cs
                xs' = map (\c -> if c == '\\' then '/' else c) xs
            in parseLocalAndLink cs' (local++xs) (link++xs')
        extract right cs =
            let (xs, cs') = span (/=right) cs
            in (xs, dropWhile (==right) cs')

-- PlantUML and ditaa JAR files embedded in pp.
-- The .jar files are converted to C with xxd and seen as a C string in Haskell.

foreign import ccall "&plantuml_jar"     _plantuml_jar      :: Ptr CChar
foreign import ccall "&plantuml_jar_len" _plantuml_jar_len  :: Ptr CInt

plantumlJar :: (Ptr CChar, Ptr CInt)
plantumlJar = (_plantuml_jar, _plantuml_jar_len)

foreign import ccall "&ditaa0_9_jar"     _ditaa_jar         :: Ptr CChar
foreign import ccall "&ditaa0_9_jar_len" _ditaa_jar_len     :: Ptr CInt

ditaaJar :: (Ptr CChar, Ptr CInt)
ditaaJar = (_ditaa_jar, _ditaa_jar_len)

-- "ressource name content" writes content (a C string containing PlantUML or ditaa)
-- to a temporary file. It returns the path of the temporary file so the caller
-- can execute it.
resource :: String -> (Ptr CChar, Ptr CInt) -> IO FilePath
resource name (array, len) = do
    tmp <- getTemporaryDirectory
    let path = tmp </> name
    alreadyExists <- doesFileExist path
    unless alreadyExists $ do
        len' <- peek len
        h <- openBinaryFile path WriteMode
        hPutBuf h array (fromIntegral len')
        hClose h
    return path

-- script executes a script and emits the output of the script.
-- Literate contents are flushed to be usable by the script.
script :: String -> String -> String -> String -> Macro
script _lang cmd header ext env [src] = do
    (env', _) <- flushlit env []
    src' <- pp' env' (fromVal src)
    let (exe:args) = words cmd
    output <- withSystemTempFile ("pp."++ext) $ \path handle -> do
        hWriteFileUTF8 handle $ unlines [header, src']
        hClose handle
        readProcessUTF8 exe (args ++ [path])
    case src of
        Val _ -> return (env', strip output)
        Block _ -> return (env', output)
script lang _ _ _ _ _ = arityError lang [1]

-- macropp executes a macro and preprocess its output.
-- It is used by exec to preprocess the standard output of a shell command.
macropp :: Macro -> Macro
macropp macro env args = do
    (env', src) <- macro env args
    pp env' src

-- language implements the macros \en and \fr.
-- language preprocesses src only if the current language is lang.
language :: String -> Macro
language lang env [src] = case lookup Lang env of
    Just val | fromVal val == lang -> ppAndStrip env src
    _ -> return (env, "")
language lang _ _ = arityError lang [1]

-- format implements the macros \html, \pdf, \odt, \epub and \mobi.
-- format preprocesses src only if the current format is fmt.
format :: String -> Macro
format fmt env [src] = case lookup FileFormat env of
    Just val | fromVal val == fmt -> ppAndStrip env src
    _ -> return (env, "")
format fmt _ _ = arityError fmt [1]

-- charsFunc is the list of characters used to execute a macro
charsFunc :: Chars
charsFunc = ['!', '\\']

-- charsOpenClose is the list of characters that can delimit macro parameters.
charsOpenClose :: [(Char, Char)]
charsOpenClose = [('(', ')'), ('{', '}'), ('[', ']')]

-- charsBlock is the list of characters that can delimit macro parameters with
-- the Markdown code block syntax.
charsBlock :: Chars
charsBlock = ['~', '`']

-- pp' returns the preprocessed string
pp' :: Env -> String -> IO String
pp' env s = fmap snd (pp env s)

-- pp returns the updated environment and the preprocessed string
pp :: Prepro

-- empty string
pp env [] = return (env, "")

-- non empty string
pp env (c0:cs)

    -- if c0 is ! or \, it may be a macro call
    | c0 `elem` charsFunc && not (null name) = case (lookup name builtin, lookup (Def name) env) of
                -- the name is a builtin macro name
                (Just func, _) -> do
                    let (args, cs'') = readArgs Nothing cs'
                    (env', doc) <- func env args
                    (env'', doc') <- pp env' cs''
                    return (env'', doc++doc')
                -- the name is a user macro name
                (Nothing, Just value) -> do
                    let (args, cs'') = readArgs Nothing cs'
                    -- user macro arguments are named 1, 2, ...
                    -- the value of the macro is preprocessed in an environment
                    -- containing the arguments
                    let args' = zip (map (Def . show) [(1::Int)..]) args
                    (env', doc) <- ppAndStrip (args'++env) value
                    -- removes the arguments but keep the side effects of the macro
                    let env'' = env' \\ args'
                    (env''', doc') <- pp env'' cs''
                    return (env''', doc++doc')
                -- unknown macro => keep it unpreprocessed
                (Nothing, Nothing) -> do
                    (env', doc) <- pp env cs'
                    return (env', (c0:name)++doc)

    -- if c0 is ~ or `, it may be the beginning of a regular code block
    -- the end delimiter must not be seen as an argument of a macro that would be at the end of the block
    | c0 `elem` charsBlock = case readArgBlock c0 (c0:cs) of
        Just (start, block, end, s2) -> do
            (env', docBlock) <- pp env block
            (env'', doc) <- pp env' s2
            return (env'', start ++ docBlock ++ end ++ doc)
        Nothing -> do
            (env', doc) <- pp env cs
            return (env', c0:doc)

    -- not a macro
    | otherwise = do
        (env', doc) <- pp env cs
        return (env', c0:doc)
    where
        (name, cs') = span (\c -> isAlphaNum c || c == '_') cs

        -- read a list of arguments
        -- All the arguments have the same delimiters, except the last one that
        -- can be a code block.
        -- The first argument is the argument delimiter (Nothing for the first call)
        -- There can be spaces before the arguments.
        readArgs :: Maybe (Char, Char) -> String -> ([Val], String)

        -- read the first argument
        readArgs Nothing s = case dropSpaces s of
            -- code block => it's the last argument
            Just s1@(c:_) | c `elem` charsBlock -> case readArgBlock c s1 of
                Just (_, block, _, s2) -> ([Block block], s2)
                Nothing -> ([], s)
            -- argument starting with an open delimiter
            Just (left:s1) ->
                case lookup left charsOpenClose of
                    Just right ->
                        -- read one argument
                        -- and the following arguments with the same delimiters
                        let (arg, s2) = readArg left right 1 s1
                            (args, s3) = readArgs (Just (left, right)) s2
                        in (Val (init arg) : args, s3)
                    Nothing ->
                        -- not a delimiter => no more arguments
                        ([], s)
            -- not a delimiter => no more arguments
            _ -> ([], s)

        -- read another argument
        readArgs leftright@(Just (left, right)) s = case dropSpaces s of
            -- code block => it's the last argument
            Just s1@(c:_) | c `elem` charsBlock -> case readArgBlock c s1 of
                Just (_, block, _, s2) -> ([Block block], s2)
                Nothing -> ([], s)
            -- argument starting with the same delimiter
            Just (left':s1) | left' == left ->
                -- read one argument
                -- and the following arguments with the same delimiters
                let (arg, s2) = readArg left right 1 s1
                    (args, s3) = readArgs leftright s2
                in (Val (init arg) : args, s3)
            -- not a delimiter => no more arguments
            _ -> ([], s)

        -- skip spaces before arguments
        dropSpaces :: String -> Maybe String
        dropSpaces s
            -- the number of '\n' is 0 or 1 (no blank line before an argument)
            | nbNl <= 1 = Just s1
            | otherwise = Nothing
            where
                (spaces, s1) = span isSpace s
                nbNl = length (filter (=='\n') spaces)

        -- read one argument, taking care of balancing delimiters
        -- "readArg left right level s" reads one argument delimited by
        -- left and right (ie left and right must be well balanced).
        -- level is incremented when left is found and decremented when
        -- right is found. When level = 0, the end of the argument is found.
        -- The function returns the argument and the rest of the string.
        readArg :: Char -> Char -> Int -> String -> (String, String)

        -- end of the argument
        readArg _ _ 0 s = ("", s)

        -- end of file => delimiters are not well balanced
        readArg _ _ _ [] = unexpectedEndOfFile env name

        -- read one char in the argument
        readArg left right level (c:s) = (c:cs'', s')
            where
                -- increase or decrease level when a delimiter is found
                level'  | c == left     = level + 1
                        | c == right    = level - 1
                        | otherwise     = level
                -- read the rest of the argument
                (cs'', s') = readArg left right level' s

        -- read an argument as a code block
        -- readArgBlock c s reads an argument with lines of c as separators.
        -- If a block with valid start and end separator is found, it returns
        -- a tuple with start, the block, end, the rest of the string.
        readArgBlock :: Char -> String -> Maybe (String, String, String, String)
        readArgBlock c s
            -- block with delimiters longer than 3 c
            | length start >= 3 = Just (start, block, end, s2)
            -- no block
            | otherwise = Nothing
            where
                (start, s1) = span (==c) s
                (block, (end, s2)) = readBlock s1
                readBlock s' | start `isPrefixOf` s' = ([], span (==c) s')
                readBlock (c':s') = (c':cs'', (end', s'')) where (cs'', (end', s'')) = readBlock s'
                readBlock [] = unexpectedEndOfFile env name

-- get a variable in the environment
getSymbol :: Env -> Var -> Val
getSymbol env var = fromMaybe (Val "") (lookup var env)

-- raise an end of file error
unexpectedEndOfFile :: Env -> String -> t
unexpectedEndOfFile env name = error $ "Unexpected end of file in " ++ fromVal (getSymbol env CurrentFile) ++
                                       "\nAn argument of the macro \"" ++ name ++ "\" may not be correctly delimited."

-- raise a file not found error
fileNotFound :: String -> t
fileNotFound name = error $ "File not found: " ++ name

-- raise an arity error
arityError :: String -> [Int] -> t
arityError name arities = error $ "Arity error: " ++ name ++ " expects " ++ nb ++ " argument" ++ s
    where
        (nb, s) = case sort arities of
                    [] -> ("no", "")
                    [0] -> ("no", "")
                    [1] -> ("1", "")
                    [0, 1] -> ("0 or 1", "")
                    [x] -> (show x, "s")
                    xs -> (intercalate ", " (map show (init xs)) ++ " or " ++ show (last xs), "s")
