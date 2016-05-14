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
import System.Environment
import System.Directory
import Data.List
import Data.Char
import Data.Maybe
import System.Process(readProcess)
import System.FilePath
import Data.Time
import Foreign.C.String

#ifdef linux_HOST_OS
import System.Posix.Process
#endif

--import Debug.Trace

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
         deriving (Eq)

-- Preprocessor environment (lookup table)
type Env = [(Var, String)]

-- A macro takes an environment and arguments
-- and returns a new environment and the result of the macro as a string.
type Macro = Env -> [String] -> IO (Env, String)

-- A preprocessor takes an environment and a string to preprocess
-- and returns a new environment and the preprocessed string.
type Prepro = Env -> String ->  IO (Env, String)

-- Diagram types managed by pp
data DiagramRuntime = Graphviz | PlantUML | Ditaa

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
    -- get $FORMAT (html or pdf)
    let fmt = map toLower $ fromMaybe "" (lookup "FORMAT" envVars)
    -- the initial environment contains the language, the format and the environment variables
    let env = (Lang, lang) : (FileFormat, fmt) : [(EnvVar name, val) | (name, val) <- envVars]
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
                    Nothing -> -- nothing has been preprocessed, let's try stdin
                               doArg env "-"
                    Just _ -> -- something has already been preprocessed
                              return (env, "")

-- "doArg env arg" parses one argument
-- and returns an updated environment and the output produced by the argument.
doArg :: Env -> String -> IO (Env, String)

-- "doArg env "-Dname=value"" adds a new definition to the environment.
doArg env ('-':'D':def) = return ((Def name, drop 1 value) : clean (Def name) env, "")
    where (name, value) = span (/= '=') def

-- "doArg env "-Uname"" removes a definition from the environment.
doArg env ('-':'U':name) = return (clean (Def name) env, "")

-- "doArg env "-fr|-en"" changes the current language
doArg env ('-':lang) | lang' `elem` langs =
    return ((Lang, lang') : clean Lang env, "") where lang' = map toLower lang

-- "doArg env "-html|-pdf"" changes the current format
doArg env ('-':fmt) | fmt' `elem` formats =
    return ((FileFormat, fmt') : clean FileFormat env, "") where fmt' = map toLower fmt

-- Other arguments starting with "-" are invalid.
doArg _ ('-':arg) | not (null arg) = error $ "Unexpected argument: " ++ arg

-- "doArg env filename" preprocessed the content of a file using the current environment.
-- The mainFileTag variable is added to the environment.
-- It contains the name of the file being preprocessed.
doArg env name = ppFile ((MainFile, name) : env) name

-- "saveLiterateContent macros env" saves all the literate contents recorded in the environment.
-- "macros" is the list of literate content that must not be saved
-- (they are used as macros to build other literate contents).
saveLiterateContent :: Env -> Env -> IO ()

-- ignore files that have not been modified since the last call to flushLit
saveLiterateContent _ ((LitFlush, _) : _) =
    return ()

-- macros shall not be saved (macro names start with litMacroTagChar)
saveLiterateContent macros ((mac@(LitMacro _), _) : env) =
    saveLiterateContent macros (clean mac env)

-- file content shall be expanded with macro contents
-- previous definitions of the same file are removed from the list to avoid writing old intermediate definitions
saveLiterateContent macros ((file@(LitFile name), content) : env) = do
    writeFileUTF8 name (expandLit macros content)
    saveLiterateContent macros (clean file env)

-- other definitions are not literate things
saveLiterateContent macros (_ : env) =
    saveLiterateContent macros env

saveLiterateContent _ [] =
    return ()

-- "isLitMacro (name, value)" tells if an environment entry is a literate content macro.
-- An entry is a literate content if its name is the name of a literate content macro.
isLitMacro :: (Var, String) -> Bool
isLitMacro = isLitMacroName . fst

-- "isLitMacroName name" tells if a name is a literate content macro name.
-- A literate content name starts with the character litContentTagChar
-- and is not a file name (ie starts with litMacroTagChar).
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
    (env', doc) <- pp ((CurrentFile, name) : clean CurrentFile env) content
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
    h <- openFile name WriteMode
    hSetEncoding h utf8
    hPutStr h content
    hClose h

-- language list
langs :: [String]
langs = ["fr", "en"]

-- format list
formats :: [String]
formats = ["html", "pdf"]

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

-- list of macros
builtin :: [(String, Macro)]
builtin = [ ("def", define)         , ("undef", undefine)
          , ("define", define)      , ("undefine", undefine)
          , ("ifdef", ifdef)        , ("ifndef", ifndef)
          , ("ifeq", ifeq)          , ("ifne", ifne)
          , ("rawdef", rawdef)

          , ("inc", include)        , ("include", include)
          , ("raw", raw)
          , ("rawinc", rawinc)      , ("rawinclude", rawinc)
          , ("exec", exec)          , ("rawexec", rawexec)

          , ("mdate", mdate)

          , ("env", readEnv)

          , ("main", mainFile)
          , ("file", currentFile)
          , ("lang", currentLang)
          , ("format", currentFormat)

          , ("add", add)

          , ("lit", lit)            , ("literate", lit)
          , ("flushlit", flushlit)  , ("flushliterate", flushlit)

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

-- shell command interpretor for Windows scripts
cmdexe :: String
cmdexe =
#ifdef linux_HOST_OS
    "wine cmd /c"
#else
    "cmd /c"
#endif

define :: Macro
define env [name, value] = do
    name' <- pp' env name >>= strip'
    return ((Def name', value) : clean (Def name') env, "")
define env [name] = define env [name, ""]
define _ _ = arityError "define" [1,2]

undefine :: Macro
undefine env [name] = do
    name' <- pp' env name >>= strip'
    return (clean (Def name') env, "")
undefine _ _ = arityError "undefine" [1]

clean :: Var -> Env -> Env
--clean name env = [ item | item@(name', _) <- env, name' /= name]
clean var = filter ((/=var) . fst)

ifdef :: Macro
ifdef env [name, t, e] = do
    name' <- pp' env name >>= strip'
    pp env $ case lookup (Def name') env of
                Just _ -> t
                Nothing -> e
ifdef env [name, t] = ifdef env [name, t, ""]
ifdef _ _ = arityError "ifdef" [1, 2]

ifndef :: Macro
ifndef env [name, t, e] = ifdef env [name, e, t]
ifndef env [name, t] = ifdef env [name, "", t]
ifndef _ _ = arityError "ifndef" [1, 2]

ifeq :: Macro
ifeq env [x, y, t, e] = do
    x' <- pp' env x
    y' <- pp' env y
    pp env (if noSpace x' == noSpace y' then t else e)
        where noSpace = filter (not . isSpace)
ifeq env [x, y, t] = ifeq env [x, y, t, ""]
ifeq _ _ = arityError "ifeq" [3, 4]

ifne :: Macro
ifne env [x, y, t, e] = ifeq env [x, y, e, t]
ifne env [x, y, t] = ifeq env [x, y, "", t]
ifne _ _ = arityError "ifne" [3, 4]

rawdef :: Macro
rawdef env [name] = do
    name' <- pp' env name >>= strip'
    return (env, getSymbol env (Def name'))
rawdef _ _ = arityError "rawdef" [1]

include :: Macro
include env [name] = pp' env name >>= strip' >>= locateFile env >>= ppFile env
include _ _ = arityError "include" [1]

locateFile :: Env -> FilePath -> IO FilePath
locateFile env name = do
    let name' = case name of
                    ('~' : '/' : relname) -> getSymbol env (EnvVar "HOME") </> relname
                    _ -> name
    let path = map (takeDirectory . getSymbol env) [CurrentFile, MainFile] ++ ["."]
    found <- findFile path name'
    case found of
        Just foundFile -> return foundFile
        Nothing -> fileNotFound name

raw :: Macro
raw env [src] = return (env, src)
raw _ _ = arityError "raw" [1]

rawinc :: Macro
rawinc env [name] = do
    doc <- pp' env name >>= strip' >>= locateFile env >>= readFileUTF8
    return (env, doc)
rawinc _ _ = arityError "rawinclude" [1]

rawexec :: Macro
rawexec env [cmd] = do
    cmd' <- pp' env cmd >>= strip'
    doc <- readProcess "sh" ["-c", cmd'] "" >>= strip'
    return (env, doc)
rawexec _ _ = arityError "rawexec" [1]

exec :: Macro
exec env [cmd] = do
    (env', doc) <- rawexec env [cmd]
    pp env' doc
exec _ _ = arityError "exec" [1]

strip :: String -> String
strip = haltStrip . haltStrip where haltStrip = dropWhile isSpace . reverse

strip' :: String -> IO String
strip' = return . strip

mdate :: Macro
mdate env files = do
    files' <- ppAll' env files
    files'' <- mapM (locateFile env) $ concatMap words files' ++ [getSymbol env MainFile | null files]
    times <- mapM getModificationTime files''
    let lastTime = maximum times
    return (env, formatTime (myLocale $ getSymbol env Lang) "%A %-d %B %Y" lastTime)

myLocale :: String -> TimeLocale
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
myLocale _ = defaultTimeLocale

ppAll' :: Env -> [String] -> IO [String]
ppAll' env xs = liftM snd (ppAll env xs)

ppAll :: Env -> [String] -> IO (Env, [String])
ppAll env [] = return (env, [])
ppAll env (x:xs) = do (env', x') <- pp env x
                      (env'', x's) <- ppAll env' xs
                      return (env'', x':x's)

readEnv :: Macro
readEnv env [name] = do
    name' <- pp' env name >>= strip'
    case lookup (EnvVar name') env of
        Just val -> pp env val
        Nothing -> return (env, "")
readEnv _ _ = arityError "env" [1]

mainFile :: Macro
mainFile env [] = return (env, fromMaybe "-" (lookup MainFile env))
mainFile _ _ = arityError "main" [0]

currentFile :: Macro
currentFile env [] = return (env, fromMaybe "-" (lookup CurrentFile env))
currentFile _ _ = arityError "file" [0]

currentLang :: Macro
currentLang env [] = return (env, fromMaybe "" (lookup Lang env))
currentLang _ _ = arityError "lang" [0]

currentFormat :: Macro
currentFormat env [] = return (env, fromMaybe "" (lookup FileFormat env))
currentFormat _ _ = arityError "format" [0]

add :: Macro
add env [name, val] = do
    name' <- pp' env name >>= strip'
    let val0 = getSymbol env (Def name')
    val1 <- pp' env val >>= strip'
    let env' = (Def name', show (atoi val0 + atoi val1)) : clean (Def name') env
    return (env', "")
add env [name] = add env [name, "1"]
add _ _ = arityError "add" [1, 2]

atoi :: String -> Integer
atoi s = case reads s of
            [(i, "")] -> i
            _ -> 0

lit :: Macro
lit env [name, lang, content] = do
    name' <- pp' env name >>= strip'
    let litLangTag = LitLang name'
    let env' = (litLangTag, lang) : clean litLangTag env
    lit env' [name, content]
lit env [name, content] = do
    name' <- pp' env name >>= strip'
    content' <- pp' env content
    let contentTag = case name' of
                        (c:name'') | c == litMacroTagChar -> LitMacro name''
                        _ -> LitFile name'
    let oldContent = fromMaybe "" $ lookup contentTag env
    let env' = (contentTag, oldContent++content') : clean contentTag env
    let lang' = litLang env' name'
    let formatedCode = case lang' of
                        Just lang'' -> unlines [codeBlock ++ " {." ++ lang'' ++ "}", content', codeBlock]
                        Nothing     -> unlines [codeBlock, content', codeBlock]
    return (env', formatedCode)
lit env [name] = do
    name' <- pp' env name >>= strip'
    let contentTag = case name' of
                        (c:name'') | c == litMacroTagChar -> LitMacro name''
                        _ -> LitFile name'
    let content = fromMaybe "" $ lookup contentTag env
    let lang' = litLang env name'
    let formatedCode = case lang' of
                        Just lang'' -> unlines [codeBlock ++ " {." ++ lang'' ++ "}", content, codeBlock]
                        Nothing     -> unlines [codeBlock, content, codeBlock]
    return (env, formatedCode)
lit _ _ = arityError "lit" [1, 2, 3]

litLang :: Env -> String -> Maybe String
litLang env name = case (lookup (LitLang name) env, defaultLitLang name) of
    (Just lang, _) -> Just lang
    (_, Just lang) -> Just lang
    _ -> Nothing

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

codeBlock :: String
codeBlock = replicate 70 '~'

flushlit :: Macro
flushlit env [] = do
    saveLiterateContent (filter isLitMacro env) env
    return ((LitFlush, "") : clean LitFlush env, "")
flushlit _ _ = arityError "flushlit" [0]

expandLit :: Env -> String -> String
expandLit macros (c0:s)
    | c0 == litMacroTagChar = content' ++ expandLit macros s'
    | otherwise = c0 : expandLit macros s
    where
        (name, s') = span (\c -> isAlphaNum c || c == '_') s
        content' = case lookup (LitMacro name) macros of
                    Nothing -> c0:name
                    Just content -> expandLit macros content
expandLit _ [] = []

diagram :: DiagramRuntime -> String -> String -> String -> Macro
diagram runtime diag header footer env [path, title, code] = do
    path' <- pp' env path >>= strip'
    title' <- pp' env title >>= strip'
    code' <- pp' env code
    let code'' = unlines [header, code', footer]
    let (gv, img, url) = splitImgUrl path' True
    oldCodeExists <- doesFileExist gv
    oldCode <- if oldCodeExists then readFileUTF8 gv else return ""
    when (code'' /= oldCode) $ do
        writeFileUTF8 gv code''
        _ <- case runtime of
            Graphviz ->
                readProcess diag ["-Tpng", "-o", img, gv] []
            PlantUML -> do
                plantuml <- resource "plantuml.jar" plantumlJar
                readProcess "java" ["-jar", plantuml, "-charset", "UTF-8", gv] []
            Ditaa -> do
                ditaa <- resource "ditaa.jar" ditaaJar
                readProcess "java" ["-jar", ditaa, "-e", "UTF-8", "-o", gv, img] []
        return ()
    return (env, "!["++title'++"]("++url++")")
diagram runtime diag header footer env [path, code] = diagram runtime diag header footer env [path, "", code]
diagram _ diag _ _ _ _ = arityError diag [2, 3]

splitImgUrl :: String -> Bool -> (String, String, String)
splitImgUrl (c:cs) both
    | c `elem` "(["     = splitImgUrl cs False
    | c `elem` ")]"     = splitImgUrl cs True
    | both              = (c:gv, c:img, c:url)
    | otherwise         = (c:gv, c:img, url)
    where (gv, img, url) = splitImgUrl cs both
splitImgUrl [] _ = (".gv", ".png", ".png")

foreign import ccall "plantuml_jar"     _plantuml_jar       :: CString
foreign import ccall "plantuml_jar_len" _plantuml_jar_len   :: Int

plantumlJar :: CStringLen
plantumlJar = (_plantuml_jar, _plantuml_jar_len)

foreign import ccall "ditaa0_9_jar"     _ditaa_jar          :: CString
foreign import ccall "ditaa0_9_jar_len" _ditaa_jar_len      :: Int

ditaaJar :: CStringLen
ditaaJar = (_ditaa_jar, _ditaa_jar_len)

resource :: String -> CStringLen -> IO FilePath
resource name content = do
    tmp <- getTemporaryDirectory
    let path = tmp </> name
    alreadyExists <- doesFileExist path
    unless alreadyExists $ writeFile path =<< peekCStringLen content
    return path

script :: String -> String -> String -> String -> Macro
script _lang cmd header ext env [src] = do
    tmp <- getTemporaryDirectory
    pid <- getProcessID
    let path = tmp </> "pp" ++ show pid ++ ext
    src' <- pp' env src
    writeFileUTF8 path $ unlines [header, src']
    let (exe:args) = words cmd
    output <- readProcess exe (args ++ [path]) []
    return (env, output)
script lang _ _ _ _ _ = arityError lang [1]

#ifdef linux_HOST_OS
-- getProcessID already defined
#else
foreign import stdcall "GetCurrentProcessId" getProcessID :: IO Int
#endif

language :: String -> Macro
language lang env [src] = case lookup Lang env of
                            Just val | val == lang -> pp env src
                            _ -> return (env, "")
language lang _ _ = arityError lang [1]

format :: String -> Macro
format fmt env [src] = case lookup FileFormat env of
                            Just val | val == fmt -> pp env src
                            _ -> return (env, "")
format fmt _ _ = arityError fmt [1]

charsFunc :: Chars
charsFunc = ['!', '\\']

charsOpenClose :: [(Char, Char)]
charsOpenClose = [('(', ')'), ('{', '}'), ('[', ']')]

charsBlock :: Chars
charsBlock = ['~', '`']

pp' :: Env -> String -> IO String
pp' env s = liftM snd (pp env s)

pp :: Prepro
pp env [] = return (env, "")
pp env (c0:cs)
    | c0 `elem` charsFunc && not (null name) = case (lookup name builtin, lookup (Def name) env) of
                (Just func, _) -> do
                    let (args, cs'') = ppArgs Nothing cs'
                    (env', doc) <- func env args
                    (env'', doc') <- pp env' cs''
                    return (env'', doc++doc')
                (Nothing, Just value) -> do
                    let (args, cs'') = ppArgs Nothing cs'
                    let args' = zip (map (Def . show) [(1::Int)..]) args
                    (env', doc) <- pp (args'++env) value
                    let env'' = env' \\ args'
                    (env''', doc') <- pp env'' cs''
                    return (env''', doc++doc')
                (Nothing, Nothing) -> do
                    (env', doc) <- pp env cs'
                    return (env', (c0:name)++doc)
    | otherwise = do
                    (env', doc) <- pp env cs
                    return (env', c0:doc)
    where
        (name, cs') = span (\c -> isAlphaNum c || c == '_') cs

        ppArgs :: Maybe (Char, Char) -> String -> ([String], String)
        ppArgs Nothing s = case dropSpaces s of -- case dropWhile isSpace s of
            Just s1@(c:_) | c `elem` charsBlock -> ppBlock s c s1
            Just (left:s1) ->
                case lookup left charsOpenClose of
                    Just right -> let (arg, s2) = ppArg left right 1 s1
                                      (args, s3) = ppArgs (Just (left, right)) s2
                                  in (init arg : args, s3)
                    Nothing -> ([], s)
            _ -> ([], s)
        ppArgs leftright@(Just (left, right)) s = case dropSpaces s of -- case dropWhile isSpace s of
            Just s1@(c:_) | c `elem` charsBlock -> ppBlock s c s1
            Just (left':s1) | left' == left ->
                let (arg, s2) = ppArg left right 1 s1
                    (args, s3) = ppArgs leftright s2
                in (init arg : args, s3)
            _ -> ([], s)

        dropSpaces :: String -> Maybe String
        dropSpaces s
            | nbNl <= 1 = Just s1
            | otherwise = Nothing
            where
                (spaces, s1) = span isSpace s
                nbNl = length (filter (=='\n') spaces)

        ppArg :: Char -> Char -> Int -> String -> (String, String)
        ppArg _ _ 0 s = ("", s)
        ppArg _ _ _ [] = unexpectedEndOfFile env
        ppArg left right level (c:s) = (c:s', s'')
            where
                (s', s'') = ppArg left right level' s
                level'  | c == left     = level + 1
                        | c == right    = level - 1
                        | otherwise     = level

        ppBlock :: String -> Char -> String -> ([String], String)
        ppBlock s0 c s
            | startLen >= 3 = ([block], s2)
            | otherwise = ([], s0)
            where
                (start, s1) = span (==c) s
                startLen = length start
                (block, s2) = getBlock s1
                getBlock s' | start `isPrefixOf` s' = ([], drop startLen s')
                getBlock (c':s') = (c':cs'', s'') where (cs'', s'') = getBlock s'
                getBlock [] = unexpectedEndOfFile env

getSymbol :: Env -> Var -> String
getSymbol env var = fromMaybe "" (lookup var env)

unexpectedEndOfFile :: Env -> t
unexpectedEndOfFile env = error $ "Unexpected end of file in " ++ getSymbol env CurrentFile

fileNotFound :: String -> t
fileNotFound name = error $ "File not found: " ++ name

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
