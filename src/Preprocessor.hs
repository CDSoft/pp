{- PP

Copyright (C) 2015, 2016, 2017 Christophe Delord

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

module Preprocessor where

import Debug.Trace

import Control.Monad
import System.IO
import System.IO.Error
import System.IO.Temp
import System.Directory
import Data.List
import Data.List.Split
import Data.Char
import Data.Maybe
import System.FilePath
import Data.Time
import Foreign.C.Types
import Foreign.Ptr
import Foreign hiding (void, new)

import Environment
import OSAbstraction
import UTF8
import ErrorMessages
import Localization
import CSV

type Chars = String

-- charsFunc is the list of characters used to execute a macro
charsFunc :: Chars
charsFunc = ['!', '\\']

validMacroName :: String -> Bool
validMacroName = all (`elem` ('_':['a'..'z']++['A'..'Z']++['0'..'9']))

-- charsOpenClose is the list of characters that can delimit macro parameters.
charsOpenClose :: [(Char, Char)]
charsOpenClose = [('(', ')'), ('{', '}'), ('[', ']')]

-- charsBlock is the list of characters that can delimit macro parameters with
-- the Markdown code block syntax.
charsBlock :: Chars
charsBlock = ['~', '`']

-- format list
formats :: [String]
formats = words "html pdf odf epub mobi"

-- dialect list
dialects :: [String]
dialects = words "md rst"

-- Graphiviz diagrams
graphvizDiagrams :: [String]
graphvizDiagrams = words "dot neato twopi circo fdp sfdp patchwork osage"

-- PlantUML diagrams
plantumlDiagrams :: [String]
plantumlDiagrams = words "uml"

-- Ditaa diagrams
ditaaDiagrams :: [String]
ditaaDiagrams = words "ditaa"

-- literate programming macros
litMacroTagChar :: Char
litMacroTagChar = '@'


-- A preprocessor takes an environment and a string to preprocess
-- and returns a new environment and the preprocessed string.
type Prepro = Env -> String ->  IO (Env, String)

-- A macro takes an environment and arguments
-- and returns a new environment and the result of the macro as a string.
type Macro = Env -> [Val] -> IO (Env, String)

-- Diagram types managed by pp
data DiagramRuntime = Graphviz | PlantUML deriving (Show)

-- list of builtin macros
builtin :: [(String, Macro)]
builtin = [ ("def", define "def")           , ("undef", undefine "undef")
          , ("define", define "define")     , ("undefine", undefine "undefine")
          , ("ifdef", ifdef)                , ("ifndef", ifndef)
          , ("ifeq", ifeq)                  , ("ifne", ifne)
          , ("rawdef", rawdef)

          , ("import", importFile)
          , ("inc", include "inc")          , ("include", include "include")
          , ("raw", raw)
          , ("rawinc", rawinc "rawinc")     , ("rawinclude", rawinc "rawinclude")

          , ("comment", comment)
          , ("quiet", quiet)

#if linux_HOST_OS || darwin_HOST_OS
          , ("exec",    script "exec"    "sh"   ""          ".sh")
          , ("rawexec", deprecated "rawexec" "exec" $ script "rawexec" "sh"   ""          ".sh")             -- deprecated
#endif
#if mingw32_HOST_OS
          , ("exec",    script "exec"    cmdexe "@echo off" ".bat")
          , ("rawexec", deprecated "rawexec" "exec" $ script "rawexec" cmdexe "@echo off" ".bat")             -- deprecated
#endif
          , ("pp", forcepp)

          , ("mdate", mdate)

          , ("env", readEnv)
          , ("os", getos)
          , ("arch", getarch)

          , ("main", mainFile)
          , ("file", currentFile)
          , ("lang", currentLang)           , ("langs", identList "langs" langs)
          , ("format", currentFormat)       , ("formats", identList "formats" formats)
          , ("dialect", currentDialect)     , ("dialects", identList "dialects" dialects)

          , ("add", add)

          , ("lit", lit)            , ("literate", lit)
          , ("flushlit", flushlit)  , ("flushliterate", flushlit)
          , ("src", source)         , ("source", source)
          , ("codeblock", codeblock)
          , ("indent", indent)

          , ("csv", csv)

          ]
          ++ [ (diag, diagram Graphviz diag ""            "")             | diag <- graphvizDiagrams]
          ++ [ (diag, diagram PlantUML diag "@startuml"   "@enduml")      | diag <- plantumlDiagrams]
          ++ [ (diag, diagram PlantUML diag "@startditaa" "@endditaa")    | diag <- ditaaDiagrams]
          ++ [ ("sh",         script "sh"         "sh"          ""          ".sh")
             , ("bash",       script "bash"       "bash"        ""          ".sh")
             , ("cmd",        script "cmd"        cmdexe        "@echo off" ".bat")
             , ("bat",        deprecated "bat" "cmd" $ script "bat"        cmdexe        "@echo off" ".bat")    -- deprecated
             , ("python",     script "python"     "python"      ""          ".py")
             , ("python2",    script "python2"    "python2"     ""          ".py")
             , ("python3",    script "python3"    "python3"     ""          ".py")
             , ("haskell",    script "haskell"    "runhaskell"  ""          ".hs")
#if mingw32_HOST_OS
             , ("powershell", script "powershell" powershellexe ""          ".ps1")
#endif
          ]
          ++ [ (lang, language lang) | lang <- langs]
          ++ [ (fmt, format fmt) | fmt <- formats]
          ++ [ (dial, dialect dial) | dial <- dialects]

-- deprecated prints a warning on stderr when a deprecated macro is executed
deprecated :: String -> String -> Macro -> Macro
deprecated old new macro env args = do
    let file = fromVal (fromMaybe (Val "-") (lookup CurrentFile env))
    hPutStrLn stderr $ "WARNING: " ++ file ++ ": \"" ++ old ++ "\" is deprecated. Please consider using \"" ++ new ++ "\" instead."
    macro env args

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

-- ppAndStrip preprocesses a value
-- and also removes leading and ending spaces unless the value is a block.
-- It returns the new environment and the preprocessed value.
ppAndStrip :: Env -> Val -> IO (Env, String)
ppAndStrip env (Val s) = do
    (env', doc) <- pp env s
    return (env', strip doc)
ppAndStrip env (Block s) =
    pp env s

-- ppAndStrip' works as ppAndStrip but only returns the preprocessed value
ppAndStrip' :: Env -> Val -> IO String
ppAndStrip' env val = fmap snd (ppAndStrip env val)

-- strip removes spaces at the beginning and the end of a string
strip :: String -> String
strip = halfStrip . halfStrip where halfStrip = dropWhile isSpace . reverse

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
                Just (_, '\n':'\r':block, _, s2) -> ([Block block], s2)
                Just (_, '\r':'\n':block, _, s2) -> ([Block block], s2)
                Just (_, '\n':block, _, s2) -> ([Block block], s2)
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
                Just (_, '\n':'\r':block, _, s2) -> ([Block block], s2)
                Just (_, '\r':'\n':block, _, s2) -> ([Block block], s2)
                Just (_, '\n':block, _, s2) -> ([Block block], s2)
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

-- macropp executes a macro and preprocesses its output.
-- It is used by exec to preprocess the standard output of a shell command.
macropp :: Macro -> Macro
macropp macro env args = do
    (env', src) <- macro env args
    pp env' src

---------------------------------------------------------------------
-- Identifier list macro (langs, formats, dialects, ...)
---------------------------------------------------------------------

identList :: String -> [String] -> Macro
identList _ idents env [] = return (env, unwords $ sort idents)
identList name _ _ _ = arityError name [0]

---------------------------------------------------------------------
-- Language macros
---------------------------------------------------------------------

-- \lang returns the current language (see Localization.langs)
currentLang :: Macro
currentLang env [] = return (env, fromVal (getSymbol env Lang))
currentLang _ _ = arityError "lang" [0]

-- language implements the macros \xx where xx is a language code
-- defined in Localization.langs.
-- language preprocesses src only if the current language is lang.
language :: String -> Macro
language lang env [src] = case lookup Lang env of
    Just val | fromVal val == lang -> ppAndStrip env src
    _ -> return (env, "")
language lang _ _ = arityError lang [1]

---------------------------------------------------------------------
-- Format macros
---------------------------------------------------------------------

-- \format returns the current output format (see formats)
currentFormat :: Macro
currentFormat env [] = return (env, fromVal (getSymbol env FileFormat))
currentFormat _ _ = arityError "format" [0]

-- format implements the macros \xxx where xxx is a format
-- defined in formats.
-- format preprocesses src only if the current format is fmt.
format :: String -> Macro
format fmt env [src] = case lookup FileFormat env of
    Just val | fromVal val == fmt -> ppAndStrip env src
    _ -> return (env, "")
format fmt _ _ = arityError fmt [1]

---------------------------------------------------------------------
-- Dialect macros
---------------------------------------------------------------------

-- \dialect returns the current dialect (see dialects)
currentDialect :: Macro
currentDialect env [] = return (env, fromVal (getSymbol env Dialect))
currentDialect _ _ = arityError "dialect" [0]

-- dialect implements the macros \xxx where xxx is a dialect
-- defined in dialects.
-- dialect preprocesses src only if the current dialect is dial.
dialect :: String -> Macro
dialect dial env [src] = case lookup Dialect env of
    Just val | fromVal val == dial -> ppAndStrip env src
    _ -> return (env, "")
dialect dial _ _ = arityError dial [1]

---------------------------------------------------------------------
-- Generic preprocessing macros
---------------------------------------------------------------------

-- \define(name)(value) adds (Def name, value) to the environment.
-- name is preprocessed but not value. The value of the macro is preprocessed
-- when the macro is evaluated (to allow macros with parameters).
define :: String -> Macro
define _ env [name, value] = do
    name' <- ppAndStrip' env name
    when (not (validMacroName name')) $ invalidNameError name'
    when (isJust (lookup name' builtin)) $ builtinRedefinition name'
    return ((Def name', value) : clean (Def name') env, "")
define macro env [name] = define macro env [name, Val ""]
define macro _ _ = arityError macro [1,2]

-- \undefine(name) removes (Def name) from the environment
undefine :: String -> Macro
undefine _ env [name] = do
    name' <- ppAndStrip' env name
    return (clean (Def name') env, "")
undefine macro _ _ = arityError macro [1]

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
include :: String -> Macro
include _ env [name] = ppAndStrip' env name >>= locateFile env >>= ppFile env
include name _ _ = arityError name [1]

-- \import(name) preprocesses name, locates the file, preprocesses its content
-- Only side effect (e.g. macro definitions) are kept in th environment.
-- Nothing is emited.
importFile :: Macro
importFile env [name] = do
    (env', _) <- ppAndStrip' env name >>= locateFile env >>= ppFile env
    return (env', "")
importFile _ _ = arityError "import" [1]

-- "locateFile env name" searches for a file in the directory of the main file
-- or in the directory of the current file or in the current directory
locateFile :: Env -> FilePath -> IO FilePath
locateFile env name = do
    let name' = case name of
            ('~' : '/' : relname) -> fromVal (getSymbol env (EnvVar (envVarStorage "HOME"))) </> relname
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
rawinc :: String -> Macro
rawinc _ env [name] = do
    doc <- ppAndStrip' env name >>= locateFile env >>= readFileUTF8
    return (env, doc)
rawinc name _ _ = arityError name [1]

-- \pp(text) preprocesses text. text can be the output of a script macro containing generated macro calls
forcepp :: Macro
forcepp env [text] = do
    (env', text') <- pp env (fromVal text)
    (env'', text'') <- pp env' text'
    return (env'', text'')
forcepp _ _ = arityError "pp" [1]

-- \comment[(title)](text) ignores title and text (just comments, no preprocessing)
comment :: Macro
comment env [_text] = return (env, "")
comment env [_title, _text] = return (env, "")
comment _ _ = arityError "comment" [1, 2]

-- \comment[(title)](text) ignores title and preprocesses text
-- The output of text is silently discarded, only side effects are kept in the environment.
quiet :: Macro
quiet env [text] = do
    (env', _) <- ppAndStrip env text
    return (env', "")
quiet env [_title, text] = quiet env [text]
quiet _ _ = arityError "quiet" [1, 2]

---------------------------------------------------------------------
-- File macros
---------------------------------------------------------------------

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

-- \main returns the name of the main file (given on the command line)
mainFile :: Macro
mainFile env [] = return (env, fromVal (fromMaybe (Val "-") (lookup MainFile env)))
mainFile _ _ = arityError "main" [0]

-- \file returns the name of the current file (\main or any file included from \main)
currentFile :: Macro
currentFile env [] = return (env, fromVal (fromMaybe (Val "-") (lookup CurrentFile env)))
currentFile _ _ = arityError "file" [0]

---------------------------------------------------------------------
-- OS macros
---------------------------------------------------------------------

-- \env(name) preprocesses name, reads an environment variable (in env)
-- and emits the value of the environment variable.
readEnv :: Macro
readEnv env [name] = do
    name' <- ppAndStrip' env name
    case lookup (EnvVar (envVarStorage name')) env of
        Just val -> return (env, fromVal val)
        Nothing -> return (env, "")
readEnv _ _ = arityError "env" [1]

-- \os emits the OS name
getos :: Macro
getos env [] = do
      return (env, osname)
getos _ _ = arityError "os" [0]

-- \arch emits the architecture of the OS
getarch :: Macro
getarch env [] = do
      return (env, osarch)
getarch _ _ = arityError "arch" [0]

---------------------------------------------------------------------
-- Arithmetic macros
---------------------------------------------------------------------

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

---------------------------------------------------------------------
-- Script macros
---------------------------------------------------------------------

-- try runs an IO action on an executable and its arguments.
-- It returns the output of the IO action or raises an error.
try :: (String -> [String] -> IO String) -> String -> [String] -> IO String
try io exe args = do
    result <- tryIOError (io exe args)
    case result of
        Left e -> error $ "Error while executing `" ++
                          intercalate " " (exe:args) ++
                          "`: " ++ show e
        Right output -> return output

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
        try readProcessUTF8 exe (args ++ [path])
    case src of
        Val _ -> return (env', strip output)
        Block _ -> return (env', output)
script lang _ _ _ _ _ = arityError lang [1]

---------------------------------------------------------------------
-- Diagrams macros
---------------------------------------------------------------------

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
                try readProcessUTF8 diag ["-Tpng", "-o", img, gv]
            PlantUML -> do
                plantuml <- resource "plantuml.jar" plantumlJar
                try readProcessUTF8 "java" ["-jar", plantuml, "-charset", "UTF-8", gv]
    let link = case fromVal $ getSymbol env Dialect of
                "rst" -> unlines [".. figure:: " ++ url, indent' 4 attrs, "", "    " ++ title']
                _ -> "!["++title'++"]("++url++")"++attrs
    return (env, link)
diagram runtime diag header footer env [path, code] =
    diagram runtime diag header footer env [path, Val "", code]
diagram _ diag _ _ _ _ = arityError diag [2, 3]

-- indent' n block adds n space at the beginning of every lines in block
indent' :: Int -> String -> String
indent' n = unlines . map (replicate n ' ' ++) . lines


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
                imgAttrs = case fromVal $ getSymbol env Dialect of
                            "rst" -> unlines $ filter (not . null) $ map strip $ lines xs
                            _ -> "{" ++ strip xs ++ "}"
            in (strip local, strip link, imgAttrs)
        parseLocalAndLink cs local link =
            let (xs, cs') = span (`notElem` "([{") cs
                xs' = map (\c -> if c == '\\' then '/' else c) xs
            in parseLocalAndLink cs' (local++xs) (link++xs')
        extract right cs =
            let (xs, cs') = span (/=right) cs
            in (xs, dropWhile (==right) cs')

-- PlantUML JAR file embedded in pp.
-- The .jar files is converted to C with xxd and seen as a C string in Haskell.

foreign import ccall "&plantuml_jar"     _plantuml_jar      :: Ptr CChar
foreign import ccall "&plantuml_jar_len" _plantuml_jar_len  :: Ptr CInt

plantumlJar :: (Ptr CChar, Ptr CInt)
plantumlJar = (_plantuml_jar, _plantuml_jar_len)

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

---------------------------------------------------------------------
-- Literate programming macros
---------------------------------------------------------------------

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

-- literate programming macro
lit :: Macro

-- \lit(name)(lang)(content) appends content to the file or macro name.
-- In the markdown output, the content will be colored according to the language lang.
lit env [name, lang, content] = do
    name' <- ppAndStrip' env name
    lang' <- ppAndStrip' env lang
    content' <- ppAndStrip' env content
    let env' = litAppend env name' (Just lang') content'
    let formatedCode = litShow env (Just lang') content'
    return (env', formatedCode)

-- \lit(name)(lang)(content) appends content to the file or macro name.
-- The current language is the previously defined language or the default
-- one according to name.
lit env [name, content] = do
    name' <- ppAndStrip' env name
    content' <- ppAndStrip' env content
    let lang = litLang env name'
    let env' = litAppend env name' lang content'
    let formatedCode = litShow env lang content'
    return (env', formatedCode)

-- \lit(name) emits the current content of a literate file or macro.
lit env [name] = do
    name' <- ppAndStrip' env name
    let lang = litLang env name'
    let content = litGet env name'
    let formatedCode = litShow env lang content
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
    let formatedCode = litShow env (Just lang') content
    return (env', formatedCode)

-- \src(name) reads a source file.
-- The language is the default one according to name.
source env [name] = do
    (env', _) <- flushlit env []
    name' <- ppAndStrip' env' name
    let lang = litLang env' name'
    content <- readFileUTF8 name'
    let formatedCode = litShow env lang content
    return (env', formatedCode)

source _ _ = arityError "src" [1, 2]

codeblock :: Macro

-- "\codeblock(len)(ch)" stores the new codeblock separator (ch repeated len times)
codeblock env [len, ch] = do
    len' <- (fromIntegral . atoi) <$> ppAndStrip' env len
    when (len' < 3) $ codeblockError
    s' <- ppAndStrip' env ch
    let line = case s' of
                [c] | c `elem` "~`" -> replicate len' c
                _ -> codeblockError
    return ((CodeBlock, Val line) : clean CodeBlock env, "")

-- "\codeblock(len" stores the new codeblock separator ('~' repeated len times)
codeblock env [len] = codeblock env [len, Val "~"]

codeblock _ _ = arityError "codeblock" [1, 2]

indent :: Macro

-- "\indent(n)(block)" indents block by n spaces (n is optional)
indent env [n, block] = do
    n' <- (fromIntegral . atoi) <$> ppAndStrip' env n
    when (n' < 3) $ indentError
    (env', block') <- pp env (fromVal block)
    return (env', indent' n' block')

-- "\indent(block)" indents block by n spaces (n is optional)
indent env [block] = indent env [Val "4", block]

indent _ _ = arityError "indent" [1, 2]

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
litShow :: Env -> Maybe String -> String -> String
litShow env lang content = formatedBlock
    where
        formatedBlock = case fromVal $ getSymbol env Dialect of
            "rst"   -> reStructuredTextBlock
            _       -> markdownBlock

        markdownBlock = case lang of
            Just lang'  -> unlines [codeBlock ++ " {." ++ lang' ++ "}", content, codeBlock]
            Nothing     -> unlines [codeBlock, content, codeBlock]

        reStructuredTextBlock = case lang of
            Just lang'  -> unlines [ ".. code-block:: " ++ lang', "", indent' 4 content, ""]
            Nothing     -> unlines [ ".. code-block:: none", "", indent' 4 content, ""]

        codeBlock = case lookup CodeBlock env of
                        Just (Val line) -> line
                        Just (Block line) -> line
                        Nothing -> replicate 70 '~'

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

---------------------------------------------------------------------
-- CSV tables
---------------------------------------------------------------------

csv :: Macro

-- \csv(filename) converts a CSV file to a markdown or reStructuredText table
-- The delimiter is automagically infered (hope it works...).
-- The first line of the CSV file is the header of the table.
csv env [filename] = do
    filename' <- ppAndStrip' env filename
    csvData <- readFileUTF8 filename'
    let table = makeTable (fromVal $ getSymbol env Dialect) Nothing csvData
    return (env, table)
--
-- \csv(filename)(header) converts a CSV file to a markdown or reStructuredText table
-- The delimiter is automagically infered (hope it works...).
-- The first line of the CSV file is the header of the table.
csv env [filename, header] = do
    filename' <- ppAndStrip' env filename
    header' <- ppAndStrip' env header
    let fields = splitOn "|" header'
    csvData <- readFileUTF8 filename'
    let table = makeTable (fromVal $ getSymbol env Dialect) (Just fields) csvData
    return (env, table)
csv _ _ = arityError "csv" [1, 2]
