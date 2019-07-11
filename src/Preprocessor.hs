{- PP

Copyright (C) 2015-2019 Christophe Delord

https://www.cdsoft.fr/pp

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

module Preprocessor ( ppFile
                    , pp
                    , Dialect
                    , Format
                    , saveLiterateContent
                    , macrochars
                    , macroargs
                    , macroblockargs
                    , literatemacrochars
                    , checkParserConsistency
                    , isValidMacroName
                    , isValidMacroNameChar
                    , longHelp
                    , longUserHelp
                    , builtin
                    )
where

import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Time
import Data.Text.Encoding
import Foreign hiding (void, new)
import Foreign.C.Types
import System.Directory
import System.FilePath
import System.PosixCompat.Files
import System.IO
import System.IO.Error
import System.IO.Temp

import qualified Data.Text as T
import Text.Mustache (compileTemplate, substitute, toMustache)
import Data.Aeson (Value, eitherDecode)
import Data.Yaml (decodeEither')
--import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BL8

import CSV
import Environment
import ErrorMessages
import Expr
import Formats
import Localization
import OSAbstraction
import PlantumlJar
import UTF8
import qualified Version

-- Validate a char in a macro name (letter, digit or underscore)
isValidMacroNameChar :: Char -> Bool
isValidMacroNameChar c = isAlphaNum c || c == '_'

-- Validate a macro name
isValidMacroName :: String -> Bool
isValidMacroName = all isValidMacroNameChar

-- A preprocessor takes an environment and a string to preprocess
-- and returns a new environment and the preprocessed string.
type Prepro = Env -> String ->  IO (Env, String)

-- Diagram types managed by pp
data DiagramRuntime = Graphviz | PlantUML | BlockDiag | Asymptote | R deriving (Show)

-- list of builtin macros
builtin :: [Macro]
builtin = [ define, undefine, defined, rawdef
          , ifdef, ifndef, ifeq, ifne, if_, eval_
          , importFile, include
          , raw, rawinc
          , comment, quiet, forcepp
          , mustache
          , mdate
          , getMainFile, getCurrentFile
          , getRootDir, getCurrentDir
          , getCurrentLang, identList "langs" (map showCap langs) "lists the known languages"
          ]
          ++ map language langs
          ++
          [ getCurrentFormat, identList "formats" (map showCap formats) "lists the known formats"
          ]
          ++ map format formats
          ++
          [ getCurrentDialect, identList "dialects" (map showCap dialects) "lists the kown output dialects"
          ]
          ++ map dialect dialects
          ++
          [ readEnv, getos, getarch
          , add, append
          , exec, rawexec `deprecated` "exec"
          , script "sh"         "sh -c"       ""          ".sh" "`!sh(CMD)` executes `CMD` in a `sh` shell."
          , script "bash"       "bash"        ""          ".sh" "`!bash(CMD)` executes `CMD` in a `bash` shell."
          , script "zsh"        "zsh"         ""          ".sh" "`!zsh(CMD)` executes `CMD` in a `zsh` shell."
          , script "fish"       "fish"        ""          ".sh" "`!fish(CMD)` executes `CMD` in a `fish` shell."
          , script "cmd"        cmdexe        "@echo off" ".bat" "`!cmd(CMD)` executes `CMD` in a Windows shell (cmd.exe)."
          , script "bat"        cmdexe        "@echo off" ".bat" "" `deprecated` "cmd"
          , script "python"     "python"      ""          ".py" "`!python(CMD)` executes `CMD` with the default Python interpretor."
          , script "python2"    "python2"     ""          ".py" "`!python2(CMD)` executes `CMD` with Python 2."
          , script "python3"    "python3"     ""          ".py" "`!python3(CMD)` executes `CMD` with Python 3."
          , script "lua"        "lua"         ""          ".lua" "`!lua(CMD)` executes `CMD` with Lua."
          , script "haskell"    "runhaskell"  ""          ".hs" "`!haskell(CMD)` executes `CMD` as a Haskell script with `runhaskell`."
          , script "stack"      "stack"       ""          ".hs" "`!stack(CMD)` executes `CMD` as a Haskell script with `stack`."
          , script "Rscript"    "Rscript"     ""          ".R"  "`!Rscript(CMD)` executes `CMD` as a R script with Rscript."
          , windowsonly $ script "powershell" powershellexe "" ".ps1" "`!cmd(CMD)` executes `CMD` in a Windows shell (Powershell)."
          ]
          ++ [diagram name Graphviz exe (const "") (const "") | (name, exe) <- graphvizExe <$> graphvizDiagrams]
          ++ [diagram name PlantUML exe (const $ "@start"++exe) (const $ "@end"++exe) | (name, exe) <- plantumlExe <$> plantumlDiagrams]
          ++ [diagram name BlockDiag exe (const $ exe++" {") (const "}") | (name, exe) <- blockdiagExe <$> blockDiagrams]
          ++ [diagram name Asymptote exe (const "import settings; libgs=\"\";") (const "") | (name, exe) <- asymptoteExe <$> asymptoteDiagrams]
          ++ [diagram name R exe (\img -> drop 1 (takeExtension img)++"('"++img++"');") (const "") | (name, exe) <- rExe <$> rDiagrams]
          ++
          [ lit, flushlit
          , source
          , codeblock
          , indent
          , csv
          , macrochars, macroargs, macroblockargs, literatemacrochars
          , builtinmacros, usermacros
          , help, userhelp
          ]

graphvizExe :: GraphvizDiagram -> (String, String)
graphvizExe d = (showCap d, showCap d)

plantumlExe :: PlantumlDiagram -> (String, String)
plantumlExe d = (showCap d, showCap d)

blockdiagExe :: BlockDiagram -> (String, String)
blockdiagExe d = (showCap d, showCap d)

asymptoteExe :: AsymptoteDiagram -> (String, String)
asymptoteExe d = (showCap d, showCap d)

rExe :: RDiagram -> (String, String)
rExe Rplot = ("Rplot", "Rscript")

-- deprecated prints a warning on stderr when a deprecated macro is executed
deprecated :: Macro -> String -> Macro
deprecated (Macro name aliases _ impl) new =
    Macro name aliases docstring impl'
    where
        impl' env args = do
            let file = fromMaybe "-" $ currentFile env
            hPutStrLn stderr $ "WARNING: " ++ file ++ ": \"" ++ name ++ "\" is deprecated. Please consider using \"" ++ new ++ "\" instead."
            impl env args
        docstring = "`!"++name++"` is *deprecated*. See "++new++"."

windowsonly :: Macro -> Macro
#if mingw32_HOST_OS
windowsonly = id
#endif
#if linux_HOST_OS || darwin_HOST_OS
windowsonly (Macro name aliases doc _) = Macro name aliases doc
    (\_ _ -> windowsOnlyError name)
#endif

-- "ppFile env name" preprocess a file using the current environment
-- env returns an updated environment and the preprocessed output.
-- The environment contains the name of the file in currentFileTag.
ppFile :: Env -> FilePath -> IO (Env, String)
ppFile env name = do
    -- read the file to preprocess
    content <- readFileUTF8 name
    -- preprocess the file in an environment containing the filename
    let env' = case name of
                "-" -> env
                _ -> addDep env name
    (env'', doc) <- pp env'{currentFile=Just name} content
    -- return the environment (with the file name of the caller) and the preprocessed output
    return (env''{currentFile=currentFile env}, doc)

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

    -- if c0 is ! and cs starts with a digit, it is a macro parameter
    | c0 `elem` charsFunc && not (null number) = case lookup number (arguments env) of
        -- the parameter is defined
        Just value -> do
            (env', doc) <- ppAndStrip env value
            (env'', doc') <- pp env' csAfterNumber
            return (env'', doc++doc')
        -- the parameter is not defined (empty string)
        Nothing -> do
            (env', doc) <- pp env csAfterNumber
            return (env', doc)

    -- if c0 is ! and cs starts with a letter, it may be a macro call
    | c0 `elem` charsFunc && not (null name) = case (lookupMacro name builtin, lookup (Def name) (vars env)) of
        -- the name is a builtin macro name
        (Just (Macro _ _ _ func), _) -> do
            let (fargs, cs') = readArgs env name Nothing csAfterName
            (env', doc) <- func env fargs
            let nl | hasBlocks fargs && not (null doc) && not (hasTerminalNewline doc) = osnl
                   | otherwise = ""
            (env'', doc') <- pp env' cs'
            return (env'', doc++nl++doc')
        -- the name is a user macro name
        (Nothing, Just value) -> do
            let (margs, cs') = readArgs env name Nothing csAfterName
            -- args that contain !i should be replaced by their definition in env to avoid infinite recursion
            let margs' = replaceUserMacroArgs env margs
            (env', doc) <- ppAndStrip env{arguments=zip (map show [1::Int ..]) margs'} value
            let nl | hasBlocks margs && not (null doc) && not (hasTerminalNewline doc) = osnl
                   | otherwise = ""
            (env'', doc') <- pp env'{arguments=arguments env} cs'
            return (env'', doc++nl++doc')
        -- unknown macro => keep it unpreprocessed
        (Nothing, Nothing) -> do
            (env', doc) <- pp env csAfterName
            return (env', c0:name++doc)

    -- if c0 is ~ or `, it may be the beginning of a regular code block
    -- the end delimiter must not be seen as an argument of a macro that would be at the end of the block
    | c0 `elem` charsBlock = case readArgBlock env name c0 (c0:cs) of
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
        (number, csAfterNumber) = span isDigit cs
        (name, csAfterName) = span isValidMacroNameChar cs
        charsFunc = macroChars env
        charsBlock = blockChars env

-- read a list of arguments
-- All the arguments have the same delimiters, except the last one that
-- can be a code block.
-- The first argument is the argument delimiter (Nothing for the first call)
-- There can be spaces before the arguments.
readArgs :: Env -> String -> Maybe (Char, Char) -> String -> ([Val], String)

-- read the first argument
readArgs env name Nothing s = case dropSpaces s of
    -- code block => last arguments
    Just s1@(c:_) | c `elem` charsBlock -> case readArgBlocks env name c s1 of
        ([], _) -> ([], s) -- keep initial spaces
        args -> args
    -- argument starting with an open delimiter
    Just (left:s1) ->
        case lookup left charsOpenClose of
            Just right ->
                -- read one argument
                -- and the following arguments with the same delimiters
                let (arg, s2) = readArg env name left right 1 s1
                    (args, s3) = readArgs env name (Just (left, right)) s2
                in (Val (init arg) : args, s3)
            Nothing ->
                -- not a delimiter => no more arguments
                ([], s)
    -- not a delimiter => no more arguments
    _ -> ([], s)

    where
        charsBlock = blockChars env
        charsOpenClose = openCloseChars env

-- read another argument
readArgs env name leftright@(Just (left, right)) s = case dropSpaces s of
    -- code block => last arguments
    Just s1@(c:_) | c `elem` charsBlock -> case readArgBlocks env name c s1 of
        ([], _) -> ([], s) -- keep initial spaces
        args -> args
    -- argument starting with the same delimiter
    Just (left':s1) | left' == left ->
        -- read one argument
        -- and the following arguments with the same delimiters
        let (arg, s2) = readArg env name left right 1 s1
            (args, s3) = readArgs env name leftright s2
        in (Val (init arg) : args, s3)
    -- not a delimiter => no more arguments
    _ -> ([], s)

    where
        charsBlock = blockChars env

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
readArg :: Env -> String -> Char -> Char -> Int -> String -> (String, String)

-- end of the argument
readArg _ _ _ _ 0 s = ("", s)

-- end of file => delimiters are not well balanced
readArg env name _ _ _ [] = unexpectedEndOfFile env name

-- read one char in the argument
readArg env name left right level (c:s) = (c:cs'', s')
    where
        -- increase or decrease level when a delimiter is found
        level'  | c == left     = level + 1
                | c == right    = level - 1
                | otherwise     = level
        -- read the rest of the argument
        (cs'', s') = readArg env name left right level' s

-- read zero or many arguments as code blocks
-- readArgBlocks c s reads arguments with lines of c as separators.
-- If blocks with valid start and end separator is found, it returns
-- a list of blocks and the rest of the string.
readArgBlocks :: Env -> String -> Char -> String -> ([Val], String)
readArgBlocks env name c s =
    let maybeArg = case readArgBlock env name c s of
            Just (_, '\n':'\r':block, _, s2) -> (Just (Block block), skipEOL s2)
            Just (_, '\r':'\n':block, _, s2) -> (Just (Block block), skipEOL s2)
            Just (_, '\n':block, _, s2) -> (Just (Block block), skipEOL s2)
            Just (_, block, _, s2) -> (Just (Block block), skipEOL s2)
            Nothing -> (Nothing, s)
    in case maybeArg of
        (Nothing, _) -> ([], s)
        (Just arg, s3) -> let (args, s4) = readArgBlocks env name c s3
                          in (arg:args, s4)

hasBlocks :: [Val] -> Bool
hasBlocks (Block _:_) = True
hasBlocks (_:args) = hasBlocks args
hasBlocks [] = False

hasTerminalNewline :: String -> Bool
hasTerminalNewline s = '\n' `elem` takeWhile isSpace (reverse s)

-- read an argument as a code block
-- readArgBlock c s reads an argument with lines of c as separators.
-- If a block with valid start and end separator is found, it returns
-- a tuple with start, the block, end, the rest of the string.
readArgBlock :: Env -> String -> Char -> String -> Maybe (String, String, String, String)
readArgBlock env name c s
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

-- skip spaces until the end of the current line and remove the new line character
skipEOL :: String -> String
skipEOL ('\n':'\r':s) = s
skipEOL ('\r':'\n':s) = s
skipEOL ('\n':s) = s
skipEOL (c:s) | isSpace c = skipEOL s
skipEOL s = s

-- replace user macro arguments by their definition without any side effect
-- see issue#29 on github
-- Thanks to bjp (https://github.com/bpj) for reporting this bug.
replaceUserMacroArgs :: Env -> [Val] -> [Val]
replaceUserMacroArgs _ [] = []
replaceUserMacroArgs env (val:args) = val'' : replaceUserMacroArgs env args
    where
        val' = replaceUserArgs env (fromVal val)
        val'' = case val of
                    Val _ -> Val val'
                    Block _ -> Block val'

replaceUserArgs :: Env -> String -> String
replaceUserArgs _ "" = ""
replaceUserArgs env (c:cs)
    | c `elem` charsFunc && not (null i) = case lookup i (arguments env) of
        Just val -> fromVal val ++ replaceUserArgs env cs'
        Nothing -> replaceUserArgs env cs'
    | otherwise = c : replaceUserArgs env cs
    where
        (i, cs') = span isDigit cs
        charsFunc = macroChars env

---------------------------------------------------------------------
-- Identifier list macro (langs, formats, dialects, ...)
---------------------------------------------------------------------

identList :: String -> [String] -> String -> Macro
identList name idents doc = Macro name []
    ("`!" ++ name ++ "` " ++ doc ++ " (" ++ intercalate ", " idents ++ ").")
    (\env [] -> return (env, unwords $ sort idents))

---------------------------------------------------------------------
-- Language macros
---------------------------------------------------------------------

-- !lang returns the current language (see Localization.langs)
getCurrentLang :: Macro
getCurrentLang = Macro "lang" []
    "`!lang` returns the current language."
    (\env args -> case args of
        [] -> return (env, showCap (currentLang env))
        _ -> arityError "lang"
    )

-- language implements the macros !xx where xx is a language code
-- defined in Localization.langs.
-- language preprocesses src only if the current language is lang.
language :: Lang -> Macro
language lang = Macro lang' []
    ("`!" ++ lang' ++ "(TEXT)` returns `TEXT` if the current language is `" ++ lang' ++ "`.")
    (\env args -> case args of
        [src] -> case currentLang env of
            val | val == lang -> ppAndStrip env src
            _ -> return (env, "")
        _ -> arityError lang'
    )
    where lang' = showCap lang

---------------------------------------------------------------------
-- Format macros
---------------------------------------------------------------------

-- !format returns the current output format (see formats)
getCurrentFormat :: Macro
getCurrentFormat = Macro "format" []
    "`!format` returns the current output format."
    (\env args -> case args of
        [] -> return (env, showCapMaybe "" (fileFormat env))
        _ -> arityError "format"
    )

-- format implements the macros !xxx where xxx is a format
-- defined in formats.
-- format preprocesses src only if the current format is fmt.
format :: Format -> Macro
format fmt = Macro fmt' []
    ("`!" ++ fmt' ++ "(TEXT)` returns `TEXT` if the current format is `" ++ fmt' ++ "`.")
    (\env args -> case args of
        [src] -> case fileFormat env of
            Just val | val == fmt -> ppAndStrip env src
            _ -> return (env, "")
        _ -> arityError fmt'
    )
    where fmt' = showCap fmt

---------------------------------------------------------------------
-- Dialect macros
---------------------------------------------------------------------

-- !dialect returns the current dialect (see dialects)
getCurrentDialect :: Macro
getCurrentDialect = Macro "dialect" []
    "`!dialect` returns the current output dialect."
    (\env args -> case args of
        [] -> return (env, showCap (currentDialect env))
        _ -> arityError "dialect"
    )

-- dialect implements the macros !xxx where xxx is a dialect
-- defined in dialects.
-- dialect preprocesses src only if the current dialect is dial.
dialect :: Dialect -> Macro
dialect dial = Macro dial' []
    ("`!" ++ dial' ++ "(TEXT)` returns `TEXT` if the current dialect is `" ++ dial' ++ "`.")
    (\env args -> case args of
        [src] -> case currentDialect env of
            val | val == dial -> ppAndStrip env src
            _ -> return (env, "")
        _ -> arityError dial'
    )
    where dial' = showCap dial

---------------------------------------------------------------------
-- Generic preprocessing macros
---------------------------------------------------------------------

-- !define(name)(value) adds (Def name, value) to the environment.
-- name is preprocessed but not value. The value of the macro is preprocessed
-- when the macro is evaluated (to allow macros with parameters).
define :: Macro
define = Macro "define" ["def"]
    "`!def[ine](SYMBOL)[[(DOC)](VALUE)]` adds the symbol `SYMBOL` to the current environment and associate it with the optional value `VALUE`. Arguments are denoted by `!1` ... `!n` in `VALUE`. If `DOC` is given it is used to document the macro (see the `-help` option)."
    impl
    where
        impl env [name, doc, value] = do
            name' <- ppAndStrip' env name
            unless (isValidMacroName name') $ invalidNameError name'
            doc' <- ppAndStrip' env doc
            when (isJust (lookupMacro name' builtin)) $ builtinRedefinition name'
            return (env{ vars = (Def name', value) : clean (Def name') (vars env)
                       , docstrings = (Def name', doc') : docstrings env
                       }, "")
        impl env [name, value] = do
            name' <- ppAndStrip' env name
            unless (isValidMacroName name') $ invalidNameError name'
            when (isJust (lookupMacro name' builtin)) $ builtinRedefinition name'
            return (env{vars=(Def name', value) : clean (Def name') (vars env)}, "")
        impl env [name] = impl env [name, Val ""]
        impl _ _ = arityError "define"

-- !undefine(name) removes (Def name) from the environment
undefine :: Macro
undefine = Macro "undefine" ["undef"]
    "`!undef[ine](SYMBOL)` removes the symbol `SYMBOL` from the current environment."
    (\env args -> case args of
        [name] -> do
            name' <- ppAndStrip' env name
            return (env{vars = clean (Def name') (vars env), arguments = clean name' (arguments env)}, "")
        _ -> arityError "undefine"
    )

-- !ifdef(name)(t)(e) preprocesses name. If the result is the name of an
-- already defined symbol in the environment, it preprocessed t, otherwise e.
-- e is optional.
ifdef :: Macro
ifdef = Macro "ifdef" []
    "`!ifdef(SYMBOL)(TEXT_IF_DEFINED)[(TEXT_IF_NOT_DEFINED)]` returns `TEXT_IF_DEFINED` if `SYMBOL` is defined or `TEXT_IF_NOT_DEFINED` if it is not defined."
    impl
    where
        impl env [name, t, e] = do
            name' <- ppAndStrip' env name
            ppAndStrip env $ case (lookup name' (arguments env), lookup (Def name') (vars env)) of
                        (Nothing, Nothing) -> e
                        _ -> t
        impl env [name, t] = impl env [name, t, Val ""]
        impl _ _ = arityError "ifdef"

-- !ifndef(name)(t)(e) is equivalent to !ifdef(name)(e)(t)
ifndef :: Macro
ifndef = Macro "ifndef" []
    "`!ifndef(SYMBOL)(TEXT_IF_NOT_DEFINED)[(TEXT_IF_DEFINED)]` returns `TEXT_IF_NOT_DEFINED` if `SYMBOL` is not defined or `TEXT_IF_DEFINED` if it is defined."
    impl
    where
        Macro _ _ _ ifdefImpl = ifdef
        impl env [name, t, e] = ifdefImpl env [name, e, t]
        impl env [name, t] = ifdefImpl env [name, Val "", t]
        impl _ _ = arityError "ifndef"

defined :: Macro
defined = Macro "defined" []
    "`!defined(SYMBOL)` returns 1 if `SYMBOL` is defined, 0 otherwise."
    impl
    where
        impl env [name] = do
            name' <- ppAndStrip' env name
            case (lookup name' (arguments env), lookup (Def name') (vars env)) of
                (Nothing, Nothing) -> return (env, "0")
                _ -> return (env, "1")
        impl _ _ = arityError "defined"

-- !ifeq(x)(y)(t)(e) preprocesses x and y. If they are equal
-- (spaces are ignored), it preprocessed t, otherwise e.
-- e is optional.
ifeq :: Macro
ifeq = Macro "ifeq" []
    "`!ifeq(X)(Y)(TEXT_IF_EQUAL)[(TEXT_IF_DIFFERENT)]` returns `TEXT_IF_EQUAL` if `X` and `Y` are equal or `TEXT_IF_DIFFERENT` if `X` and `Y` are different. Two pieces of text are equal if all non-space characters are the same."
    impl
    where
        impl env [x, y, t, e] = do
            x' <- ppAndStrip' env x
            y' <- ppAndStrip' env y
            ppAndStrip env (if noSpace x' == noSpace y' then t else e)
                where noSpace = filter (not . isSpace)
        impl env [x, y, t] = impl env [x, y, t, Val ""]
        impl _ _ = arityError "ifeq"

-- !ifne(x)(y)(t)(e) is equivalent to !ifeq(x)(y)(e)(t)
ifne :: Macro
ifne = Macro "ifne" []
    "`!ifne(X)(Y)(TEXT_IF_DIFFERENT)[(TEXT_IF_EQUAL)]` returns `TEXT_IF_DIFFERENT` if `X` and `Y` are different or `TEXT_IF_EQUAL` if `X` and `Y` are equal."
    impl
    where
        Macro _ _ _ ifeqImpl = ifeq
        impl env [x, y, t, e] = ifeqImpl env [x, y, e, t]
        impl env [x, y, t] = ifeqImpl env [x, y, Val "", t]
        impl _ _ = arityError "ifne"

if_ :: Macro
if_ = Macro "if" []
    "`!if(EXPR)(TEXT_IF_EXPR_IS_TRUE)[(TEXT_IF_EXPR_IS_FALSE)]` returns `TEXT_IF_EXPR_IS_TRUE` if `EXPR` is true or `TEXT_IF_EXPR_IS_FALSE` if `EXPR` is false."
    impl
    where
        impl env [expr, t, e] = do
            expr' <- ppAndStrip' env expr
            ppAndStrip env $ case eval (fromMaybe "-" $ currentFile env) expr' of
                (_, True) -> t
                (_, False) -> e
        impl env [expr, t] = impl env [expr, t, Val ""]
        impl _ _ = arityError "if"

eval_ :: Macro
eval_ = Macro "eval" []
    "`!eval(EXPR) evaluates `EXPR`."
    impl
    where
        impl env [expr] = do
            expr' <- ppAndStrip' env expr
            case eval (fromMaybe "-" $ currentFile env) expr' of
                (val, _) -> return (env, val)
        impl _ _ = arityError "eval"

-- !rawdef(name) preprocesses name and emits the raw definition
-- (no preprocessing)
rawdef :: Macro
rawdef = Macro "rawdef" []
    "`!rawdef(X)` returns the raw (unevaluated) definition of `X`."
    (\env args -> case args of
        [name] -> do
            name' <- ppAndStrip' env name
            return (env, fromVal (getSymbol env (Def name')))
        _ -> arityError "rawdef"
    )

-- !include(name) preprocesses name, locates the file and preprocesses its content
include :: Macro
include = Macro "include" ["inc"]
    "`!inc[lude](FILENAME)` preprocesses and returns the content of the file named `FILENAME` and includes it in the current document. If the file path is relative it is searched first in the directory of the current file then in the directory of the main file."
    (\env args -> case args of
        [name] -> ppAndStrip' env name >>= locateFile env >>= ppFile env
        _ -> arityError "include"
    )

-- !import(name) preprocesses name, locates the file, preprocesses its content
-- Only side effect (e.g. macro definitions) are kept in th environment.
-- Nothing is emited.
importFile :: Macro
importFile = Macro "import" []
    "`!import(FILENAME)` works as `!include(FILENAME)` but returns nothing. This is useful to import macro definitions."
    (\env args -> case args of
        [name] -> do
            (env', _) <- ppAndStrip' env name >>= locateFile env >>= ppFile env
            return (env', "")
        _ -> arityError "import"
    )

-- "locateFile env name" searches for a file in the directory of the main file
-- or in the directory of the current file or in the current directory
locateFile :: Env -> FilePath -> IO FilePath
locateFile env name = do
    let name' = case name of
            ('~' : '/' : relname) -> fromVal (getSymbol env (EnvVar (envVarStorage "HOME"))) </> relname
            _ -> name
    let path = searchSpace env
    found <- findFile path name'
    case found of
        Just foundFile -> return foundFile
        Nothing -> fileNotFound name

searchSpace :: Env -> [FilePath]
searchSpace env = [ takeDirectory (fromMaybe "-" (f env)) | f <- [currentFile, mainFile] ] ++ [ "." ]

-- !raw(text) emits text unpreprocessed
raw :: Macro
raw = Macro "raw" []
    "`!raw(TEXT)` returns `TEXT` without any preprocessing."
    (\env args -> case args of
        [src] -> return (env, fromVal src)
        _ -> arityError "raw"
    )

-- !rawinclude(name) preprocesses name, locates the file and emits it unpreprocessed
rawinc :: Macro
rawinc = Macro "rawinclude" ["rawinc"]
    "`!rawinc[lude](FILE)` returns the content of `FILE` without any preprocessing."
    (\env args -> case args of
        [name] -> do
            name' <- ppAndStrip' env name >>= locateFile env
            let env' = addDep env name'
            doc <- readFileUTF8 name'
            return (env', doc)
        _ -> arityError "rawinclude"
    )

-- !pp(text) preprocesses text. text can be the output of a script macro containing generated macro calls
forcepp :: Macro
forcepp = Macro "pp" []
    "`!pp(TEXT)` preprocesses and return `TEXT`. This macro is useful to preprocess the output of script macros for instance (`!sh`, `!python`, ...)."
    (\env args -> case args of
        [text] -> do
            (env', text') <- pp env (fromVal text)
            (env'', text'') <- pp env' text'
            return (env'', text'')
        _ -> arityError "pp"
    )

-- !mustache(json/yaml file)(template) preprocesses template with mustache, using a json/yaml file
mustache :: Macro
mustache = Macro "mustache" []
    "`!mustache(JSON/YAML file)(TEMPLATE)` preprocesses `TEMPLATE` with mustache, using a `JSON/YAML file`."
    (\env args -> case args of
        [dataFileName, template] -> do
            (env', dataContent) <- ppAndStrip' env dataFileName >>= locateFile env >>= ppFile env
            (env'', template') <- pp env' (fromVal template)
            let templateFileName = fromMaybe "-" (currentFile env)
            let eitherTemplate = compileTemplate templateFileName (T.pack template')
            case eitherTemplate of
                Left err -> mustacheError templateFileName(show err)
                Right compiledTemplate -> do
                    let readJSON :: String -> Either String Value
                        readJSON s = eitherDecode (BL8.fromStrict $ encodeUtf8 $ T.pack s)
                    let readYAML :: String -> Either String Value
                        readYAML s = case decodeEither' (encodeUtf8 $ T.pack s) of
                            Left err -> Left (show err)
                            Right yaml -> Right yaml
                    let decoder = case takeExtension (fromVal dataFileName) of
                            ".yml" -> readYAML
                            ".yaml" -> readYAML
                            _ -> readJSON
                    let eitherDecoded = decoder dataContent
                    case eitherDecoded of
                        Left err -> mustacheError (fromVal dataFileName) err
                        Right decodedData -> do
                            let txt = substitute compiledTemplate (toMustache decodedData)
                            return (env'', T.unpack txt)
        _ -> arityError "mustache"
    )

-- !comment[(title)](text) ignores title and text (just comments, no preprocessing)
comment :: Macro
comment = Macro "comment" []
    "`!comment(TEXT)` considers `TEXT` as well as any additional parameters as comment. Nothing is preprocessed or returned."
    (\env _ -> return (env, ""))

-- !quiet[(title)](text) ignores title and preprocesses text
-- The output of text is silently discarded, only side effects are kept in the environment.
quiet :: Macro
quiet = Macro "quiet" []
    "`!quiet(TEXT)` quietly preprocesses `TEXT` and returns nothing. Only the side effects (e.g. macro definitions) are kept in the environment."
    impl
    where
        impl env (t:ts) = do
            (env', _) <- ppAndStrip env t
            impl env' ts
        impl env [] = return (env, "")

---------------------------------------------------------------------
-- File macros
---------------------------------------------------------------------

-- !mdate(file1 ... filen)(file'1 ... file'n)... preprocesses the list of
-- filenames (space separated) and returns the most recent modification date.
-- If no file is given, the date of the main file is returned.
mdate :: Macro
mdate = Macro "mdate" []
    "`!mdate(FILES)` returns the modification date of the most recent file."
    (\env files -> do
        files' <- mapM (ppAndStrip' env) files
        files'' <- mapM (locateFile env) $ concatMap words files' ++ [fromMaybe "-" (mainFile env) | null files]
        let env' = addDeps env files''
        times <- mapM getModificationTime files''
        let lastTime = maximum times
        tz <- getCurrentTimeZone
        let localTime = utcToLocalTime tz lastTime
        return (env', formatTime (myLocale $ currentLang env) "%A %-d %B %Y" localTime)
    )

-- !main returns the name of the main file (given on the command line)
getMainFile :: Macro
getMainFile = Macro "main" []
    "`!main` returns the name of the main file (given on the command line)."
    (\env args -> case args of
        [] -> return (env, fromMaybe "-" (mainFile env))
        _ -> arityError "main"
    )

-- !file returns the name of the current file (!main or any file included from !main)
getCurrentFile :: Macro
getCurrentFile = Macro "file" []
    "`!file` returns the name of the current file."
    (\env args -> case args of
        [] -> return (env, fromMaybe "-" (currentFile env))
        _ -> arityError "file"
    )

-- !root returns the directory name of the main file
getRootDir :: Macro
getRootDir = Macro "root" []
    "`!root` returns the directory name of the main file."
    (\env args -> case args of
        [] -> case mainFile env of
            Just name | name /= "-" -> return (env, takeDirectory name)
            _ -> return (env, ".")
        _ -> arityError "root"
    )

-- !cwd returns the directory name of the current file
getCurrentDir :: Macro
getCurrentDir = Macro "cwd" []
    "`!cwd` returns the directory name of the current file."
    (\env args -> case args of
        [] -> case currentFile env of
            Just name | name /= "-" -> return (env, takeDirectory name)
            _ -> return (env, ".")
        _ -> arityError "cwd"
    )

---------------------------------------------------------------------
-- OS macros
---------------------------------------------------------------------

-- !env(name) preprocesses name, reads an environment variable (in env)
-- and emits the value of the environment variable.
readEnv :: Macro
readEnv = Macro "env" []
    "`!env(VARNAME)` preprocesses and returns the value of the process environment variable `VARNAME`."
    (\env args -> case args of
        [name] -> do
            name' <- ppAndStrip' env name
            case lookup (EnvVar (envVarStorage name')) (vars env) of
                Just val -> return (env, fromVal val)
                Nothing -> return (env, "")
        _ -> arityError "env"
    )

-- !os emits the OS name
getos :: Macro
getos = Macro "os" []
    "`!os` returns the OS name (e.g. `linux` on Linux, `darwin` on MacOS, `windows` on Windows)."
    (\env args -> case args of
        [] -> return (env, osname)
        _ -> arityError "os"
    )

-- !arch emits the architecture of the OS
getarch :: Macro
getarch = Macro "arch" []
    "`!arch` returns the machine architecture (e.g. `x86_64`, `i386`, ...)."
    (\env args -> case args of
        [] -> return (env, osarch)
        _ -> arityError "arch"
    )

---------------------------------------------------------------------
-- Arithmetic macros
---------------------------------------------------------------------

-- !add(name)(val) preprocesses name and val and adds val to the integer value
-- stored in name. If name is not defined its value is 0.
add :: Macro
add = Macro "add" []
    "`!add(VARNAME)[(INCREMENT)]` computes `VARNAME+INCREMENT` and stores the result to `VARNAME`. The default value of the increment is 1."
    impl
    where
        impl env [name, val] = do
            name' <- ppAndStrip' env name
            let val0 = fromVal $ getSymbol env (Def name')
            val1 <- ppAndStrip' env val
            let env' = env{vars = (Def name', Val (show (atoi val0 + atoi val1))) : clean (Def name') (vars env)}
            return (env', "")
        impl env [name] = impl env [name, Val "1"]
        impl _ _ = arityError "add"

-- atoi s converts s to an integer (0 if empty or not an integer)
atoi :: String -> Integer
atoi s = case reads s of
            [(i, "")] -> i
            _ -> 0

---------------------------------------------------------------------
-- String macros
---------------------------------------------------------------------

-- !append(name)(val) preprocesses name and val and appends val to
-- the value stored in name. If name is not defined its value is an empty string.
append :: Macro
append = Macro "append" []
    "`!append(VARNAME)[(TEXT)]` appends `TEXT` to `!VARNAME` and stores the result to `VARNAME`."
    impl
    where
        impl env [name, text] = do
            name' <- ppAndStrip' env name
            let text0 = fromVal $ getSymbol env (Def name')
            text1 <- ppAndStrip' env text
            let env' = env{vars = (Def name', Val (text0++text1)) : clean (Def name') (vars env)}
            return (env', "")
        impl _ _ = arityError "append"

---------------------------------------------------------------------
-- Script macros
---------------------------------------------------------------------

-- try runs an IO action on an executable and its arguments.
-- It returns the output of the IO action or raises an error.
try :: (FilePath -> [String] -> IO String) -> FilePath -> [String] -> IO String
try io exe args = do
    result <- tryIOError (io exe args)
    case result of
        Left e -> errorWithoutStackTrace $ "Error while executing `" ++
                                           unwords (exe:args) ++
                                           "`: " ++ show e
        Right output -> return output

-- script executes a script and emits the output of the script.
-- Literate contents are flushed to be usable by the script.
script :: String -> String -> String -> String -> String -> Macro
script lang cmd header ext doc = Macro lang []
    doc
    (\env args -> case args of
        [src] -> do
            (env', _) <- flushlitImpl env []
            src' <- pp' env' (fromVal src)
            let (exe:params) = words cmd
            output <- withSystemTempFile ("pp" <.> ext) $ \path handle -> do
                hWriteFileUTF8 handle $ case header of
                                            [] -> src'
                                            _ -> unlines [header, src']
                hClose handle
                setFileMode path 0o550
                try readProcessUTF8 exe (params ++ [path])
            case src of
                Val _ -> return (env', strip output)
                Block _ -> return (env', output)
        _ -> arityError lang
    )
    where
        Macro _ _ _ flushlitImpl = flushlit

exec :: Macro
exec = macro doc
    where
#if linux_HOST_OS || darwin_HOST_OS
        macro = script "exec" "sh -c" "" ".sh"
#endif
#if mingw32_HOST_OS
        macro = script "exec" cmdexe "@echo off" ".bat"
#endif
        doc = "`!exec(COMMAND)` executes a shell command with the default shell (`sh` or `cmd` according to the OS)."

rawexec :: Macro
rawexec = Macro "rawexec" [] doc impl
    where Macro _ _ doc impl = exec

---------------------------------------------------------------------
-- Diagrams macros
---------------------------------------------------------------------

-- diagram generates a GraphViz, PlantUML, ditaa or Asymptote diagram.
-- The metadata file associated to the diagram source file contains
-- additional information that can not be in the source file but
-- are required to know if the image shall be regenerated or not
-- (thanks to Vittorio Romeo for the suggestion).
diagram :: String -> DiagramRuntime -> String -> (String->String) -> (String->String) -> Macro
diagram name runtime exe header footer = Macro name []
    ("`!"++name++"(IMAGE)[(LEGEND)](GRAPH DESCRIPTION)` renders a " ++ name ++ " image with " ++ show runtime ++ ".")
    impl
    where
        impl env [path, title, code] = do
            path' <- ppAndStrip' env path
            title' <- ppAndStrip' env title
            code' <- pp' env (fromVal code)
            let (localMaybeExt, linkMaybeExt, attrs) = parseImageAttributes env path'
            let srcExt = case runtime of
                    Graphviz -> "dot"
                    PlantUML -> "uml"
                    BlockDiag -> "diag"
                    Asymptote -> "asy"
                    R -> "r"
            let (ext, ext', local, link) = case takeExtension linkMaybeExt of
                    ".png" -> (PNG, "png", localMaybeExt, linkMaybeExt)
                    ".svg" -> (SVG, "svg", localMaybeExt, linkMaybeExt)
                    ".pdf" -> (PDF, "pdf", localMaybeExt, linkMaybeExt)
                    _ -> (f, e, localMaybeExt<.>e, linkMaybeExt<.>e)
                            where (f, e) = case (runtime, exe, fileFormat env) of
                                    (Graphviz, _, Just Pdf)  -> (PDF, "pdf")
                                    (Graphviz, _, _)         -> (SVG, "svg")
                                    (PlantUML, "ditaa", _)   -> (PNG, "png")
                                    (PlantUML, _, Just Pdf)  -> (PNG, "png")
                                    (PlantUML, _, _)         -> (SVG, "svg")
                                    (BlockDiag, _, Just Pdf) -> (PDF, "pdf")
                                    (BlockDiag, _, _)        -> (SVG, "svg")
                                    (Asymptote, _, Just Pdf) -> (PDF, "pdf")
                                    (Asymptote, _, _)        -> (SVG, "svg")
                                    (R, _, Just Pdf)         -> (PDF, "pdf")
                                    (R, _, _)                -> (SVG, "svg")
            let src = local -<.> srcExt
            let dat = src <.> ext' <.> "dat"
            let img = local
            let url = link
            let code'' = unlines [header img, code', footer img]
            let metaData = unlines [ "Diagram metadata (generated by pp)"
                                   , "Generator: " ++ show runtime
                                   , "Type     : " ++ name
                                   , "Renderer : " ++ exe
                                   , "Source   : " ++ src
                                   , "Image    : " ++ img
                                   ]
            oldCodeExists <- doesFileExist src
            oldCode <- if oldCodeExists then readFileUTF8 src else return ""
            oldMetaDataExists <- doesFileExist dat
            oldMetaData <- if oldMetaDataExists then readFileUTF8 dat else return ""
            when (code'' /= oldCode || metaData /= oldMetaData) $ do
                writeFileUTF8 dat metaData
                writeFileUTF8 src code''
                void $ case runtime of
                    Graphviz ->
                        try readProcessUTF8 exe ["-T"++ext', "-o", img, src]
                    PlantUML -> do
                        plantuml <- resource "plantuml.jar" plantumlJar
                        try readProcessUTF8 "java" ["-jar", plantuml, "-t"++ext', "-charset", "UTF-8", src]
                    BlockDiag ->
                        try readProcessUTF8 exe ["-a", "-T"++ext', "-o", img, src]
                    Asymptote -> case ext of
                        PNG -> do
                            let pdf = src -<.> "pdf"
                            void $ try readProcessUTF8 exe ["-f", "pdf", "-o", pdf, src]
                            try readProcessUTF8 "convert" ["-density", "600", pdf, img]
                        SVG ->
                            try readProcessUTF8 exe ["-f", "svg", "-o", img, src]
                        PDF ->
                            try readProcessUTF8 exe ["-f", "pdf", "-o", img, src]
                    R ->
                        try readProcessUTF8 "Rscript" [src]
            let hyperlink = case currentDialect env of
                        Md -> "!["++title'++"]("++url++")"++attrs
                        Rst -> unlines [".. figure:: " ++ url, indent' 4 attrs, "", "    " ++ title']
            return (env, hyperlink)
        impl env [path, code] = impl env [path, Val "", code]
        impl _ _ = arityError name

-- indent' n block adds n space at the beginning of every lines in block
indent' :: Int -> String -> String
indent' n = unlines . map (replicate n ' ' ++) . lines

-- parseImageAttributes extracts path to generate the scripts and images
-- and path to put in the markdown link.
-- Components put in parents or brackets belongs to the path of images on disk.
-- The function also adds the file extension.
-- An optional text between curly brackets at the end are image attributs that
-- are added after the link in the markdown output.
parseImageAttributes :: Env -> String -> (FilePath, String, String)
parseImageAttributes env s = (localPath, linkPath, attrs)
    where
        prefix = imagePath env
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
                imgAttrs = case currentDialect env of
                            Md -> "{" ++ strip xs ++ "}"
                            Rst -> unlines $ filter (not . null) $ map strip $ lines xs
            in (strip local, strip link, imgAttrs)
        parseLocalAndLink cs local link =
            let (xs, cs') = span (`notElem` "([{") cs
                xs' = map (\c -> if c == '\\' then '/' else c) xs
            in parseLocalAndLink cs' (local++xs) (link++xs')
        extract right cs =
            let (xs, cs') = span (/=right) cs
            in (xs, dropWhile (==right) cs')

-- "resource name content" writes content (a C string containing PlantUML)
-- to a temporary file. It returns the path of the temporary file so the caller
-- can execute it.
resource :: FilePath -> (Ptr CChar, Ptr CInt) -> IO FilePath
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
saveLiterateContent :: Env -> [(String, String)] -> [(FilePath, String)] -> IO ()

-- file content shall be expanded with macro contents
-- previous definitions of the same file are removed from the list to avoid writing old intermediate definitions
saveLiterateContent env macros ((filename, content) : files) = do
    writeFileUTF8 filename (expandLit env macros content)
    saveLiterateContent env macros files

saveLiterateContent _ _ [] =
    return ()

-- literate programming macro
lit :: Macro

lit = Macro "literate" ["lit"]
    "`!lit[erate](FILENAME)[(LANG)][(CONTENT)]` appends `CONTENT` to the file `FILENAME`. If `FILENAME` starts with `@` it's a macro, not a file. The output is highlighted using the programming language `LANGUAGE`. The list of possible languages is given by `pandoc --list-highlight-languages`. Files are actually written when all the documents have been successfully preprocessed. Macros are expanded when the files are written. This macro provides basic literate programming features. If `LANG` is not given, pp uses the previously defined language for the same file or macro or a default language according to its name. If `CONTENT`is not given, pp returns the current content of `FILENAME`."
    impl
    where

        -- !lit(name)(lang)(content) appends content to the file or macro name.
        -- In the markdown output, the content will be colored according to the language lang.
        impl env [name, lang, content] = do
            name' <- ppAndStrip' env name
            lang' <- ppAndStrip' env lang
            content' <- ppAndStrip' env content
            let env' = litAppend env name' (Just lang') content'
            let formatedCode = litShow env (Just lang') content'
            return (env', formatedCode)

        -- !lit(name)(lang)(content) appends content to the file or macro name.
        -- The current language is the previously defined language or the default
        -- one according to name.
        impl env [name, content] = do
            name' <- ppAndStrip' env name
            content' <- ppAndStrip' env content
            let lang = litLang env name'
            let env' = litAppend env name' lang content'
            let formatedCode = litShow env lang content'
            return (env', formatedCode)

        -- !lit(name) emits the current content of a literate file or macro.
        impl env [name] = do
            name' <- ppAndStrip' env name
            let lang = litLang env name'
            let content = litGet env name'
            let formatedCode = litShow env lang content
            return (env, formatedCode)

        impl _ _ = arityError "literate"

-- source file inclusion
source :: Macro

source = Macro "source" ["src"]
    "`!source(FILENAME)[(LANG)]` or `!src(FILENAME)[(LANG)]` formats an existing source file in a colorized code block."
    impl
    where

        -- !src(name)(lang) reads a source file.
        -- In the markdown output, the content will be colored according to the language lang.
        impl env [name, lang] = do
            (env', _) <- flushlitImpl env []
            name' <- ppAndStrip' env' name
            lang' <- ppAndStrip' env' lang
            content <- readFileUTF8 name'
            let formatedCode = litShow env (Just lang') content
            return (addDep env' name', formatedCode)

        -- !src(name) reads a source file.
        -- The language is the default one according to name.
        impl env [name] = do
            (env', _) <- flushlitImpl env []
            name' <- ppAndStrip' env' name
            let lang = litLang env' name'
            content <- readFileUTF8 name'
            let formatedCode = litShow env lang content
            return (addDep env' name', formatedCode)

        impl _ _ = arityError "source"

        Macro _ _ _ flushlitImpl = flushlit

codeblock :: Macro

codeblock = Macro "codeblock" []
    "`!codeblock(LENGTH)[(CHAR)]` sets the default line separator for code blocks. The default value is a 70 tilda row (`!codeclock(70)(~)`)."
    impl
    where

        -- "!codeblock(len)(ch)" stores the new codeblock separator (ch repeated len times)
        impl env [len, ch] = do
            len' <- fromIntegral . atoi <$> ppAndStrip' env len
            when (len' < 3) codeblockError
            s' <- ppAndStrip' env ch
            let line = case s' of
                        [c] | c `elem` "~`" -> replicate len' c
                        _ -> codeblockError
            return (env{codeBlock = Just line}, "")

        -- "!codeblock(len" stores the new codeblock separator ('~' repeated len times)
        impl env [len] = impl env [len, Val "~"]

        impl _ _ = arityError "codeblock"

indent :: Macro

indent = Macro "indent" []
    "`!indent[(N)](BLOCK)` indents each line of a block with `N` spaces. The default value of `N` is 4 spaces."
    impl
    where

        -- "!indent(n)(block)" indents block by n spaces (n is optional)
        impl env [n, block] = do
            n' <- fromIntegral . atoi <$> ppAndStrip' env n
            when (n' < 3) indentError
            (env', block') <- pp env (fromVal block)
            return (env', indent' n' block')

        -- "!indent(block)" indents block by n spaces (n is optional)
        impl env [block] = impl env [Val "4", block]

        impl _ _ = arityError "indent"

-- "litGet env name" gets the current content of a literate file or macro
-- in the environment.
litGet :: Env -> String -> String
litGet env (c:name) | c `elem` literateMacroChars env = fromMaybe "" $ lookup name (litMacros env)
litGet env name = fromMaybe "" $ lookup name (litFiles env)

-- "litAppend env name lang content" appends content to a literate file or macro
-- and record the language lang in the environment
litAppend :: Env -> String -> Maybe String -> String -> Env
litAppend env name lang content = env''
    where
        oldContent = litGet env name
        newContent = oldContent++content
        env' = case name of
                (c:name') | c `elem` literateMacroChars env ->
                    env{litMacros = (name', newContent) : clean name' (litMacros env)}
                name' ->
                    env{litFiles = (name', newContent) : clean name' (litFiles env)}
        env'' = case lang of
            Just lang' -> env'{litLangs = (name, lang') : litLangs env'}
            _ -> env'

-- "litShow lang content" format content using the language lang.
-- The real format will be made by Pandoc.
litShow :: Env -> Maybe String -> String -> String
litShow env lang content = formatedBlock
    where
        formatedBlock = case currentDialect env of
            Md  -> markdownBlock
            Rst -> reStructuredTextBlock

        markdownBlock = case lang of
            Just lang'  -> unlines [sep ++ " {." ++ lang' ++ "}", content, sep]
            Nothing     -> unlines [sep, content, sep]

        reStructuredTextBlock = case lang of
            Just lang'  -> unlines [ ".. code-block:: " ++ lang', "", indent' 4 content, ""]
            Nothing     -> unlines [ ".. code-block:: none", "", indent' 4 content, ""]

        sep = fromMaybe (replicate 70 '~') (codeBlock env)

-- "litLang env name" looks for the language of the literate file or macro
-- in the environment if it has already been defined or tries to guess it
-- from its name.
litLang :: Env -> String -> Maybe String
litLang env name = case lookup name (litLangs env) of
    Just lang -> Just lang
    Nothing   -> defaultLitLang name

-- default language deduced from the name of the file
-- (see "pandoc --version")
defaultLitLang :: FilePath -> Maybe String
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
flushlit = Macro "flushliterate" ["flushlit"]
    "`!flushlit[erate]` writes files built with `!lit` before reaching the end of the document. This macro is automatically executed before any script execution or file inclusion with `!src`."
    (\env args -> case args of
        [] -> do
            saveLiterateContent env (litMacros env) (litFiles env)
            return (env, "")
        _ -> arityError "flushliterate"
    )

-- "expandLit macros text" expand literal macros in a string.
-- macros is a lookup table containing all the literal macros
expandLit :: Env -> [(String, String)] -> String -> String
expandLit env macros (c0:s)
    | c0 `elem` literateMacroChars env = content' ++ expandLit env macros s'
    | otherwise = c0 : expandLit env macros s
    where
        (name, s') = span isValidMacroNameChar s
        content' = case lookup name macros of
                    Nothing -> c0:name
                    Just content -> expandLit env macros content
expandLit _ _ [] = []

---------------------------------------------------------------------
-- CSV tables
---------------------------------------------------------------------

csv :: Macro

csv = Macro "csv" []
    "`!csv(FILENAME)[(HEADER)]` converts a CSV file to a Markdown or reStructuredText table. `HEADER` defines the header of the table, fields are separated by pipes (`|`). If `HEADER` is not defined, the first line of the file is used as the header of the table."
    impl
    where

        -- !csv(filename) converts a CSV file to a markdown or reStructuredText table
        -- The delimiter is automagically infered (hope it works...).
        -- The first line of the CSV file is the header of the table.
        impl env [filename] = do
            filename' <- ppAndStrip' env filename
            csvData <- readFileUTF8 filename'
            let table = makeTable (currentDialect env) Nothing csvData
            return (addDep env filename', table)

        -- !csv(filename)(header) converts a CSV file to a markdown or reStructuredText table
        -- The delimiter is automagically infered (hope it works...).
        -- The first line of the CSV file is the header of the table.
        impl env [filename, header] = do
            filename' <- ppAndStrip' env filename
            header' <- ppAndStrip' env header
            let fields = splitOn "|" header'
            csvData <- readFileUTF8 filename'
            let table = makeTable (currentDialect env) (Just fields) csvData
            return (addDep env filename', table)

        impl _ _ = arityError "csv"

---------------------------------------------------------------------
-- Parser customization macros
---------------------------------------------------------------------

macrochars :: Macro
macrochars = Macro "macrochars" []
    "`!macrochars(CHARS)` defines the chars used to call a macro. The default value is `\"!\"`. Any non space character can start a macro call (e.g. after `!macrochars(!\\)` both `!foo` and `\\foo` are valid macro calls."
    (\env args -> case args of
        [chars] -> do
            chars' <- ppAndStrip' env chars
            let env' = env{macroChars = filter (not . isSpace) chars'}
            unless (checkParserConsistency env') $ macrocharsError (fromVal chars)
            return (env', "")
        _ -> arityError "macrochars"
    )

macroargs :: Macro
macroargs = Macro "macroargs" []
    "`!macroargs(CHARS)` defines the chars used to separate macro arguments. The default value is `\"(){}[]\"` (e.g. after `!macroargs(()«»)` both `!foo(...)` and `!foo«...»` are valid macro calls)."
    (\env args -> case args of
        [chars] -> do
            chars' <- ppAndStrip' env chars
            let pairs = chunksOf 2 $ filter (not . isSpace) chars'
            unless (all ((==2) . length) pairs) $ macroargsError (fromVal chars)
            let assoc = map (\[o,c] -> (o,c)) pairs
            let env' = env{openCloseChars = assoc}
            unless (checkParserConsistency env') $ macroargsError (fromVal chars)
            return (env', "")
        _ -> arityError "macroargs"
    )

macroblockargs :: Macro
macroblockargs = Macro "macroblockargs" []
    "`!macroblockargs(CHARS)` defines the chars used to separate macro block arguments. The default value is ``\"~`\"``."
    (\env args -> case args of
        [chars] -> do
            chars' <- ppAndStrip' env chars
            let env' = env{blockChars = filter (not . isSpace) chars'}
            unless (checkParserConsistency env') $ macroblockargsError (fromVal chars)
            return (env', "")
        _ -> arityError "macroblockargs"
    )

literatemacrochars :: Macro
literatemacrochars = Macro "literatemacrochars" []
    "`!literatemacrochars(CHARS)` defines the chars used to identify literate programming macros. The default value is `\"@\"`. Any non space character can start a literate programming macro (e.g. after `!literatemacrochars(@&)` both `@foo` and `&foo` are valid macro calls."
    (\env args -> case args of
        [chars] -> do
            chars' <- ppAndStrip' env chars
            let env' = env{literateMacroChars = filter (not . isSpace) chars'}
            unless (checkParserConsistency env') $ literatemacrocharsError (fromVal chars)
            return (env', "")
        _ -> arityError "literatemacrochars"
    )

checkParserConsistency :: Env -> Bool
checkParserConsistency env = concat sets == nub (concat sets)
                             && all (not . null) sets
    where
        sets = [ macroChars env
               , concat [ [o,c] | (o,c) <- openCloseChars env ]
               , blockChars env
               , literateMacroChars env
               ]

---------------------------------------------------------------------
-- Help generation
---------------------------------------------------------------------

builtinmacros :: Macro
builtinmacros = Macro "macros" []
    "`!macros` lists the builtin macros."
    (\env args -> case args of
        [] -> do
            let macroList = concat [ name:aliases | Macro name aliases _ _ <- builtin ]
            return (env, unlines macroList)
        _ -> arityError "macros"
    )

usermacros :: Macro
usermacros = Macro "usermacros" []
    "`!usermacros` lists the user macros."
    (\env args -> case args of
        [] -> do
            let macroList = reverse $ nub [ name | (Def name, _) <- vars env, not ("_" `isPrefixOf` name) ]
            return (env, unlines macroList)
        _ -> arityError "usermacros"
    )

help :: Macro
help = Macro "help" []
    "`!help` prints built-in macro help."
    (\env args -> case args of
        [] -> do
            let docs = renderMarkdownHelp [ (name:aliases, doc) | Macro name aliases doc _ <- builtin ]
            return (env, docs)
        _ -> arityError "help"
    )

userhelp :: Macro
userhelp = Macro "userhelp" []
    "`!userhelp` prints user macro help."
    (\env args -> case args of
        [] -> do
            let docs = renderMarkdownHelp [ ([name], doc) | (Def name, doc) <- docstrings env, not ("_" `isPrefixOf` name) ]
            return (env, docs)
        _ -> arityError "help"
    )

longHelp :: Env -> String
longHelp env = unlines $ copyright ++ commandline ++ builtinMacros ++ userMacros
    where
        copyright =
            [ Version.copyright
            ]
        commandline =
            [ "COMMAND LINE OPTIONS"
            , ""
            , indent' 4 Version.help
            ]
        builtinMacros =
            [ "BUILT-IN MACROS"
            , ""
            , indent' 4 $ renderPlainHelp [ (name:aliases, doc) | Macro name aliases doc _ <- builtin ]
            ]
        userMacros
            | null (docstrings env) = []
            | otherwise =
                [ "USER MACROS"
                , ""
                , indent' 4 $ renderPlainHelp [ ([name], doc) | (Def name, doc) <- docstrings env, not ("_" `isPrefixOf` name) ]
                ]

longUserHelp :: Env -> String
longUserHelp env = unlines userMacros
    where
        userMacros
            | null (docstrings env) = []
            | otherwise =
                [ "USER MACROS"
                , ""
                , indent' 4 $ renderPlainHelp [ ([name], doc) | (Def name, doc) <- docstrings env, not ("_" `isPrefixOf` name) ]
                ]

renderMarkdownHelp :: [([String], String)] -> String
renderMarkdownHelp docs = 
    unlines $ concat [ [ renderNames names, renderDoc doc ] | (names, doc) <- docs ]
    where
        renderNames :: [String] -> String
        renderNames names = intercalate ", " [ "**`" ++ name ++ "`**" | name <- names ]
        renderDoc :: String -> String
        renderDoc = (':':) . drop 1 . indent' 4 . unlines . wrap 60

renderPlainHelp :: [([String], String)] -> String
renderPlainHelp docs = 
    unlines $ concat [ [ renderNames names, renderDoc doc ] | (names, doc) <- docs ]
    where
        renderNames :: [String] -> String
        renderNames = intercalate ", "
        renderDoc :: String -> String
        renderDoc = indent' 4 . filter (/='`') . unlines . wrap 60

wrap :: Int -> String -> [String]
wrap width s = reverse $ map (reverse . strip) $ go s [] []
    where
        go, goSpaces, goTicks, goWord :: String -> String -> [String] -> [String]
        go [] [] ls = ls
        go [] l ls = l:ls
        go (c:cs) l ls
            | isSpace c = if length l > width
                            then goSpaces cs "" (l:ls)
                            else goSpaces cs (' ':l) ls
            | c == '`' = goTicks cs (c:l) ls
            | otherwise = goWord cs (c:l) ls
        goSpaces (c:cs) l ls
            | isSpace c = goSpaces cs l ls
            | otherwise = go (c:cs) l ls
        goSpaces [] l ls = go [] l ls
        goTicks ('`':cs) l ls = go cs ('`':l) ls
        goTicks (c:cs) l ls = goTicks cs (c:l) ls
        goTicks [] l ls = go [] l ls
        goWord (c:cs) l ls
            | isSpace c = go (c:cs) l ls
            | otherwise = goWord cs (c:l) ls
        goWord [] l ls = go [] l ls
