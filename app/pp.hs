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

import Control.Monad
import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import System.IO

import ErrorMessages
import Environment
import Formats
import Localization
import Preprocessor
import UTF8
import qualified Version

exitMacros :: [String]
exitMacros = [ "langs", "formats", "dialects" ]
           ++ map show langs
           ++ map show formats
           ++ map show dialects
           ++ [ "os", "arch" ]
           ++ [ "macros", "usermacros" ]

-- The main function builds the initial environment, parses the input
-- and print the output on stdout.
main :: IO ()
main = do
    -- work with UTF-8 documents
    setUTF8Encoding stdin
    setUTF8Encoding stdout
    -- parse the arguments and produce the preprocessed output
    env <- initialEnvironment (head langs) (head dialects)
    unless (checkParserConsistency env) defaultParserConfigurationError
    (env', doc) <- getArgs >>= doArgs env
    case makeTarget env' of
        Just target ->
            -- -M option => print dependencies
            putStrLn $ target ++ ": " ++ (unwords.nub.reverse) (dependencies env')
        _ ->
            -- just write the preprocessed output to stdout
            putStr doc
    -- finally save the literate content (if any)
    saveLiterateContent env' (litMacros env') (litFiles env')

-- "doArgs env args" parses the command line arguments
-- and returns an updated environment and the preprocessed output
doArgs :: Env -> [String] -> IO (Env, String)

-- Parse all the arguments.
doArgs env (arg:args) = do
    (env', doc, args') <- doArg env arg args
    (env'', doc') <- doArgs env' args'
    return (env'', doc ++ doc')

-- No more argument
-- mainFileTag is put in the environment only when a file has been preprocessed.
-- This variable is not set when no file is given on the command line.
-- In this case, pp preprocesses stdin.
doArgs env [] = case (mainFile env, ignoreStdin env) of
    -- nothing has been preprocessed, let's try stdin
    (Nothing, False) -> do
        (env', doc, _) <- doArg env "-" []
        return (env', doc)
    -- something has already been preprocessed
    -- or stdin is ignored when some macros are use on the command line
    _ -> return (env, "")

-- "doArg env arg" parses one argument
-- and returns an updated environment, the output produced by the argument and the remaining arguments.
doArg :: Env -> String -> [String] -> IO (Env, String, [String])

-- "doArg" env "-v" shows the current version of pp
doArg _ "-v" _ = putStrLn Version.copyright >> exitSuccess

-- "doArg" env "-h" shows a short help message
doArg _ "-h" _ = putStrLn Version.help >> exitSuccess

-- "doArg" env "-help" show a longer help message
doArg env "-help" _ = putStrLn (longHelp env) >> exitSuccess

-- "doArg" env "-userhelp" show a longer help message (user macros only)
doArg env "-userhelp" _ = putStrLn (longUserHelp env) >> exitSuccess

-- "doArg env "-D name=value"" adds a new definition to the environment.
doArg env "-D" (def:args) = return (env{vars=(Def name, Val (drop 1 value)) : clean (Def name) (vars env)}, "", args)
    where (name, value) = span (/= '=') def

-- "doArg env "-Dname=value"" adds a new definition to the environment.
doArg env ('-':'D':def) args = return (env{vars=(Def name, Val (drop 1 value)) : clean (Def name) (vars env)}, "", args)
    where (name, value) = span (/= '=') def

-- "doArg env "-U name"" removes a definition from the environment.
doArg env "-U" (name:args) = return (env{vars=clean (Def name) (vars env)}, "", args)

-- "doArg env "-Uname"" removes a definition from the environment.
doArg env ('-':'U':name) args = return (env{vars=clean (Def name) (vars env)}, "", args)

-- "doArg env "-fr|-en"" changes the current language
doArg env ('-':lang) args | isJust maybeLang =
    return (env{currentLang=fromJust maybeLang}, "", args) where maybeLang = readCap lang

-- "doArg env "-html|-pdf|-odt|-epub|-mobi"" changes the current format
doArg env ('-':fmt) args | isJust maybeFmt =
    return (env{fileFormat=maybeFmt}, "", args) where maybeFmt = readCap fmt

-- "doArg env "-md|-rst"" changes the current dialect
doArg env ('-':dial) args | isJust maybeDial =
    return (env{currentDialect=fromJust maybeDial}, "", args) where maybeDial = readCap dial

-- "doArg env "-img prefix"" changes the output image path prefix
doArg env "-img" (prefix:args) =
    return (env{imagePath=prefix}, "", args)

-- "doArg env "-img=prefix"" changes the output image path prefix
doArg env ('-':'i':'m':'g':'=':prefix) args =
    return (env{imagePath=prefix}, "", args)

-- "doArg env "-import name" preprocesses a file and discards its output
-- It can be used to load macro definitinos for instance
doArg env "-import" (name:args) = do
    (env', _) <- ppFile env{currentFile=Just name} name
    return (env', "", args)

-- "doArg env "-import=name" preprocesses a file and discards its output
-- It can be used to load macro definitinos for instance
doArg env ('-':'i':'m':'p':'o':'r':'t':'=':name) args = do
    (env', _) <- ppFile env{currentFile=Just name} name
    return (env', "", args)

-- "doArg" env "-M" target enables the tracking of dependencies (i.e. included and imported files)
-- target is the name of the Makefile target
doArg env "-M" (target:args) =
    return (env{makeTarget=Just target}, "", args)

-- "doArg" env "-M=target" enables the tracking of dependencies (i.e. included and imported files)
-- target is the name of the Makefile target
doArg env ('-':'M':'=':target) args =
    return (env{makeTarget=Just target}, "", args)

-- "doArg" env "-plantuml" <path to plantuml.jar> uses a specific plantuml.jar instead of the embedded one
doArg env "-plantuml" (plantumlJar:args) =
    return (env{customPlantuml=Just plantumlJar}, "", args)

-- "doArg" env "-plantuml=<path to plantuml.jar>" uses a specific plantuml.jar instead of the embedded one
doArg env ('-':'p':'l':'a':'n':'t':'u':'m':'l':'=':plantumlJar) args =
    return (env{customPlantuml=Just plantumlJar}, "", args)

-- "doArg" env "-ditaa" <path to ditaa.jar> uses a specific ditaa.jar instead of the embedded one
doArg env "-ditaa" (ditaaJar:args) =
    return (env{customDitaa=Just ditaaJar}, "", args)

-- "doArg" env "-ditaa=<path to ditaa.jar>" uses a specific ditaa.jar instead of the embedded one
doArg env ('-':'d':'i':'t':'t':'a':'=':ditaaJar) args =
    return (env{customDitaa=Just ditaaJar}, "", args)

doArg env ('-':arg) args
    | not (null arg) = case maybeMacro of
        -- Macros can be called from the command line
        Just _ -> do
            (env', s) <- pp env code
            let env'' = env'{ignoreStdin = ignoreStdin env' || name `elem` exitMacros}
            return (env'', if null s then s else s++"\n", args)
        -- Other arguments starting with "-" are invalid.
        Nothing -> errorWithoutStackTrace $ "Unexpected argument: " ++ arg
    where
        (functor, params') = span (/='=') arg
        (name, params) = span isValidMacroNameChar functor
        code = head (macroChars env) : name
               ++ params
               ++ if not (null params')
                then replicate 70 '~' ++ params' ++ replicate 70 '~'
                else ""
        maybeMacro = lookupMacro name builtin

-- "doArg env filename" preprocessed the content of a file using the current environment.
-- The mainFileTag variable is added to the environment.
-- It contains the name of the file being preprocessed.
doArg env name args = do
    (env', doc) <- ppFile env{mainFile=Just name} name
    return (env', doc, args)
