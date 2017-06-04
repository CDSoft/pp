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

import System.IO
import System.Environment
import System.Exit
import Data.Char
import Data.List

import qualified Version
import Environment
import Preprocessor
import Localization
import UTF8

-- The main function builds the initial environment, parses the input
-- and print the output on stdout.
main :: IO ()
main = do
    -- work with UTF-8 documents
    setUTF8Encoding stdin
    setUTF8Encoding stdout
    -- parse the arguments and produce the preprocessed output
    env <- initialEnvironment (head langs) (head dialects)
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
    (env', doc, args') <- doArg env arg args
    (env'', doc') <- doArgs env' args'
    return (env'', doc ++ doc')

-- No more argument
-- mainFileTag is put in the environment only when a file has been preprocessed.
-- This variable is not set when no file is given on the command line.
-- In this case, pp preprocesses stdin.
doArgs env [] = case lookup MainFile env of
    -- nothing has been preprocessed, let's try stdin
    Nothing -> do (env', doc, _) <- doArg env "-" []
                  return (env', doc)
    -- something has already been preprocessed
    Just _ -> return (env, "")

-- "doArg env arg" parses one argument
-- and returns an updated environment, the output produced by the argument and the remaining arguments.
doArg :: Env -> String -> [String] -> IO (Env, String, [String])

-- "doArg" env "-v" shows the current version of pp
doArg _ "-v" _ = putStrLn Version.copyright >> exitSuccess

-- "doArg" env "-h" shows a short help message
doArg _ "-h" _ = putStrLn Version.help >> exitSuccess

-- "doArg env "-D name=value"" adds a new definition to the environment.
doArg env "-D" (def:args) = return ((Def name, Val (drop 1 value)) : clean (Def name) env, "", args)
    where (name, value) = span (/= '=') def

-- "doArg env "-Dname=value"" adds a new definition to the environment.
doArg env ('-':'D':def) args = return ((Def name, Val (drop 1 value)) : clean (Def name) env, "", args)
    where (name, value) = span (/= '=') def

-- "doArg env "-U name"" removes a definition from the environment.
doArg env "-U" (name:args) = return (clean (Def name) env, "", args)

-- "doArg env "-Uname"" removes a definition from the environment.
doArg env ('-':'U':name) args = return (clean (Def name) env, "", args)

-- "doArg env "-fr|-en"" changes the current language
doArg env ('-':lang) args | lang' `elem` langs =
    return ((Lang, Val lang') : clean Lang env, "", args) where lang' = map toLower lang

-- "doArg env "-html|-pdf|-odt|-epub|-mobi"" changes the current format
doArg env ('-':fmt) args | fmt' `elem` formats =
    return ((FileFormat, Val fmt') : clean FileFormat env, "", args) where fmt' = map toLower fmt

-- "doArg env "-md|-rst"" changes the current dialect
doArg env ('-':dial) args | dial' `elem` dialects =
    return ((Dialect, Val dial') : clean Dialect env, "", args) where dial' = map toLower dial

-- "doArg env "-img prefix"" changes the output image path prefix
doArg env "-img" (prefix:args) =
    return ((ImagePath, Val prefix) : clean ImagePath env, "", args)

-- "doArg env "-img=prefix"" changes the output image path prefix
doArg env ('-':'i':'m':'g':'=':prefix) args =
    return ((ImagePath, Val prefix) : clean ImagePath env, "", args)

-- "doArg env "-import name" preprocesses a file and discards its output
-- It can be used to load macro definitinos for instance
doArg env "-import" (name:args) = do
    (env', _) <- ppFile ((CurrentFile, Val name) : env) name
    return (env', "", args)

-- "doArg env "-import=name" preprocesses a file and discards its output
-- It can be used to load macro definitinos for instance
doArg env ('-':'i':'m':'p':'o':'r':'t':'=':name) args = do
    (env', _) <- ppFile ((CurrentFile, Val name) : env) name
    return (env', "", args)

-- "doArg" env "-langs" shows the list of languages
doArg _ "-langs" _ = putStrLn (unwords $ sort langs) >> exitSuccess

-- "doArg" env "-dialects" shows the list of dialects
doArg _ "-dialects" _ = putStrLn (unwords $ sort dialects) >> exitSuccess

-- "doArg" env "-formats" shows the list of formats
doArg _ "-formats" _ = putStrLn (unwords $ sort formats) >> exitSuccess

-- Other arguments starting with "-" are invalid.
doArg _ ('-':arg) _ | not (null arg) = error $ "Unexpected argument: " ++ arg

-- "doArg env filename" preprocessed the content of a file using the current environment.
-- The mainFileTag variable is added to the environment.
-- It contains the name of the file being preprocessed.
doArg env name args = do
    (env', doc) <- ppFile ((MainFile, Val name) : env) name
    return (env', doc, args)
