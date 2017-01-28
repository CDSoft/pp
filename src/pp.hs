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

import qualified Version
import Environment
import Preprocessor
import UTF8

-- The main function builds the initial environment, parses the input
-- and print the output on stdout.
main :: IO ()
main = do
    -- work with UTF-8 documents
    setUTF8Encoding stdin
    setUTF8Encoding stdout
    -- parse the arguments and produce the preprocessed output
    env <- initialEnvironment
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

-- "doArg" env "-v" shows the current version of pp
doArg _ "-v" = putStrLn Version.copyright >> exitSuccess

-- "doArg" env "-h" shows a short help message
doArg _ "-h" = putStrLn Version.help >> exitSuccess

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
