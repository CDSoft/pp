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

module ErrorMessages ( unexpectedEndOfFile
                     , arityError
                     , invalidNameError
                     , builtinRedefinition
                     , fileNotFound
                     , codeblockError
                     , indentError
                     , macrocharsError
                     , macroargsError
                     , literatemacrocharsError
                     , defaultParserConfigurationError
#if linux_HOST_OS || darwin_HOST_OS
                     , windowsOnlyError
#endif
                     )
where

import Data.Maybe

import Environment

-- raise an end of file error
unexpectedEndOfFile :: Env -> FilePath -> t
unexpectedEndOfFile env name = errorWithoutStackTrace $ "Unexpected end of file in " ++ fromMaybe "-" (currentFile env) ++
                                                        "\nAn argument of the macro \"" ++ name ++ "\" may not be correctly delimited."

-- raise a file not found error
fileNotFound :: FilePath -> t
fileNotFound name = errorWithoutStackTrace $ "File not found: " ++ name

-- raise an arity error
arityError :: String -> t
arityError name = errorWithoutStackTrace $ name ++ " wrong number of arguments"

-- raise a wrong codeblock specification error
codeblockError :: t
codeblockError = errorWithoutStackTrace "codeblock expects a length higher than 3 and either a tilda or a backtick."

-- raise a wrong indentation specification error
indentError :: t
indentError = errorWithoutStackTrace "indent expects a length higher than 3."

-- raise an invalid name error
invalidNameError :: String -> t
invalidNameError name = errorWithoutStackTrace $ "\"" ++ name ++"\" is not a valid macro name."

-- raise an builtin redefinition error
builtinRedefinition :: String -> t
builtinRedefinition name = errorWithoutStackTrace $ "\"" ++ name ++"\" is a built-in macro and can not be redefined."

-- raise a parser consistency error
macrocharsError :: String -> t
macrocharsError chars = errorWithoutStackTrace $ "macrochars invalid parameter: \"" ++ chars ++ "\""

-- raise a parser consistency error
macroargsError :: String -> t
macroargsError chars = errorWithoutStackTrace $ "macroargs invalid parameter: \"" ++ chars ++ "\""

-- raise a parser consistency error
literatemacrocharsError :: String -> t
literatemacrocharsError chars = errorWithoutStackTrace $ "literatemacrochars invalid parameter: \"" ++ chars ++ "\""

-- raise a parser consistency error
defaultParserConfigurationError :: t
defaultParserConfigurationError = errorWithoutStackTrace "Unexpected error: Invalid parser configuration"

#if linux_HOST_OS || darwin_HOST_OS
windowsOnlyError :: String -> t
windowsOnlyError name = errorWithoutStackTrace $ name ++ " is available on Windows only"
#endif
