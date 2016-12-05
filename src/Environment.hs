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

module Environment where

import Data.Char
import Data.Maybe
import System.Environment

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

-- values stored in the environment
data Val = Val String           -- regular (stripped) value
         | Block String         -- code block (unstripped) value
         deriving (Eq)

-- Preprocessor environment (lookup table)
type Env = [(Var, Val)]

fromVal :: Val -> String
fromVal (Val s) = s
fromVal (Block s) = s

-- `clean name env` removes an outdated variable from the environment
clean :: Var -> Env -> Env
clean var = filter ((/=var) . fst)

-- Get a variable in the environment
getSymbol :: Env -> Var -> Val
getSymbol env var = fromMaybe (Val "") (lookup var env)

-- Build the initial environment
initialEnvironment :: IO Env
initialEnvironment = do
    -- get $LANG (en, fr, ...)
    envVars <- getEnvironment
    let lang = case lookup "LANG" envVars of
                    Just ('C':_) -> "en"
                    Just val -> map toLower $ take 2 val
                    Nothing -> ""
    -- get $FORMAT (html, pdf, ...)
    let fmt = map toLower $ fromMaybe "" (lookup "FORMAT" envVars)
    -- the initial environment contains the language, the format and the environment variables
    return $ (Lang, Val lang) : (FileFormat, Val fmt) : [(EnvVar name, Val val) | (name, val) <- envVars]
