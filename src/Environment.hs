{- PP

Copyright (C) 2015-2023 Christophe Delord

http://cdelord.fr/pp

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

module Environment ( Env(..)
                   , Var(..)
                   , Val(..)
                   , Macro(..)
                   , fromVal
                   , getSymbol
                   , clean
                   , lookupMacro
                   , call
                   , initialEnvironment
                   , addDep
                   , addDeps
                   -- exported for test purpose
                   , defaultMacroChars
                   , defaultOpenCloseChars
                   , defaultBlockChars
                   , defaultLiterateMacroChars
                   )
where

import Data.Maybe
import System.Environment

import Formats
import Localization
import OSAbstraction

-- defaultMacroChars is the list of characters used to execute a macro
defaultMacroChars :: Chars
defaultMacroChars = ['!']

-- defaultOpenCloseChars is the list of characters that can delimit macro parameters.
defaultOpenCloseChars :: [(Char, Char)]
defaultOpenCloseChars = [('(', ')'), ('{', '}'), ('[', ']')]

-- defaultBlockChars is the list of characters that can delimit macro parameters with
-- the Markdown code block syntax.
defaultBlockChars :: Chars
defaultBlockChars = ['~', '`']

-- literate programming macros
defaultLiterateMacroChars :: Chars
defaultLiterateMacroChars = ['@']

-- symbol type of a definition in the environment
data Var = Def String           -- user macro definition
         | EnvVar String        -- environment variable
         deriving (Eq)

-- values stored in the environment
data Val = Val String           -- regular (stripped) value
         | Block String         -- code block (unstripped) value
         deriving (Eq, Show)

type Chars = String

-- pp environment (macros, parameters, ...)
data Env = Env { vars :: [(Var, Val)]               -- lookup table of global variables and user macros
               , docstrings :: [(Var, String)]      -- docstrings of user macros
               , arguments :: [(String, Val)]       -- local arguments of a user defined macro
               , currentLang :: Lang                -- current language
               , fileFormat :: Maybe Format         -- current file format
               , currentDialect :: Dialect          -- current dialect
               , mainFile :: Maybe FilePath         -- main file name (given on the command line)
               , currentFile :: Maybe FilePath      -- current file name (can be included in other files)
               , makeTarget :: Maybe FilePath       -- target name for the dependency file (-M)
               , dependencies :: [FilePath]         -- list of dependencies
               , litFiles :: [(FilePath, String)]   -- literate file name
               , litMacros :: [(String, String)]    -- literate macro name
               , litLangs :: [(String, String)]     -- language of a literate file or macro
               , imagePath :: String                -- prefix added to the image path (diagrams)
               , codeBlock :: Maybe String          -- format of code blocks generated by source
               , macroChars :: Chars                -- chars that execute macros
               , openCloseChars :: [(Char, Char)]   -- parameter delimiters
               , blockChars :: Chars                -- block delimiter chars
               , literateMacroChars :: Chars        -- literate programming macro delimiters
               , ignoreStdin :: Bool                -- some command line arguments disable stdin
               , customPlantuml :: Maybe FilePath   -- external PlantUML jar file to use
               , customDitaa :: Maybe FilePath      -- external ditaa jar file to use
               }

fromVal :: Val -> String
fromVal (Val s) = s
fromVal (Block s) = s

-- `clean name env` removes an outdated variable from the environment
clean :: Eq a => a -> [(a, b)] -> [(a, b)]
clean var = filter ((/=var) . fst)

-- Get a variable in the environment
getSymbol :: Env -> Var -> Val
getSymbol Env{vars=vs} var = fromMaybe (Val "") (lookup var vs)

-- A macro takes an environment and arguments
-- and returns a new environment and the result of the macro as a string.
-- It also as some additional metadata (docstring)
type MacroFunc = Env -> [Val] -> IO (Env, String)
data Macro = Macro String [String]  -- name and aliases
                   String           -- docstring
                   MacroFunc        -- implementation

-- Lookup a macro in a macro list
lookupMacro :: String -> [Macro] -> Maybe Macro
lookupMacro name (macro@(Macro name' aliases' _ _) : macros)
    | name `elem` (name':aliases') = Just macro
    | otherwise = lookupMacro name macros
lookupMacro _ [] = Nothing

-- Call a macro
call :: Macro -> Env -> [Val] -> IO (Env, String)
call macro = let Macro _ _ _ macroImpl = macro in macroImpl

-- Build the initial environment
initialEnvironment :: Lang -> Dialect -> IO Env
initialEnvironment defaultLang defaultDialect = do
    -- get $LANG (en, fr, ...)
    envVars <- getEnvironment
    let lang = fromMaybe defaultLang $ readCapMaybe $ take 2 <$> lookup "LANG" envVars
    -- get $FORMAT (html, pdf, ...)
    let fmt = readCapMaybe $ lookup "FORMAT" envVars
    -- get $DIALECT (md, rst, ...)
    let dial = fromMaybe defaultDialect $ readCapMaybe $ lookup "DIALECT" envVars
    -- the initial environment contains the language, the format and the environment variables
    return Env { vars = [(EnvVar (envVarStorage name), Val val) | (name, val) <- envVars]
               , docstrings = []
               , arguments = []
               , currentLang = lang
               , fileFormat = fmt
               , currentDialect = dial
               , mainFile = Nothing
               , currentFile = Nothing
               , makeTarget = Nothing
               , dependencies = []
               , litFiles = []
               , litMacros = []
               , litLangs = []
               , imagePath = ""
               , codeBlock = Nothing
               , macroChars = defaultMacroChars
               , openCloseChars = defaultOpenCloseChars
               , blockChars = defaultBlockChars
               , literateMacroChars = defaultLiterateMacroChars
               , ignoreStdin = False
               , customPlantuml = Nothing
               , customDitaa = Nothing
               }

-- track dependencies
-- see issue 30, thanks to trygvis for the idea
addDep :: Env -> FilePath -> Env
addDep env@Env{dependencies=deps} name = env{dependencies=name:deps}

addDeps :: Env -> [FilePath] -> Env
addDeps = foldl addDep
