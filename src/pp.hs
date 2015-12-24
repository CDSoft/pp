{- PP

Copyright (C) 2015 Christophe Delord

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

import System.IO
import System.Environment
import System.Directory
import Data.List
import Data.Char
--import Data.Maybe
import System.Process(readProcess)
--import System.Directory
import System.FilePath
#ifdef linux_HOST_OS
import System.Locale
#endif
import Data.Time
--import Data.Time.Format

type Chars = String
type Env = [(String, String)]
type Macro = Env -> [String] -> IO (Env, String)
type Prepro = Env -> String ->  IO (Env, String)

main :: IO ()
main = do
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    args <- getArgs
    env <- getEnvironment
    let lang = case lookup "LANG" env of
                    Just ('C':_) -> "en"
                    Just val -> take 2 val
                    Nothing -> ""
    let fmt = case lookup "FORMAT" env of
                    Just val -> val
                    Nothing -> ""
    doc <- doArgs ((langTag, lang) : (formatTag, fmt) : [(envTag++name, val) | (name, val) <- env]) args
    putStr doc

doArgs :: Env -> [String] -> IO String
doArgs env (arg:args) = do
    (env', doc) <- doArg env arg
    doc' <- doArgs env' args
    return $ doc ++ doc'
doArgs env [] = case lookup mainTag env of
                    Nothing -> -- nothing have been preprocessed, let's try stdin
                               do
                                   (_, doc) <- doArg env "-"
                                   return doc
                    Just _ -> -- something has already been preprocessed
                              return ""

doArg :: Env -> String -> IO (Env, String)

doArg env ('-':'D':def) = return $ ((n, drop 1 v) : env, "")
    where (n,v) = span (/='=') def

doArg env ('-':'U':name) = return $ ([(n,v) | (n,v)<-env, n/=name], "")

doArg _ ('-':arg) | not (null arg) = error $ "Unexpected argument: " ++ arg

doArg env name = ppFile ((mainTag, name) : env) name

ppFile :: Env -> FilePath -> IO (Env, String)
ppFile env name = do
    let caller = (getSymbol env currentTag)
    content <- readFileUTF8 name
    (env', doc) <- pp ((currentTag, name) : env) content
    return $ ((currentTag, caller) : env', doc)

readFileUTF8 :: String -> IO String
readFileUTF8 "-" = getContents
readFileUTF8 name = do
    h <- openFile name ReadMode
    hSetEncoding h utf8
    doc <- hGetContents h
    return doc

-- attribute name of the current file in the environment
currentTag :: String
currentTag = "file"

-- attribute name of the main file in the environment
mainTag :: String
mainTag = "main"

-- attribute name of the current language in the environment
langTag :: String
langTag = "lang"

-- language list
langs :: [String]
langs = ["fr", "en"]

-- attribute name of the current output format
formatTag :: String
formatTag = "format"

-- format list
formats :: [String]
formats = ["html", "pdf"]

-- specific character prepend to the process environment variable names in the environment
envTag :: String
envTag = "$"

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

          , ("add", add)

          ]
          ++ [ (lang, language lang) | lang <- langs]
          ++ [ (fmt, format fmt) | fmt <- formats]

define :: Macro
define env [name, value] = return $ ((name, value) : env, "")
define env [name] = define env [name, ""]
define _ _ = arityError "define" [1,2]

undefine :: Macro
undefine env [name] = return $ ([ (n,v) | (n,v) <- env, n/=name ], "")
undefine _ _ = arityError "undefine" [1]

ifdef :: Macro
ifdef env [name, t, e] = case lookup name env of
                            Just _ -> pp env t
                            Nothing -> pp env e
ifdef env [name, t] = ifdef env [name, t, ""]
ifdef _ _ = arityError "ifdef" [1, 2]

ifndef :: Macro
ifndef env [name, t, e] = ifdef env [name, e, t]
ifndef env [name, t] = ifdef env [name, "", t]
ifndef _ _ = arityError "ifndef" [1, 2]

ifeq :: Macro
ifeq env [x, y, t, e] = do (env', x') <- pp env x
                           (env'', y') <- pp env' y
                           pp env'' (if strip x' == strip y' then t else e)
                                where strip = filter (not . isSpace)
ifeq env [x, y, t] = ifeq env [x, y, t, ""]
ifeq _ _ = arityError "ifeq" [3, 4]

ifne :: Macro
ifne env [x, y, t, e] = ifeq env [x, y, e, t]
ifne env [x, y, t] = ifeq env [x, y, "", t]
ifne _ _ = arityError "ifne" [3, 4]

rawdef :: Macro
rawdef env [name] = return (env, getSymbol env name)
rawdef _ _ = arityError "rawdef" [1]

include :: Macro
include env [name] = do name' <- locateFile env name
                        ppFile env name'
include _ _ = arityError "include" [1]

locateFile :: Env -> FilePath -> IO FilePath
locateFile env name = do
    let name' = case name of
                    ('~' : '/' : relname) -> getSymbol env (envTag++"HOME") </> relname
                    _ -> name
    let path = map (\p -> takeDirectory (getSymbol env p)) [currentTag, mainTag] ++ ["."]
    found <- findFile path name'
    case found of
        Just foundFile -> return foundFile
        Nothing -> fileNotFound name

raw :: Macro
raw env [src] = return (env, src)
raw _ _ = arityError "raw" [1]

rawinc :: Macro
rawinc env [name] = do name' <- locateFile env name
                       doc <- readFile name'
                       return (env, doc)
rawinc _ _ = arityError "rawinclude" [1]

rawexec :: Macro
rawexec env [cmd] = do (env', cmd') <- pp env cmd
                       doc <- readProcess "sh" ["-c", cmd'] ""
                       return (env', doc)
rawexec _ _ = arityError "rawexec" [1]

exec :: Macro
exec env [cmd] = do (doc, env') <- rawexec env [cmd]
                    pp doc env'
exec _ _ = arityError "exec" [1]

mdate :: Macro
mdate env files = do
    (env', files') <- ppAll env files
    files'' <- mapM (locateFile env) $ (concatMap words files') ++ [getSymbol env mainTag | null files]
    times <- mapM getModificationTime files''
    let lastTime = (last . sort) times
    return (env', formatTime (myLocale $ getSymbol env langTag) "%A %-d %B %Y" lastTime)

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
#ifdef linux_HOST_OS
                    intervals = [("an","ans"),("mois","mois"),("jour","jours")
                                ,("heure","heures"),("min","mins"),("sec","secs"),("usec","usecs")],
#endif
                    amPm = ("AM","PM"),
#ifdef mingw32_HOST_OS
                    knownTimeZones = [],
#endif
                    dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y",
                    dateFmt = "%d/%m/%y",
                    timeFmt = "%H:%M:%S",
                    time12Fmt = "%I:%M:%S %p"
                }
myLocale _ = defaultTimeLocale

ppAll :: Env -> [String] -> IO (Env, [String])
ppAll env [] = return (env, [])
ppAll env (x:xs) = do (env', x') <- pp env x
                      (env'', x's) <- ppAll env' xs
                      return (env'', x':x's)

readEnv :: Macro
readEnv env [name] = case lookup (envTag++name) env of
                    Just val -> pp env val
                    Nothing -> return (env, "")
readEnv _ _ = arityError "env" [1]

add :: Macro
add env [name, val] = do
    let val0 = getSymbol env name
    (env', val1) <- pp env val
    let env'' = (name, show (atoi val0 + atoi val1)) : env'
    return (env'', "")

add env [name] = add env [name, "1"]
add _ _ = arityError "add" [1, 2]

atoi :: String -> Integer
atoi s = case reads s of
            [(i, "")] -> i
            _ -> 0

language :: String -> Macro
language lang env [src] = case lookup langTag env of
                            Just val | val == lang -> pp env src
                            _ -> return (env, "")
language lang _ _ = arityError lang [1]

format :: String -> Macro
format fmt env [src] = case lookup formatTag env of
                            Just val | val == fmt -> pp env src
                            _ -> return (env, "")
format fmt _ _ = arityError fmt [1]

charsFunc :: Chars
charsFunc = ['!', '\\']

charsOpenClose :: [(Char, Char)]
charsOpenClose = [('(', ')'), ('{', '}'), ('[', ']')]

pp :: Prepro
pp env [] = return (env, "")
pp env (c0:cs)
    | c0 `elem` charsFunc && not (null name) = case lookup name builtin of
                Just func -> do let (args, cs'') = ppArgs cs'
                                (env', doc) <- func env args
                                (env'', doc') <- pp env' cs''
                                return (env'', doc++doc')
                Nothing -> case lookup name env of
                                Just value -> do let (args, cs'') = ppArgs cs'
                                                 let args' = zip (map show [(1::Int)..]) args
                                                 (env', doc) <- pp (args'++env) value
                                                 let env'' = env' \\ args'
                                                 (env''', doc') <- pp env'' cs''
                                                 return (env''', doc++doc')
                                Nothing -> do (env', doc) <- pp env cs'
                                              return (env', (c0:name)++doc)
    | otherwise = do (env', doc) <- pp env cs
                     return (env', c0:doc)
    where
        (name, cs') = span (\c -> isAlphaNum c || c == '_') cs

        ppArgs :: String -> ([String], String)
        ppArgs (c:s) = case lookup c charsOpenClose of
                        Just c1 -> (init arg : args, s'')
                                        where
                                            (arg, s') = ppArg c c1 1 s
                                            (args, s'') = ppArgs s'
                        Nothing -> ([], c:s)
        ppArgs s = ([], s)

        ppArg :: Char -> Char -> Int -> String -> (String, String)
        ppArg _ _ 0 s = ("", s)
        ppArg _ _ _ [] = unexpectedEndOfFile env
        ppArg left right level (c:s) = (c:s', s'')
            where
                (s', s'') = ppArg left right level' s
                level'  | c == left     = level + 1
                        | c == right    = level - 1
                        | otherwise     = level

getSymbol :: Env -> String -> String
getSymbol env name = case lookup name env of
                        Just val -> val
                        Nothing -> ""

unexpectedEndOfFile :: Env -> t
unexpectedEndOfFile env = error $ "Unexpected end of file in " ++ (getSymbol env currentTag)

fileNotFound :: String -> t
fileNotFound name = error $ "File not found: " ++ name

arityError :: String -> [Int] -> t
arityError name arities = error $ "Arity error: " ++ name ++ " expects " ++ nb ++ " argument" ++ s
    where
        (nb, s) = case arities of
                    [] -> ("no", "")
                    [0] -> ("no", "")
                    [1] -> ("1", "")
                    [0, 1] -> ("0 or 1", "")
                    [x] -> (show x, "s")
                    xs -> (intercalate ", " (map show (init xs)) ++ " or " ++ show (last xs), "s")
