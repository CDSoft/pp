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
import System.Process(callProcess, readProcess)
import System.FilePath
import Data.Time
import Foreign.C.String

#ifdef linux_HOST_OS
import System.Posix.Process
#else
--import System.Win32.Process
#endif

type Chars = String
type Env = [(String, String)]
type Macro = Env -> [String] -> IO (Env, String)
type Prepro = Env -> String ->  IO (Env, String)

data DiagramRuntime = Graphviz | PlantUML | Ditaa

main :: IO ()
main = do
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    args <- getArgs
    env <- getEnvironment
    let lang = case lookup "LANG" env of
                    Just ('C':_) -> "en"
                    Just val -> map toLower $ take 2 val
                    Nothing -> ""
    let fmt = map toLower $ fromMaybe "" (lookup "FORMAT" env)
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

doArg env ('-':'D':def) = return ((n, drop 1 v) : env, "")
    where (n,v) = span (/='=') def

doArg env ('-':'U':name) = return ([(n,v) | (n,v)<-env, n/=name], "")

doArg _ ('-':arg) | not (null arg) = error $ "Unexpected argument: " ++ arg

doArg env name = ppFile ((mainTag, name) : env) name

ppFile :: Env -> FilePath -> IO (Env, String)
ppFile env name = do
    let caller = getSymbol env currentTag
    content <- readFileUTF8 name
    (env', doc) <- pp ((currentTag, name) : env) content
    return ((currentTag, caller) : env', doc)

readFileUTF8 :: FilePath -> IO String
readFileUTF8 "-" = getContents
readFileUTF8 name = do
    h <- openFile name ReadMode
    hSetEncoding h utf8
    SIO.hGetContents h

writeFileUTF8 :: FilePath -> String -> IO ()
writeFileUTF8 "-" content = putStr content
writeFileUTF8 name content = do
    h <- openFile name WriteMode
    hSetEncoding h utf8
    hPutStr h content
    hClose h

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

          , ("add", add)

          ]
          ++ [ (diag, diagram Graphviz diag "" "") | diag <- graphvizDiagrams]
          ++ [ (diag, diagram PlantUML diag "@startuml" "@enduml") | diag <- plantumlDiagrams]
          ++ [ (diag, diagram Ditaa diag "" "") | diag <- ditaaDiagrams]
          ++ [ ("sh", script "sh" "sh" "" ".sh")
             , ("bash", script "bash" "bash" "" ".sh")
#ifdef linux_HOST_OS
             , ("bat", script "bat" "wine cmd /c" "@echo off" ".bat")
#else
             , ("bat", script "bat" "cmd /c" "@echo off" ".bat")
#endif
             , ("python", script "python" "python" "" ".py")
             , ("haskell", script "haskell" "runhaskell" "" ".hs")
          ]
          ++ [ (lang, language lang) | lang <- langs]
          ++ [ (fmt, format fmt) | fmt <- formats]

define :: Macro
define env [name, value] = do
    name' <- pp' env name
    return ((name', value) : env, "")
define env [name] = define env [name, ""]
define _ _ = arityError "define" [1,2]

undefine :: Macro
undefine env [name] = do
    name' <- pp' env name
    return ([ (n,v) | (n,v) <- env, n/=name' ], "")
undefine _ _ = arityError "undefine" [1]

ifdef :: Macro
ifdef env [name, t, e] = do
    name' <- pp' env name
    case lookup name' env of
        Just _ -> pp env t
        Nothing -> pp env e
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
    name' <- pp' env name
    return (env, getSymbol env name')
rawdef _ _ = arityError "rawdef" [1]

include :: Macro
include env [name] = pp' env name >>= locateFile env >>= ppFile env
include _ _ = arityError "include" [1]

locateFile :: Env -> FilePath -> IO FilePath
locateFile env name = do
    let name' = case name of
                    ('~' : '/' : relname) -> getSymbol env (envTag++"HOME") </> relname
                    _ -> name
    let path = map (takeDirectory . getSymbol env) [currentTag, mainTag] ++ ["."]
    found <- findFile path name'
    case found of
        Just foundFile -> return foundFile
        Nothing -> fileNotFound name

raw :: Macro
raw env [src] = return (env, src)
raw _ _ = arityError "raw" [1]

rawinc :: Macro
rawinc env [name] = do
    doc <- pp' env name >>= locateFile env >>= readFileUTF8
    return (env, doc)
rawinc _ _ = arityError "rawinclude" [1]

rawexec :: Macro
rawexec env [cmd] = do
    cmd' <- pp' env cmd
    doc <- readProcess "sh" ["-c", cmd'] ""
    return (env, strip doc)
rawexec _ _ = arityError "rawexec" [1]

exec :: Macro
exec env [cmd] = do
    (env', doc) <- rawexec env [cmd]
    pp env' doc
exec _ _ = arityError "exec" [1]

strip :: String -> String
strip = strip' . strip' where strip' = dropWhile isSpace . reverse

mdate :: Macro
mdate env files = do
    files' <- ppAll' env files
    files'' <- mapM (locateFile env) $ concatMap words files' ++ [getSymbol env mainTag | null files]
    times <- mapM getModificationTime files''
    let lastTime = maximum times
    return (env, formatTime (myLocale $ getSymbol env langTag) "%A %-d %B %Y" lastTime)

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
ppAll' env xs = do
    (_, xs') <- ppAll env xs
    return xs'

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
    name' <- pp' env name
    let val0 = getSymbol env name'
    val1 <- pp' env val
    let env' = (name', show (atoi val0 + atoi val1)) : env
    return (env', "")

add env [name] = add env [name, "1"]
add _ _ = arityError "add" [1, 2]

atoi :: String -> Integer
atoi s = case reads s of
            [(i, "")] -> i
            _ -> 0

diagram :: DiagramRuntime -> String -> String -> String -> Macro
diagram runtime diag header footer env [path, title, code] = do
    path' <- pp' env $ strip path
    title' <- pp' env $ strip title
    code' <- pp' env code
    let code'' = unlines [header, code', footer]
    let (gv, img, url) = splitImgUrl path' True
    oldCodeExists <- doesFileExist gv
    oldCode <- if oldCodeExists then readFileUTF8 gv else return ""
    when (code'' /= oldCode) $ do
        writeFileUTF8 gv code''
        case runtime of
            Graphviz -> callProcess diag ["-Tpng", "-o", img, gv]
            PlantUML -> do
                plantuml <- resource "plantuml.jar" plantumlJar
                callProcess "java" ["-jar", plantuml, "-charset", "UTF-8", gv]
            Ditaa -> do ditaa <- resource "ditaa.jar" ditaaJar
                        callProcess "java" ["-jar", ditaa, "-e", "UTF-8", "-o", gv, img]
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
    putStrLn $ "resource " ++ name
    tmp <- getTemporaryDirectory
    let path = tmp </> name
    putStrLn $ "path = " ++ path
    alreadyExists <- doesFileExist path
    unless alreadyExists $ writeFile path =<< peekCStringLen content
    putStrLn "ok"
    return path

script :: String -> String -> String -> String -> Macro
script _lang cmd header ext env [src] = do
    tmp <- getTemporaryDirectory
#ifdef linux_HOST_OS
    pid <- getProcessID
#else
    pid <- getCurrentProcessId
#endif
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
foreign import stdcall "GetCurrentProcessId" getCurrentProcessId :: IO Int
#endif

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

pp' :: Env -> String -> IO String
pp' env s = do
    (_, s') <- pp env s
    return s'

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
getSymbol env name = fromMaybe "" (lookup name env)

unexpectedEndOfFile :: Env -> t
unexpectedEndOfFile env = error $ "Unexpected end of file in " ++ getSymbol env currentTag

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
