#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package bytestring
  --package filepath
  --package split
-}

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

{- This script reimplements xxd to remove a dependency.
 - It creates a C array with the content of the input file.
 -}

import Data.Char
import Data.List.Split
import System.Environment
import System.Exit
import System.FilePath

import qualified Data.ByteString as B

main :: IO ()
main = do
    args <- getArgs
    case args of
        [blobName] -> do
            let cvar = cVarname blobName
            let hsvar = hsVarname blobName
            let cmod = cFilename blobName
            let hsmod = hsFilename blobName

            putStrLn $ "Blob file: " ++ blobName
            blob <- B.readFile blobName
            let blobLen = B.length blob
            putStrLn $ "size     : " ++ show blobLen ++ " bytes"

            putStrLn $ "output   : " ++ cmod
            writeFile cmod $ unlines
                [   "/* generated from " ++ takeFileName blobName ++ ". Do not modify. */"
                ,   ""
                ,   "unsigned char " ++ cvar ++ "[] = {"
                ,   mkBlob blob
                ,   "};"
                ,   ""
                ,   "unsigned int " ++ cvar ++ "_len = " ++ show blobLen ++ ";"
                ]

            putStrLn $ "output   : " ++ hsmod
            writeFile hsmod $ unlines
                [   "{- generated from " ++ takeFileName blobName ++ ". Do not modify. -}"
                ,   ""
                ,   "module " ++ dropExtension (takeFileName hsmod)
                ,   "where"
                ,   ""
                ,   "import Foreign.C.Types"
                ,   "import Foreign.Ptr"
                ,   ""
                ,   "foreign import ccall \"&" ++ cvar ++ "\"     _" ++ cvar ++ "     :: Ptr CChar"
                ,   "foreign import ccall \"&" ++ cvar ++ "_len\" _" ++ cvar ++ "_len :: Ptr CInt"
                ,   ""
                ,   hsvar ++ " :: (Ptr CChar, Ptr CInt)"
                ,   hsvar ++ " = (_" ++ cvar ++ ", _" ++ cvar ++ "_len)"
                ]

        _ -> putStrLn "usage: hsblob <blob filename>" >> exitFailure

cVarname :: FilePath -> String
cVarname = map tr . takeFileName
    where
        tr c | isAlphaNum c = toLower c
             | otherwise = '_'

hsVarname :: FilePath -> String
hsVarname = lowerCamelCase . takeFileName

filename :: FilePath -> FilePath
filename name = dirname </> upperCamelCase basename
    where
        (dirname, basename) = splitFileName name

cFilename :: FilePath -> FilePath
cFilename = (<.> "c") . (++ "_c") . filename

hsFilename :: FilePath -> FilePath
hsFilename = (<.> "hs") . filename

lowerCamelCase :: String -> String
lowerCamelCase = lower . dropNonLetters

upperCamelCase :: String -> String
upperCamelCase = capitalize . dropNonLetters

dropNonLetters :: String -> String
dropNonLetters = dropWhile (not . isLetter)

capitalize :: String -> String
capitalize (c:cs) = toUpper c : lower cs
capitalize [] = []

lower :: String -> String
lower (c:cs) | isAlphaNum c = toLower c : lower cs
             | otherwise    = capitalize $ dropNonLetters cs
lower [] = []

mkBlob :: B.ByteString -> String
mkBlob blob = unlines $ map concat bytes
    where
        bytes = map (join ",") $ chunksOf 32 $ B.unpack blob
        join sep = map $ (++ sep) . show
