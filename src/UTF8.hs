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

module UTF8 ( setUTF8Encoding
            , readFileUTF8
            , writeFileUTF8
            , hWriteFileUTF8
            , runProcessUTF8
            , readProcessUTF8
            )
where

import Control.Monad
import System.IO
import System.Exit
import qualified System.IO.Strict as SIO
import System.Process

-- setUTF8Encoding sets UTF8 as the current encoding of a file handle
setUTF8Encoding :: Handle -> IO ()
setUTF8Encoding h = hSetEncoding h utf8

-- "readFileUTF8 name" reads an UTF-8 file.
-- If name is "-", it reads stdin.
readFileUTF8 :: FilePath -> IO String
readFileUTF8 "-" = getContents
readFileUTF8 name = do
    h <- openFile name ReadMode
    hSetEncoding h utf8
    -- the file must not be read lazily
    -- (in some case we want to be able to read files
    -- that have been previously produced by the same document)
    content <- SIO.hGetContents h
    hClose h
    return content

-- "writeFileUTF8 name content" writes an UTF-8 file.
-- If name is "-", it writes to stdout.
writeFileUTF8 :: FilePath -> String -> IO ()
writeFileUTF8 "-" content = putStr content
writeFileUTF8 name content = do
    handle <- openBinaryFile name WriteMode
    hWriteFileUTF8 handle content
    hClose handle

-- "hwriteFileUTF8 handle content" writes an UTF-8 file.
hWriteFileUTF8 :: Handle -> String -> IO ()
hWriteFileUTF8 handle content = do
    hSetEncoding handle utf8
    hPutStr handle content

-- "runrocessUTF8 cmd arg" executes "cmd args"
runProcessUTF8 :: String -> [String] -> IO ()
runProcessUTF8 cmd args = void $ readProcessUTF8 cmd args

-- "readProcessUTF8 cmd arg" executes "cmd args"
-- and returns the standard output produced by the command.
readProcessUTF8 :: String -> [String] -> IO String
readProcessUTF8 cmd args = do
    (_, Just hOut, _, hProc) <- createProcess (proc cmd args) { std_out = CreatePipe }
    hSetEncoding hOut utf8
    out <- SIO.hGetContents hOut
    code <- waitForProcess hProc
    case code of
        ExitSuccess -> return out
        ExitFailure _ -> exitWith code
