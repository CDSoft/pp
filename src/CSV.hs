{- PP

Copyright (C) 2015, 2016, 2017, 2018 Christophe Delord

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

module CSV (makeTable)
where

import Control.Monad.Exception.Asynchronous.Lazy (Exceptional(..))
import Data.Char
import Data.List
import Data.Spreadsheet

import Formats

-- Type of cells
data Kind = Number | Other deriving (Eq)

-- Alignment of columns
data Alignment = LeftAligned Int | RightAligned Int deriving (Show, Eq)

-- List of possible cell separators
separators :: String
separators = ",;\t|"

-- parseCSV parses a CSV string.
-- The header `header` is used as the table header.
-- If header is Nothing, the first line of the CSV data is used as a header.
-- It returns the alignments of the columns and the table itself.
-- The cells are left or right justified.
parseCSV :: Maybe [String] -> String -> ([Alignment], [[String]])
parseCSV header csvData = (columnFormats, table)
    where
        -- CSV parser
        qm = '"'
        sep = inferSeparator csvData
        rows = case fromString qm sep csvData of
                    Exceptional{exception=Nothing, result=rs} -> joinMultiLines rs
                    Exceptional{exception=Just exc} -> errorWithoutStackTrace exc
        -- Header
        rows' = case header of
            Nothing -> rows
            Just h -> h : rows
        -- Line padding (each line must have the same length)
        maxRowLen = maximum $ map length rows'
        paddedRows = [ row ++ replicate (maxRowLen - length row) "" | row <- rows' ]
        -- Left/right alignment according to the kind of data
        columnFormats = map columnFormat $ transpose paddedRows
        table = align paddedRows columnFormats

-- Infers the most probable separator according to the frequencies of characters
inferSeparator :: String -> Char
inferSeparator csvData = snd $ maximum freqs
    where
        count c = length $ filter (==c) csvData
        freqs = zip (map count separators) separators

-- a cell can be a number or anything else
cellKind :: String -> Kind
cellKind cell = case (reads cell :: [(Double, String)]) of
    [_] -> Number
    _ -> Other

-- a column is right justified if it contains more numbers than non-numbers
columnFormat :: [String] -> Alignment
columnFormat cells
    | length lefts > length rights = LeftAligned w
    | otherwise = RightAligned w
    where
        fmts = map cellKind cells
        (rights, lefts) = partition (==Number) fmts
        w = maximum $ map length cells

-- add left or right padding to each cell according to the column alignment
align :: [[String]] -> [Alignment] -> [[String]]
align rows columnFormats = map alignRow rows
    where
        alignRow = zipWith alignCell columnFormats
        alignCell (LeftAligned w) cell = ' ' : cell' ++ replicate (w - length cell') ' ' ++ " "
            where cell' = strip cell
        alignCell (RightAligned w) cell = ' ' : replicate (w - length cell') ' ' ++ cell' ++ " "
            where cell' = strip cell
        strip = halfStrip . halfStrip where halfStrip = dropWhile isSpace . reverse

-- remove new lines in cells
joinMultiLines :: [[String]] -> [[String]]
joinMultiLines = map (map (map join . filter (/= '\r')))
    where join '\n' = ' '
          join c = c

-- generates a markdown or reStructuredText table
makeTable :: Dialect -> Maybe [String] -> String -> String
makeTable dialect header csvData = unlines $
    [ sepLine
    , makeLine headerRow
    , headerSepLine
    ] ++ concat [ [makeLine row, sepLine] | row <- rows ]
    where
        (columnFormats, table) = parseCSV header csvData
        (headerRow : rows) = table

        sepLine = concat ["+", intercalate "+" (map makeSep columnFormats), "+"]
        headerSepLine = concat ["+", intercalate "+" (map makeBigSep columnFormats), "+"]

        makeSep (LeftAligned w) = replicate (w+2) '-'
        makeSep (RightAligned w) = replicate (w+2) '-'

        makeBigSep (LeftAligned w) = alignmentChar ++ replicate (w+1) '='
        makeBigSep (RightAligned w) = replicate (w+1) '=' ++ alignmentChar

        alignmentChar = case dialect of
            Md -> ":"
            Rst -> "="

        makeLine row = concat [ "|", intercalate "|" row, "|" ]
        
