{- PP

Copyright (C) 2015, 2016, 2017, 2018 Christophe Delord

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

module Formats ( Dialect(..), dialects
               , Format(..), formats
               , Diagram(..)
               , GraphvizDiagram(..), graphvizDiagrams
               , PlantumlDiagram(..), plantumlDiagrams
               , AsymptoteDiagram(..), asymptoteDiagrams
               , RDiagram(..), rDiagrams
               , readCap
               , readCapMaybe
               , showCap
               , showCapMaybe
               )

where

import Data.Char

-- format list
data Format = Html | Pdf | Odf | Epub | Mobi
              deriving (Show, Read, Eq, Enum, Bounded)

formats :: [Format]
formats = [(minBound :: Format) .. ]

-- dialect list
data Dialect = Md | Rst
             deriving (Show, Read, Eq, Enum, Bounded)

dialects :: [Dialect]
dialects = [(minBound :: Dialect) .. ]

-- Diagrams
data Diagram = GraphvizDiagram | PlantumlDiagram | AsymptoteDiagram | RDiagram
data GraphvizDiagram = Dot | Neato | Twopi | Circo | Fdp | Sfdp | Patchwork | Osage
                     deriving (Show, Read, Enum, Bounded)
data PlantumlDiagram = Uml | Ditaa
                     deriving (Show, Read, Enum, Bounded)
data AsymptoteDiagram = Asy
                      deriving (Show, Read, Enum, Bounded)
data RDiagram = Rplot
                deriving (Show, Read, Enum, Bounded)

graphvizDiagrams :: [GraphvizDiagram]
graphvizDiagrams = [(minBound :: GraphvizDiagram) .. ]

plantumlDiagrams :: [PlantumlDiagram]
plantumlDiagrams = [(minBound :: PlantumlDiagram) .. ]

asymptoteDiagrams :: [AsymptoteDiagram]
asymptoteDiagrams = [(minBound :: AsymptoteDiagram) .. ]

rDiagrams :: [RDiagram]
rDiagrams = [(minBound :: RDiagram) .. ]

-- Convert a string to a Haskell type

readCap :: Read a => String -> Maybe a
readCap (c:s) = case reads (toUpper c : map toLower s) of
    [(x, "")] -> Just x
    _ -> Nothing
readCap [] = Nothing

readCapMaybe :: Read a => Maybe String -> Maybe a
readCapMaybe (Just s) = readCap s
readCapMaybe Nothing = Nothing

-- Convert a Haskell type to a string

showCap :: Show a => a -> String
showCap = map toLower . show

showCapMaybe :: Show a => String -> Maybe a -> String
showCapMaybe _ (Just x) = showCap x
showCapMaybe def Nothing = def
