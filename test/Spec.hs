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

import Test.Hspec

import Localization
import Preprocessor

import Data.List
import Data.Time

main :: IO ()
main = hspec $ do

    describe "Localization.myLocale" $ do
        it "defines date format for every language in langs" $ do
            head (wDays $ myLocale "fr") `shouldBe` ("Dimanche", "Dim")
            head (wDays $ myLocale "it") `shouldBe` ("domenica","dom")
            head (wDays $ myLocale "es") `shouldBe` ("domingo","dom")
            head (wDays $ myLocale "en") `shouldBe` ("Sunday","Sun")
            let locales = map myLocale langs
            nub locales `shouldBe` locales -- defined for all languages

    describe "Preprocessor" $ do
        it "defines disjoint character sets" $ do
            let sets = concat [ charsFunc, charsBlock, [litMacroTagChar] ]
            nub sets `shouldBe` sets
        it "defines disjoint macro name sets" $ do
            let sets = concat [ langs
                              , formats
                              , dialects
                              , graphvizDiagrams
                              , plantumlDiagrams
                              , ditaaDiagrams
                              ]
            nub sets `shouldBe` sets
            let builtinNames = map fst builtin
            nub builtinNames `shouldBe` builtinNames
