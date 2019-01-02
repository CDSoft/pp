{- PP

Copyright (C) 2015-2019 Christophe Delord

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

import Test.Hspec

import Environment
import Preprocessor

import Data.List
import Control.Monad

main :: IO ()
main = hspec $ do

    describe "Preprocessor" $ do
        it "defines disjoint character sets" $ do
            let sets = defaultMacroChars
                       ++ concat [ [o,c] | (o,c) <- defaultOpenCloseChars ]
                       ++ defaultBlockChars
                       ++ defaultLiterateMacroChars
            nub sets `shouldBe` sets
        it "defines at least one macro call char" $ defaultMacroChars `shouldNotBe` []
        it "defines at least one kind of parentheses" $ defaultOpenCloseChars `shouldNotBe` []
        it "defines at least one kind of block delimiter" $ defaultBlockChars `shouldNotBe` []
        it "defines at least one literate macro char" $ defaultLiterateMacroChars `shouldNotBe` []

    describe "Documentation" $
        forM_ builtin $ \(Macro name _ doc _) -> do
            it ("the docstring of '!"++name++"' starts with `!") $ take 2 doc `shouldBe` "`!"
            it ("the docstring of '!"++name++"' ends with .") $ last doc `shouldBe` '.'
