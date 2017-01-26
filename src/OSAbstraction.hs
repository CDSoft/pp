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

module OSAbstraction where

#if linux_HOST_OS || darwin_HOST_OS
#else
import Data.Char
#endif

-- shell command interpretor for Windows .bat scripts
cmdexe :: String
cmdexe =
#if linux_HOST_OS || darwin_HOST_OS
    "wine cmd /c"
#else
    "cmd /c"
#endif

-- environment variable storage (should be case-insensitive on Windows)
envVarStorage :: String -> String
#if linux_HOST_OS || darwin_HOST_OS
envVarStorage = id
#else
envVarStorage = map toUpper
#endif
