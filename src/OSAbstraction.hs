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

{-# LANGUAGE CPP #-}

module OSAbstraction ( osname
                     , osarch
                     , cmdexe
#if mingw32_HOST_OS
                     , powershellexe
#endif
                     , envVarStorage
                     )
where

import System.Info

#if mingw32_HOST_OS
import Data.Char
#endif

-- OS name
osname :: String
#if mingw32_HOST_OS
osname = "windows"
#endif
#if linux_HOST_OS || darwin_HOST_OS
osname = os
#endif

-- machine architecture
osarch :: String
osarch = arch

-- shell command interpretor for Windows .bat scripts
cmdexe :: String
#if linux_HOST_OS || darwin_HOST_OS
cmdexe = "wine cmd /c"
#endif
#if mingw32_HOST_OS
cmdexe = "cmd /c"
#endif

#if mingw32_HOST_OS
-- powershell command interpretor for Windows .ps1 scripts
powershellexe :: String
powershellexe =
    "powershell -File"
#endif

-- environment variable storage (should be case-insensitive on Windows)
-- Thanks to tajmone (https://github.com/tajmone) for pointing out this difference.
envVarStorage :: String -> String
#if linux_HOST_OS || darwin_HOST_OS
envVarStorage = id
#endif
#if mingw32_HOST_OS
envVarStorage = map toUpper
#endif
