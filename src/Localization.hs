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

module Localization ( Lang, langs
                    , myLocale
                    )
where

import Data.Time

-- language list
data Lang = En | Fr | It | Es
          deriving (Show, Read, Eq, Enum, Bounded)

langs :: [Lang]
langs = [(minBound :: Lang) ..]

-- "myLocale lang" returns the date format description for a given language.
myLocale :: Lang -> TimeLocale

-- french locale date format
myLocale Fr = TimeLocale {
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

-- italian locale date format
-- contributed by tajmone (https://github.com/tajmone)
myLocale It = TimeLocale {
                    wDays = [("domenica","dom")
                            ,("lunedì","lun")
                            ,("martedì","mar")
                            ,("mercoledì","mer")
                            ,("giovedì","gio")
                            ,("venerdì","ven")
                            ,("sabato","sab")],
                    months = [("gennaio","gen")
                             ,("febbraio","feb")
                             ,("marzo","mar")
                             ,("aprile","apr")
                             ,("maggio","mag")
                             ,("giugno","giu")
                             ,("luglio","lug")
                             ,("agosto","ago")
                             ,("settembre","set")
                             ,("ottobre","ott")
                             ,("novembre","nov")
                             ,("dicembre","dic")],
                    amPm = ("AM","PM"),
                    knownTimeZones = [],
                    dateTimeFmt = "%a %e %b %Y, %H:%M:%S %Z",
                    dateFmt = "%d/%m/%y",
                    timeFmt = "%H:%M:%S",
                    time12Fmt = "%I:%M:%S %p"
              }

-- spanish locale date format
-- contributed by conradolandia (https://github.com/conradolandia)
myLocale Es = TimeLocale {
                    wDays = [("domingo","dom")
                            ,("lunes","lun")
                            ,("martes","mar")
                            ,("miércoles","mie")
                            ,("jueves","jue")
                            ,("viernes","vie")
                            ,("sábado","sab")],
                    months = [("enero","ene")
                             ,("febrero","feb")
                             ,("marzo","mar")
                             ,("abril","abr")
                             ,("mayo","may")
                             ,("junio","jun")
                             ,("julio","jul")
                             ,("agosto","ago")
                             ,("septiembre","sep")
                             ,("octubre","oct")
                             ,("noviembre","nov")
                             ,("diciembre","dic")],
                    amPm = ("AM","PM"),
                    knownTimeZones = [],
                    dateTimeFmt = "%a %e %b %Y, %H:%M:%S %Z",
                    dateFmt = "%d/%m/%y",
                    timeFmt = "%H:%M:%S",
                    time12Fmt = "%I:%M:%S %p"
              }

-- English is the default locale
myLocale En = defaultTimeLocale

