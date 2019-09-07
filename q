module Daten where

import Data.Time.Calendar
import Data.Time.Format

type DatenDate = (Integer, Int, Int)
type DatenFormat = String

data Daten = Daten DatenDate DatenDate DatenFormat 

getDates (Daten start@(sy, sm, sd) end@(ey, em, ed) tpl) = map show . takeWhile (<= endDay) . map ((flip addDays) startDay) $ [0..]
  where
    startDay = fromGregorian sy sm sd
    endDay = fromGregorian ey em ed 
