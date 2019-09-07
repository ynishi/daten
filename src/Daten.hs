module Daten
  ( DatenDate
  , DatenFormat
  , Daten(..)
  , toFormatteds
  , join
  ) where

import           Data.List
import           Data.Time.Calendar
import           Data.Time.Format

type DatenDate = (Integer, Int, Int)

type DatenFormat = String

data Daten =
  Daten DatenDate
        DatenDate
        DatenFormat
  deriving (Show)

toFormatteds d@(Daten start end tpl) = map showWithFormat $ dates start end
  where
    showWithFormat = formatTime defaultTimeLocale tpl

dates (sy, sm, sd) (ey, em, ed) =
  takeWhile (<= endDay) . map addDaysToStart $ [0 ..]
  where
    startDay = fromGregorian sy sm sd
    endDay = fromGregorian ey em ed
    addDays' = flip addDays
    addDaysToStart = addDays' startDay

join x d = "'" ++ intercalate x (toFormatteds d) ++ "'"
