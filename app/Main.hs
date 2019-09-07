module Main where

import           Data.Semigroup      ((<>))
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Daten               as D
import           Options.Applicative

data AppOpts = AppOpts
  { start   :: String
  , end     :: String
  , format  :: String
  , joinStr :: String
  } deriving (Show)

startParser :: Parser String
startParser =
  strOption
    (long "start" <> short 's' <>
     help "Start date with format 'yyyy-mm-dd' or today" <>
     showDefault <>
     value "today" <>
     metavar "START")

endParser :: Parser String
endParser =
  strOption
    (long "end" <> short 'e' <>
     help "End date with format 'yyyy-mm-dd' or today" <>
     showDefault <>
     value "today" <>
     metavar "END")

formatParser :: Parser String
formatParser =
  strOption
    (long "format" <> short 'f' <> help "format to print" <> showDefault <>
     value "%Y-%m-%d" <>
     metavar "FORMAT")

joinStrParser :: Parser String
joinStrParser =
  strOption
    (long "join_str" <> short 'j' <> help "Join string" <> showDefault <>
     value "','" <>
     metavar "JOIN_STR")

--appOpts :: Parser AppOpts
appOpts =
  AppOpts <$> startParser <*> endParser <*> formatParser <*> joinStrParser

main :: IO ()
main = do
  o <- execParser opts
  t <- getCurrentTime
  let s = todayOr (start o) t
  let e = todayOr (end o) t
  putStrLn $ D.join (joinStr o) (D.Daten s e (format o))
  where
    utcToGregorian = toGregorian . utctDay
    strToGregorian s =
      (read . take 4 $ s, read . take 2 . drop 5 $ s, read . drop 8 $ s)
    todayOr x t =
      if x == "today"
        then utcToGregorian t
        else strToGregorian x
    opts =
      info
        (appOpts <**> helper)
        (fullDesc <> progDesc "Upgrade for TARGET" <>
         header "sysup - a command for upgrade")
