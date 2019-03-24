module Db where

import ClassyPrelude
import Network.HTTP.Simple
import Data.String (fromString)
import Control.Monad (void)

readNotForgingDelegates :: IO [Text]
readNotForgingDelegates =
  lines <$> readFileUtf8 "db_NotForgingDelegates.txt"

writeNotForgingDelegates :: [Text] -> IO ()
writeNotForgingDelegates =
  writeFileUtf8 "db_NotForgingDelegates.txt" . unlines
