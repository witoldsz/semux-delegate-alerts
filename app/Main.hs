{-# LANGUAGE FlexibleInstances #-}

module Main where

import ClassyPrelude
import Text.Read
import Semux
import Db
import Discord (publish)
import System.Environment
import Data.Time.Clock
import Control.Monad (when)

main :: IO ()
main = do
  semuxApi       <-          getEnv "SEMUX_API"
  webhookUrl     <-          getEnv "WEBHOOK_URL"
  delegate       <- pack <$> getEnv "DELEGATE"
  alertAfterSecs <- read <$> getEnv "ALERT_AFTER_SECS" :: IO Int

  now              <- getCurrentTime
  lastCoinbase     <- getLastCoinbase semuxApi delegate
  let diff         =  now `minus` lastCoinbase
  let isNotForging =  diff > alertAfterSecs

  notForgingDelegates <- readNotForgingDelegates
  let wasNotForging   =  delegate `elem` notForgingDelegates

  (delegateDescr, _) <- getDelegateInfo semuxApi delegate
  -- TODO: new feature: publish warning when rank is too low

  case (isNotForging, wasNotForging) of

    (True, False) -> do
      let msg =
            delegateDescr
              <> " hasn't forged since `" <> tshow lastCoinbase <> "`"
              <> " (" <> tshow diff <> "s ago)."
      publish webhookUrl msg
      putStrLn msg
      writeNotForgingDelegates (delegate : notForgingDelegates)

    (False, True) -> do
      let msg =
            delegateDescr <> " is OK, has forged " <> tshow diff <> "s ago."
      publish webhookUrl msg
      putStrLn msg
      writeNotForgingDelegates $ filter (/= delegate) notForgingDelegates

    _ ->
      putStrLn $ delegateDescr
        <> " COINBASE=" <> tshow lastCoinbase
        <> " " <> tshow diff <> "s"

minus :: UTCTime -> UTCTime -> Int
minus t1 t2 =
  fst $ properFraction $ diffUTCTime t1 t2
