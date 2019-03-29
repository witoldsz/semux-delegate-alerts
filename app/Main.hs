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
  alertRank      <- read <$> getEnv "ALERT_RANK" :: IO Int

  (delegateDescr, rank) <- getDelegateInfo semuxApi delegate

  lowRankDelegates <- readLowRankDelegates
  let wasLowRank = delegate `elem` lowRankDelegates
  let isLowRank = rank >= alertRank

  case (isLowRank, wasLowRank) of

    (True, False) -> do
      let msg = delegateDescr <> " has low rank (≥ " <> tshow alertRank <> "). Get some votes!"
      publish webhookUrl msg
      putStrLn msg
      writeLowRankDelegates (delegate : lowRankDelegates)

    (False, True) -> do
      let msg = delegateDescr <> " has rank OK now (< " <> tshow alertRank <> ")."
      publish webhookUrl msg
      putStrLn msg
      writeLowRankDelegates $ filter (/= delegate) lowRankDelegates

    _ -> return ()

  notForgingDelegates <- readNotForgingDelegates
  let wasNotForging   =  delegate `elem` notForgingDelegates
  now                 <- getCurrentTime
  lastCoinbase        <- getLastCoinbase semuxApi delegate
  let diff            =  now `minus` lastCoinbase
  let isNotForging    =  diff > alertAfterSecs

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
