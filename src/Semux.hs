{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Semux (getDelegateInfo, getLastCoinbase) where

import ClassyPrelude
import Data.Time
import Data.List (findIndex)
import Data.Maybe
import Data.Aeson
import GHC.Generics (Generic)
import Network.HTTP.Simple

pageSize :: Int
pageSize = 100

getDelegateInfo :: String -> Text -> IO (Text, Int)
getDelegateInfo semuxApi delegate = do
  ds <- result <$> getDelegates semuxApi
  return
    $ fromMaybe (delegate, 0)
      $ info <$> find byAddress ds <*> ((1 +) <$> findIndex byAddress ds)
  where
    byAddress :: Delegate -> Bool
    byAddress d =
      _dAddress d == delegate

    info :: Delegate -> Int -> (Text, Int)
    info d rank =
      ( _dAddress d <> " " <> _dName d <> " #" <> tshow rank
      , rank
      )


getLastCoinbase :: String -> Text -> IO UTCTime
getLastCoinbase semuxApi delegate = do
  txCount <- transactionCount . result <$> getAccount semuxApi delegate
  timestamp <$> findLastCoinbase txCount

  where
    findLastCoinbase :: Int -> IO Transaction
    findLastCoinbase lastTx = do
      response <- getTransactions semuxApi delegate txRange
      let txsRev = (reverse . result) response
      let lastCoinbase = find (\i -> transactionType i == "COINBASE") txsRev
      case lastCoinbase of
        Just coinbase -> return coinbase
        Nothing
          | from > 0  -> findLastCoinbase from
          | otherwise -> fail "No COINBASE found"

      where
        from = max 0 (lastTx - pageSize)
        txRange = (from, lastTx)

-- Delegate
data Delegate = Delegate
  { _dAddress :: !Text
  , _dName :: !Text
  , _dValidator :: !Bool
  }

instance FromJSON Delegate where
  parseJSON = withObject "Delegate" $ \v -> Delegate
    <$> v .: "address"
    <*> v .: "name"
    <*> v .: "validator"

getDelegates :: String -> IO (ApiResponse [Delegate])
getDelegates semuxApi =
  getResponseBody <$> httpJSON (parseRequest_ $ semuxApi ++ "delegates")

-- Account
data Account = Account
  { transactionCount :: Int
  } deriving (Show, Generic)

instance FromJSON Account

getAccount :: String -> Text -> IO (ApiResponse Account)
getAccount semuxApi addr =
  getResponseBody <$> httpJSON (parseRequest_ url)
  where
    url = semuxApi <> "account?address=" <> unpack addr

-- Transaction
data Transaction = Transaction
 { timestamp :: UTCTime
 , transactionType :: !Text
 } deriving (Show)

instance FromJSON Transaction where
  parseJSON = withObject "Transaction" $ \v -> Transaction
    <$> fmap textToUTC (v .: "timestamp")
    <*> v .: "type"

getTransactions :: String -> Text -> (Int, Int) -> IO (ApiResponse [Transaction])
getTransactions semuxApi addr (from, to) =
  getResponseBody <$> httpJSON (parseRequest_ url)
  where
    url =
      semuxApi ++ "account/transactions?address="
        ++ unpack addr ++ "&from=" ++ show from ++ "&to=" ++ show to

-- Response
data ApiResponse a = Response
     { success :: Bool
     , message :: !Text
     , result :: a
     } deriving (Show, Generic)

instance FromJSON (ApiResponse [Transaction])
instance FromJSON (ApiResponse Account)
instance FromJSON (ApiResponse [Delegate])

-- UTCTime
textToUTC :: Text -> UTCTime
textToUTC = fromJust . parseTimeM False defaultTimeLocale "%s" . unpack . take 10
