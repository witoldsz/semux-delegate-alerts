module Discord (publish) where

import ClassyPrelude
import Data.Aeson
import Data.ByteString.Lazy
import Network.HTTP.Simple
import Data.String (fromString)
import Control.Monad (void)

publish :: String -> Text -> IO ()
publish webhookUrl message = do

  let request = setRequestMethod "POST"
       $ setRequestHeader "Content-Type" ["application/json"]
       $ setRequestBodyJSON (object [ "content" .= message ])
       $ fromString webhookUrl

  void $ httpNoBody request
