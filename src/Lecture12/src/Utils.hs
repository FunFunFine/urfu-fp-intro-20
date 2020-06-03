{-# LANGUAGE DeriveAnyClass #-}

module Utils where

import Control.Monad.Except
import Data.Aeson (ToJSON, encode)
import Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Types (hContentType)
import Servant.Server

throwJSONError :: (MonadError ServerError m, ToJSON a) => ServerError -> a -> m b
throwJSONError err json = throwError $ err
  { errBody = encode json
  , errHeaders = [ jsonHeader ]
  }
  where
    jsonHeader =
      ( hContentType
      , "application/json;charset=utf-8" )

data JSONError = JSONError
  { error :: Text
  } deriving (Generic, ToJSON)

toServerError :: String -> ServerError
toServerError e = ServerError
  { errHTTPCode = 500
  , errReasonPhrase = ""
  , errBody = encode $ JSONError $ T.pack e
  , errHeaders = [("content-type", "application/json")] }