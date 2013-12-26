{-# LANGUAGE DeriveGeneric #-}
module Rememe.Types where

import GHC.Generics
import Data.Aeson

data Credentials = Credentials { credentialsUsr :: !String
                               , credentialsPwd :: !String
                               } deriving (Show, Generic)

instance FromJSON Credentials
instance ToJSON Credentials
