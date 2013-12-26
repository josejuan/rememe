{-# LANGUAGE OverloadedStrings #-}
module Rememe.Business.Error (
  BusinessErrorTypes (..)
, BusinessError (..)
, error_Unknown
, error_Access
, error_NoValidated
, error_IsAlreadyValidated
, error_WrongValidationCode
, error_NoteKeyNotFound
, error_UserEmailAlreadyInUse
) where

import Data.Aeson
import Data.Text (Text, pack)
import Control.Monad.Trans.Error

data BusinessErrorTypes =
       Error_Unknown
     | Error_Access
     | Error_NoValidated
     | Error_IsAlreadyValidated
     | Error_WrongValidationCode
     | Error_NoteKeyNotFound
     | Error_UserEmailAlreadyInUse
     deriving Show

data BusinessError = BusinessError { businessErrorType :: !BusinessErrorTypes
                                   , businessErrorDesc :: !Text
                                   } deriving Show

error_Unknown               = BusinessError Error_Unknown
error_Access                = BusinessError Error_Access
error_NoValidated           = BusinessError Error_NoValidated
error_IsAlreadyValidated    = BusinessError Error_IsAlreadyValidated
error_WrongValidationCode   = BusinessError Error_WrongValidationCode
error_NoteKeyNotFound       = BusinessError Error_NoteKeyNotFound
error_UserEmailAlreadyInUse = BusinessError Error_UserEmailAlreadyInUse

err :: Text -> Int -> Text -> Value
err ctx code msg = object ["error" .= code, "description" .= msg, "context" .= ctx]

instance ToJSON BusinessError where
  toJSON (BusinessError Error_Unknown               ctx) = err ctx 0 "Unknown"
  toJSON (BusinessError Error_Access                ctx) = err ctx 1 "Wrong user and/or password!"
  toJSON (BusinessError Error_NoValidated           ctx) = err ctx 2 "Your account is not validated!"
  toJSON (BusinessError Error_IsAlreadyValidated    ctx) = err ctx 3 "Your account is already validated!"
  toJSON (BusinessError Error_WrongValidationCode   ctx) = err ctx 4 "The provided validation code is not as expected!"
  toJSON (BusinessError Error_NoteKeyNotFound       ctx) = err ctx 5 "The provided note key not match with current user!"
  toJSON (BusinessError Error_UserEmailAlreadyInUse ctx) = err ctx 6 "The provided user email is already in use!"

instance Error BusinessError where
  noMsg  = error_Unknown "(unknown context)"
  strMsg = error_Unknown . pack
