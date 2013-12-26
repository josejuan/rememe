{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, ScopedTypeVariables, NoMonomorphismRestriction, QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts, TemplateHaskell, OverloadedStrings, GADTs, MultiParamTypeClasses #-}
import Yesod
import GHC.Generics
import Rememe.Business
import Data.Either
import Data.Aeson
import Database.Persist.Sql
import Data.Text
import Control.Monad.Trans.Error (throwError)
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)

data RememeApp = RememeApp { cnx :: ConnectionPool }

instance Yesod RememeApp

instance YesodPersist RememeApp where
    type YesodPersistBackend RememeApp = SqlPersistT
    runDB action = getYesod >>= runSqlPool action . cnx

mkYesod "RememeApp" [parseRoutes|
/ RememeEndPointR POST
|]

data RememeRequest = RememeRequest { action            :: String
                                   , usermail          :: String
                                   , password          :: String
                                   , validationCode    :: Maybe String
                                   , userdata          :: Maybe User
                                   , notedata          :: Maybe (Entity Note)
                                   , hiddenMustBe      :: Maybe Bool
                                   , titleMustContains :: Maybe Text
                                   } deriving (Show, Generic)

instance FromJSON RememeRequest
instance ToJSON RememeRequest

data Restatus a = Restatus String a
instance ToJSON a => ToJSON (Restatus a) where
  toJSON (Restatus status value) = object ["status" .= status, "value" .= value]

postRememeEndPointR :: Handler Value
postRememeEndPointR = do
  addHeader "Access-Control-Allow-Origin" "*"
  addHeader "Access-Control-Allow-Methods" "*"
  jsonBody <- parseJsonBody
  case jsonBody of
    Error jsonParseError -> invalidArgs ["Invalid message format", pack jsonParseError]
    Success request      -> do
                              let creds = Credentials (usermail request) (password request)
                              response <- runDB $ case (action request) of
                                                    "uservalidate" -> userValidationOperation creds request
                                                    "useradd"      -> userAddOperation creds
                                                    otherwise      -> flip runBusiness creds $ performRequestOperation request
                              return $ case response of
                                             Left responseError -> toJSON responseError
                                             Right responseData -> responseData

userAddOperation creds = do
  rs <- unprotecteAddUserAccount creds
  return $ (rs >> (Right $ toJSON $ Restatus "Account validation code has been sent to your mail successfully" ("" :: String)))

userValidationOperation creds request = do
  rs <- case validationCode request of
           Just vc    -> unprotectedValidateUserAccount creds vc
           Nothing    -> return $ Left $ error_Unknown $ pack "Validation code not provided"
  return $ (rs >> (Right $ toJSON $ Restatus "Account validated successfully" ("" :: String)))

operationError errorType errorMessage = lift $ throwError $ errorType $ pack errorMessage

performRequestOperation request@(RememeRequest { action = a }) = do
  case a of
    "userinfo"   -> getUserInfo >>= returnJson
    "userupdate" -> performUpdateUserInfo $ userdata request
    "noteadd"    -> performNoteAdd $ notedata request
    "noteupdate" -> performNoteUpdate $ notedata request
    "notelist"   -> listUserNotes (hiddenMustBe request) (titleMustContains request) >>= returnJson
    "notedelete" -> performNoteDelete $ notedata request
    otherwise    -> operationError error_Unknown $ "Requested action is unkown: " ++ a

performNoteDelete noteInfo =
  case noteInfo of
    Just (Entity knote _) -> deleteUserNote knote  >> (returnJson $ Restatus "Note data deleted successfully" ("" :: String))
    Nothing               -> operationError error_Unknown "Note data to delete not provided"

performUpdateUserInfo mnewUserInfo =
  case mnewUserInfo of
    Just newUserInfo -> updateUserInfo newUserInfo >> (returnJson $ Restatus "Account updated successfully" ("" :: String))
    Nothing          -> operationError error_Unknown "User data to update not provided"

performNoteAdd mnewNoteInfo =
  case mnewNoteInfo of
    Just (Entity _ newNoteInfo) -> addUserNote newNoteInfo >>= returnJson . Restatus "Note created successfully"
    Nothing                     -> operationError error_Unknown "Note data to insert not provided"

performNoteUpdate mnewNoteInfo =
  case mnewNoteInfo of
    Just newNoteInfo -> updateUserNote newNoteInfo >> returnJson (Restatus "Note updated successfully" ("" :: String))
    Nothing          -> operationError error_Unknown "Note data to update not provided"

main :: IO ()
main = withSqlitePool "rememe.db3" 10 $ \p -> do
         runResourceT $ runStderrLoggingT $ flip runSqlPool p $ runMigration migrateAll
         warp 8181 $ RememeApp p
