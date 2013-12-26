{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies, GADTs, FlexibleContexts, OverloadedStrings #-}
module Rememe.Business (
  module Rememe.Types
, module Rememe.Model
, module Rememe.Business.Error
, runBusiness
, nullKey
, getUserInfo
, updateUserInfo
, addUserNote
, updateUserNote
, deleteUserNote
, unprotecteAddUserAccount
, unprotectedValidateUserAccount
, listUserNotes
) where

import Rememe.Types
import Rememe.Model
import Rememe.Business.Error
import Rememe.Business.Mail (notify)
import Data.Text
import Data.Maybe
import Database.Persist
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Error (ErrorT, runErrorT, throwError)
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)

unprotectedValidateUserAccount :: (PersistQuery m, PersistMonadBackend m ~ PersistEntityBackend User)
                                  => Credentials -> String -> m (Either BusinessError ())
unprotectedValidateUserAccount (Credentials usr pwd) validationCode = do
  ud <- selectFirst [ UserEmail ==. usr, UserPassword ==. pwd ] []
  case ud of
    Nothing             -> return $ Left $ error_Access ""
    Just (Entity uk ur) -> case (userValidated ur, userUuidValidation ur == validationCode) of
                             (True,      _) -> return $ Left $ error_IsAlreadyValidated ""
                             (False, False) -> return $ Left $ error_WrongValidationCode ""
                             (False, True ) -> do
                                                 update uk [UserValidated =. True]
                                                 return $ Right ()

unprotecteAddUserAccount :: PersistQuery m => Credentials -> m (Either BusinessError ())
unprotecteAddUserAccount (Credentials usr pwd) = do
  pusr <- selectFirst [ UserEmail ==. usr ] [] -- this check is not transactional, backend could fail after it (really, it's a precheck)
  case pusr of
    Just  _ -> return $ Left $ error_UserEmailAlreadyInUse ""
    Nothing -> do
                 uid <- liftIO $ nextRandom
                 let vcode = toString uid
                 insert (User { userEmail          = usr
                              , userPassword       = pwd
                              , userName           = ""
                              , userValidated      = False
                              , userUuidValidation = vcode
                              })
                 liftIO $ notify "rememe@computer-mind.com"
                                 (pack usr)
                                 "Rememe New Account Validation Code"
                                 (pack $ "Your validation code is: " ++ vcode)
                 return $ Right ()

data BusinessConfig = BusinessConfig { userData :: !(Entity User) } deriving Show

type BusinessRun m a = ReaderT BusinessConfig (ErrorT BusinessError m) a

runBusiness :: (PersistQuery m, PersistMonadBackend m ~ PersistEntityBackend User)
               => BusinessRun m a -> Credentials -> m (Either BusinessError a)
runBusiness m creds = do uk <- selectFirst [ UserEmail    ==. credentialsUsr creds
                                           , UserPassword ==. credentialsPwd creds ] []
                         case uk of
                           Nothing    -> return $ Left $ error_Access ""
                           Just udata -> if not (userValidated $ entityVal udata)
                                           then return $ Left $ error_NoValidated ""
                                           else runErrorT (runReaderT m (BusinessConfig udata))

nullKey :: KeyBackend backend entity
nullKey = Key PersistNull

getUserKey :: Monad m => BusinessRun m (Key User)
getUserKey = ask >>= return . entityKey . userData

getUserInfo :: Monad m => BusinessRun m User
getUserInfo = ask >>= return . entityVal . userData

updateUserInfo :: (PersistQuery m, PersistMonadBackend m ~ PersistEntityBackend User) => User -> BusinessRun m ()
updateUserInfo newUserInfo = do
   ukey <- getUserKey
   update ukey [ UserName =. userName newUserInfo ]

addUserNote :: (PersistStore m, PersistMonadBackend m ~ PersistEntityBackend Note) => Note -> BusinessRun m (Key Note)
addUserNote note = do
  ukey <- getUserKey
  insert (note { noteUserId = ukey })

getUserNoteInfo :: (PersistQuery m, PersistMonadBackend m ~ PersistEntityBackend Note) => Key Note -> BusinessRun m (Entity Note)
getUserNoteInfo noteKey = do
  ukey <- getUserKey
  enote <- selectFirst [ NoteUserId ==. ukey, NoteId ==. noteKey ] []
  case enote of
    Just note -> return note
    Nothing   -> lift $ throwError $ error_NoteKeyNotFound ""

updateUserNote :: (PersistQuery m, PersistMonadBackend m ~ PersistEntityBackend User) => Entity Note -> BusinessRun m ()
updateUserNote (Entity nkey newNoteInfo) = do
   ukey <- getUserKey
   Entity nkey' _ <- getUserNoteInfo nkey
   update nkey' [ NoteTitle       =. noteTitle newNoteInfo
                , NoteDescription =. noteDescription newNoteInfo
                , NoteAlarmAt     =. noteAlarmAt newNoteInfo
                , NoteAlarmOn     =. noteAlarmOn newNoteInfo
                , NoteHidden      =. noteHidden newNoteInfo
                ]

deleteUserNote :: (PersistQuery m, PersistMonadBackend m ~ PersistEntityBackend Note) => Key Note -> BusinessRun m ()
deleteUserNote noteKey = do
  ukey <- getUserKey
  knote <- selectKeysList [ NoteUserId ==. ukey, NoteId ==. noteKey ] []
  case knote of
    [nkey]  -> delete nkey
    []      -> lift $ throwError $ error_NoteKeyNotFound ""
    _       -> lift $ throwError $ error_Unknown "Only one key expected but many returned"

listUserNotes :: (PersistQuery m, PersistMonadBackend m ~ PersistEntityBackend Note) => Maybe Bool -> Maybe Text -> BusinessRun m [Entity Note]
listUserNotes hiddenMustBe titleMustContains = do
  ukey <- getUserKey
  ns <- selectList [ NoteUserId ==. ukey ] []
  return $ Prelude.filter (f.entityVal) ns

  where f n = fh (noteHidden n) && ft (pack $ noteTitle n)
        fh = maybe (\_ -> True) (\h -> \a -> a == h) hiddenMustBe
        ft = maybe (\_ -> True) (\t -> \a -> a == t) titleMustContains
