{-# LANGUAGE QuasiQuotes, TypeFamilies, FlexibleContexts, TemplateHaskell, OverloadedStrings, GADTs  #-}
module Rememe.Model where

import Database.Persist.TH
import Data.Time (UTCTime)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

User json

  email           String
  password        String
  name            String

  validated       Bool
  uuidValidation  String

  UniqueEmail email

  deriving Show


Note json

  userId      UserId
  title       String
  description String
  alarmAt     UTCTime
  alarmOn     Bool
  hidden      Bool

  UniqueNote userId title

  deriving Show

|]
