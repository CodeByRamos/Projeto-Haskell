{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Models where

import Data.Text          (Text)
import Data.Time          (Day, UTCTime)
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Plant
    name              Text
    species           Text Maybe
    acquiredDate      Day
    location          Text
    heightCm          Double Maybe
    waterIntervalDays Int Maybe
    notes             Text Maybe
    createdAt         UTCTime
    deriving Show

CareLog
    plantId   PlantId
    date      Day
    careType  Text
    notes     Text Maybe
    createdAt UTCTime
    deriving Show
|]
