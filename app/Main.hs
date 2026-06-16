{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Logger       (runStderrLoggingT)
import qualified Data.Text        as T
import Database.Persist.Sql       (runSqlPool)
import Database.Persist.Sqlite    (createSqlitePool, runMigration)
import Models                     (migrateAll)
import Routes                     (appRoutes)
import System.Environment         (lookupEnv)
import Text.Read                  (readMaybe)
import Web.Scotty                 (scotty)

main :: IO ()
main = do
  -- Render injeta a porta via $PORT; em desenvolvimento usamos 3000.
  port   <- maybe 3000 (\s -> maybe 3000 id (readMaybe s)) <$> lookupEnv "PORT"
  -- Caminho do banco configurável (disco persistente do Render, por ex.).
  dbPath <- maybe "plantdiary.db" T.pack <$> lookupEnv "DATABASE_PATH"
  pool <- runStderrLoggingT $ createSqlitePool dbPath 5
  runSqlPool (runMigration migrateAll) pool
  putStrLn $ "PlantDiary iniciado na porta " <> show port
  scotty port (appRoutes pool)
