{-# LANGUAGE OverloadedStrings #-}

-- | Ponto de entrada do PlantDiary.
--
-- É o "gerente que abre o restaurante": roda uma única vez quando o site liga.
-- Ele prepara o banco de dados, descobre em qual porta atender e então
-- entrega o controle para as rotas (definidas em "Routes").
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
  -- A porta vem da variável de ambiente PORT (o Render define ela sozinho).
  -- Se não existir (rodando no seu PC), usamos 3000 como padrão.
  port   <- maybe 3000 (\s -> maybe 3000 id (readMaybe s)) <$> lookupEnv "PORT"
  -- Onde o arquivo do banco SQLite fica salvo. Também configurável por ambiente.
  dbPath <- maybe "plantdiary.db" T.pack <$> lookupEnv "DATABASE_PATH"
  -- Abre um "pool" com 5 conexões reaproveitáveis com o banco.
  pool <- runStderrLoggingT $ createSqlitePool dbPath 5
  -- Cria/atualiza as tabelas no banco a partir dos modelos (ver "Models").
  runSqlPool (runMigration migrateAll) pool
  putStrLn $ "PlantDiary iniciado na porta " <> show port
  -- Liga o servidor web e passa a responder aos pedidos usando as rotas.
  scotty port (appRoutes pool)
