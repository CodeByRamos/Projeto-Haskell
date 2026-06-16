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

-- | Modelos de dados (o "molde" do que guardamos no banco).
--
-- O bloco abaixo é uma mini-linguagem do Persistent: a partir dele o programa
-- cria AUTOMATICAMENTE as tabelas no banco SQLite e o código Haskell para
-- ler/gravar cada uma. Não precisamos escrever SQL na mão.
--
-- Dicas de leitura:
--   * "Text", "Day", "Int"... são os tipos de cada campo (texto, data, número).
--   * "Maybe" significa OPCIONAL — o campo pode ficar vazio (ex.: a espécie).
--   * Um campo terminado em "Id" (como "userId UserId") é uma LIGAÇÃO: aponta
--     para o dono daquele registro em outra tabela (chave estrangeira).
--   * "UniqueEmail email" garante que não existam dois usuários com o mesmo e-mail.
module Models where

import Data.Text          (Text)
import Data.Time          (Day, UTCTime)
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
-- Uma pessoa cadastrada no site.
User
    email        Text        -- e-mail de login
    passwordHash Text        -- senha já embaralhada (hash) — nunca em texto puro
    name         Text        -- nome de exibição
    createdAt    UTCTime
    UniqueEmail email         -- não pode repetir e-mail
    deriving Show

-- Uma sessão ativa (mantém o usuário logado entre páginas).
Session
    token     Text           -- código aleatório guardado no cookie do navegador
    userId    UserId         -- a qual usuário esta sessão pertence
    createdAt UTCTime
    UniqueToken token
    deriving Show

-- Uma planta. Pertence sempre a um usuário (userId).
Plant
    userId            UserId      -- dono da planta
    name              Text
    species           Text Maybe   -- nome científico (opcional)
    acquiredDate      Day
    location          Text         -- onde fica (sala, varanda...)
    heightCm          Double Maybe -- altura aproximada (opcional)
    waterIntervalDays Int Maybe    -- de quantos em quantos dias regar (opcional)
    notes             Text Maybe
    createdAt         UTCTime
    deriving Show

-- Um registro de cuidado (rega, poda...). Pertence a uma planta (plantId).
CareLog
    plantId   PlantId         -- a qual planta este cuidado se refere
    date      Day
    careType  Text            -- tipo: "rega", "poda", "adubacao"...
    notes     Text Maybe
    createdAt UTCTime
    deriving Show
|]
