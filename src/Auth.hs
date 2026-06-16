{-# LANGUAGE OverloadedStrings #-}

-- | Hashing de senhas (PBKDF2-HMAC-SHA256) e geração de tokens de sessão.
module Auth
  ( hashPassword
  , verifyPassword
  , generateToken
  ) where

import           Crypto.Hash.Algorithms   (SHA256 (..))
import qualified Crypto.KDF.PBKDF2         as PBKDF2
import           Crypto.Random            (getRandomBytes)
import           Data.ByteArray.Encoding  (Base (Base16), convertFromBase, convertToBase)
import qualified Data.ByteString          as BS
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)

-- Número de iterações do PBKDF2 e tamanho do hash em bytes.
pbkdf2Params :: PBKDF2.Parameters
pbkdf2Params = PBKDF2.Parameters 100000 32

-- Calcula o hash da senha com um sal específico.
hashWith :: BS.ByteString -> BS.ByteString -> BS.ByteString
hashWith password salt =
  PBKDF2.generate (PBKDF2.prfHMAC SHA256) pbkdf2Params password salt

toHex :: BS.ByteString -> Text
toHex = decodeUtf8 . convertToBase Base16

fromHex :: Text -> Maybe BS.ByteString
fromHex t = either (const Nothing) Just (convertFromBase Base16 (encodeUtf8 t))

-- | Gera um hash no formato "salHex:hashHex" a partir de uma senha em texto.
hashPassword :: Text -> IO Text
hashPassword password = do
  salt <- getRandomBytes 16 :: IO BS.ByteString
  let h = hashWith (encodeUtf8 password) salt
  return (toHex salt <> ":" <> toHex h)

-- | Verifica se a senha em texto corresponde ao hash armazenado.
verifyPassword :: Text -> Text -> Bool
verifyPassword password stored =
  case T.splitOn ":" stored of
    [saltHex, hashHex] ->
      case fromHex saltHex of
        Just salt -> toHex (hashWith (encodeUtf8 password) salt) == hashHex
        Nothing   -> False
    _ -> False

-- | Gera um token de sessão aleatório (32 bytes em hexadecimal).
generateToken :: IO Text
generateToken = do
  bytes <- getRandomBytes 32 :: IO BS.ByteString
  return (toHex bytes)
