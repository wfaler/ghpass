{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
import Shelly
import Data.Text as T
import System.IO
import Control.Exception
import GHC.Generics
import Data.Aeson
default (T.Text)


data Index = Index {entries :: [IndexEntry]} deriving(Show,Eq,Generic)
data IndexEntry = IndexEntry { label :: T.Text, file :: T.Text} deriving(Show,Eq,Generic)
data Entry = Entry { keyValues :: [KeyValue] } deriving(Show,Eq,Generic)
data KeyValue = KeyValue {key :: T.Text, value :: T.Text} deriving(Show,Eq,Generic)

instance FromJSON KeyValue
instance ToJSON KeyValue

instance FromJSON Entry
instance ToJSON Entry

instance FromJSON IndexEntry
instance ToJSON IndexEntry

instance FromJSON Index
instance ToJSON Index

main :: IO ()
main = do
  putStrLn $ "hello "


encryptAES256 :: T.Text -> T.Text -> T.Text -> IO T.Text
encryptAES256 contents filename passphrase = do
  shelly $ silently $ do
    (-|-) (run "echo" [contents]) (run "gpg2" ["-ac", "-o", filename, "--cipher-algo", "AES256", "--batch", "--yes", "--passphrase", passphrase])

decryptAES256 :: T.Text -> T.Text -> IO T.Text
decryptAES256 filename passphrase = do
  shelly $ silently $ do
    (run "gpg2" ["--decrypt", "--cipher-algo", "AES256", "--batch", "--yes", "--passphrase", passphrase, filename])


promptPass :: IO String
promptPass = do
  putStrLn "Passphrase: "
  withEcho False getLine

setPassphrase :: IO (Maybe String)
setPassphrase = do
  putStrLn "Passphrase: "
  pass <- withEcho False getLine
  putStrLn "Repeat Passphrase: "
  pass2 <- withEcho False getLine
  if (pass == pass2)
    then return $ Just pass
    else return Nothing

withEcho :: Bool -> IO a -> IO a
withEcho e action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin e) (hSetEcho stdin old) action



