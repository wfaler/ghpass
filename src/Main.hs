{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
import Shelly
import Data.Text as T
import System.IO
import Control.Exception
default (T.Text)

main :: IO ()
main = do

  val <- shelly $ verbosely $ do
    (-|-) (run "echo" ["foo bar"]) (run "gpg2" ["-ac", "-o", "file.gpg"])
  putStrLn $ "hello " ++ (show val)
  
-- same path on remote host
            -- will create directories

encrypt :: T.Text -> T.Text -> T.Text -> IO T.Text
encrypt contents filename passphrase = do
  shelly $ silently $ do
    (-|-) (run "echo" [contents]) (run "gpg2" ["-ac", "-o", filename, "--batch", "--yes", "--passphrase", passphrase])

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
