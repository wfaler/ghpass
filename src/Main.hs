{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}

import Shelly
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.IO
import Control.Exception
import GHC.Generics
import Data.Aeson
import qualified System.Process as P
import qualified System.Info as INFO
import qualified Data.Text.Lazy.Encoding as TE
import qualified System.Directory as DIR
import System.Exit
import System.Environment
import System.Random
import qualified Data.List as L
import Data.Maybe
import Data.UUID.V4
import qualified Data.UUID as UUID
import Data.Char

type Passphrase = T.Text
type Contents = T.Text
type Filename = T.Text


data Index = Index {entries :: [IndexEntry]} deriving(Show,Eq,Generic)
data IndexEntry = IndexEntry { label :: T.Text, file :: String} deriving(Eq,Generic)
data Entry = Entry { keyValues :: [KeyValue] } deriving(Eq,Show,Generic)
data KeyValue = KeyValue {key :: T.Text, value :: T.Text} deriving(Eq,Generic)

instance Show IndexEntry where
  show e = T.unpack $ ((label) e)

instance Show KeyValue where
  show kv = ((T.unpack . key) kv) ++ ": " ++ ((T.unpack . value) kv)

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
  args <- getArgs
  processArgs args
  where
    processArgs ["init"] = initStore
    processArgs ["gen", "--no-specials", pwdLength] = do
      _ <- genPass genPwdNoSpecials pwdLength
      putStrLn "A new password has ben generated for you and put in your clipboard"
    processArgs ["gen", pwdLength] = do
      _ <- genPass genPwdSpecials pwdLength
      putStrLn "A new password has ben generated for you and put in your clipboard"
    processArgs ["new", entry, username, pwdLength] = do
      pass <- promptPass
      idx <- index pass
      passwd <- (genPass genPwdSpecials pwdLength)
      newEntry entry username passwd pass idx
    processArgs ["new", "--no-specials", entry, username, pwdLength] = do
      pass <- promptPass
      idx <- index pass
      passwd <- (genPass genPwdNoSpecials pwdLength)
      newEntry entry username passwd pass idx
    processArgs ["new", "--own-pass", entry, username, password] = do
      pass <- promptPass
      idx <- index pass
      newEntry entry username (T.pack password) pass idx
    processArgs ["clip", entry] = do
      pass <- promptPass
      doEntry entry clipEntry pass
    processArgs ["show", entry] = do
      pass <- promptPass
      doEntry entry (\entry -> mapM_ (putStrLn . show) (keyValues entry)) pass
    processArgs ["search", substr] = search substr
    processArgs ["search"] = search ""
    processArgs ["import1password", fileName] = error "todo"
    processArgs ["update", entry, key, value] = error "todo"
    processArgs ["rm", entry] = error "todo"
    processArgs ["rm-key", entry, key] = error "todo"
    processArgs __ = do
      putStrLn "your options did not match any valid options"
      exitFailure


search substr = do
  pass <- promptPass
  matches <- index pass >>= (\idx -> return $ filter (\e -> L.isInfixOf (fmap toLower substr) (((fmap toLower) . T.unpack . label) e)) (entries idx))
  putStrLn "Matching entries: "
  showWithIndex 1 matches
  putStrLn "Select index you want to show"
  entryNo <- getLine >>= (return . read) :: IO Int
  if (entryNo <= (length matches))
    then do
    (return ((T.unpack . label) (matches !! (entryNo - 1)))) >>= (clipOrShow pass)
    else do
    putStrLn "Your input does not match any entries"
    exitFailure

clipOrShow pass entry = do
  putStrLn "clipboard (c) or show (s)?"
  arg <- getLine
  doArg arg
  where
    doArg "s" = doEntry entry (\entry -> mapM_ (putStrLn . show) (keyValues entry)) pass
    doArg "c" = doEntry entry clipEntry pass
    doArg _ = clipOrShow pass entry
    
showWithIndex :: Int -> [IndexEntry] -> IO ()
showWithIndex _ [] = return ()
showWithIndex i (x:xs) = do
  putStrLn $ (show i) ++ ". " ++ (show x)
  showWithIndex (i + 1) xs

clipEntry entry = do
  _ <- clip $ fromMaybe "" (fmap (\kv -> (value kv)) (L.find (\kv -> (key kv) == "password") (keyValues entry)))
  putStrLn "Password has been copied into your clipboard"

doEntry entry entryFn pass = do
  maybeEntry <- index pass >>= (\idx -> return $ L.find (\x -> (label x) == (T.pack entry)) (entries idx))
  clipEntry maybeEntry pass
  where
    clipEntry Nothing _ = do
      putStrLn $ "no entry matching '" ++ entry ++ "' found!"
      exitFailure
    clipEntry (Just fileEntry) pass = do
      entry <- getEntry pass (file fileEntry)
      entryFn entry
  

newEntry :: String -> String -> T.Text -> Passphrase -> Index -> IO ()
newEntry entry username newPwd pass idx = do
  newEntryWithEntries entry [(KeyValue "username" (T.pack username)), (KeyValue "password" newPwd)] pass idx


entryExists :: String -> Index -> Bool
entryExists entry idx = any (\x -> (label x) == (T.pack entry)) (entries idx)

newEntryWithEntries :: String -> [KeyValue] -> Passphrase -> Index -> IO ()
newEntryWithEntries entry kvs pass idx = do
  entryExists <- return $ entryExists entry idx
  if (entryExists)
    then
    do
      putStrLn $ "entry with name " ++ entry ++ "already exists!"
      exitFailure
    else
    do
      filename <- nextRandom
      dtDir <- dataDir
      entr <- return $ Entry kvs
      _ <- saveFile False entr (dtDir ++ (UUID.toString filename) ++ ".gpg") pass
      newIdx <- saveIndex (Index ((IndexEntry (T.pack entry) (UUID.toString filename)) : (entries idx))) pass
      putStrLn "New entry created"
  

genPass :: Read a => (a -> [t] -> IO T.Text) -> String -> IO T.Text
genPass genFn pwdLength = do
      passwd <- genFn (read pwdLength) []
      _ <- clip passwd
      return passwd

clip :: T.Text -> IO P.ProcessHandle
clip = do
  if (INFO.os == "linux")
    then xclip
    else pbcopy

gpg :: Shelly.FilePath
gpg = "gpg2"
  
pbcopy :: T.Text -> IO P.ProcessHandle
pbcopy text = P.runCommand $ "echo -n '" ++ (T.unpack text) ++ "' | pbcopy"

xclip :: T.Text -> IO P.ProcessHandle
xclip text = P.runCommand $ "echo -n '" ++ (T.unpack text) ++ "' | xclip -selection clipboard"

encrypt :: Contents -> Filename -> Passphrase -> IO T.Text
encrypt = encryptAES256

encryptAES256 :: Contents -> Filename -> Passphrase -> IO T.Text
encryptAES256 contents filename passphrase = do
  shelly $ silently $ do
    (-|-) (run "echo" ["-n", contents]) (run gpg ["-ac", "-o", filename, "--cipher-algo", "AES256", "--batch", "--yes", "--passphrase", passphrase])

decrypt :: Filename -> Passphrase -> IO T.Text
decrypt filename passphrase = do
  shelly $ silently $ do
    (run gpg ["--decrypt", "--batch", "--yes", "--passphrase", passphrase, filename])


promptPass :: IO Passphrase
promptPass = do
  putStrLn "Passphrase: "
  withEcho False getLine >>= (return . T.pack)

setPassphrase :: IO (Maybe Passphrase)
setPassphrase = do
  putStrLn "Passphrase: "
  pass <- withEcho False getLine
  putStrLn "Repeat Passphrase: "
  pass2 <- withEcho False getLine
  if (pass == pass2)
    then (return . Just . T.pack) pass
    else return Nothing

withEcho :: Bool -> IO a -> IO a
withEcho e action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin e) (hSetEcho stdin old) action

fromTxt :: FromJSON a => T.Text -> Maybe a
fromTxt text = (decode . TE.encodeUtf8 . TL.fromChunks) [text]

toTxt :: ToJSON a => a -> T.Text
toTxt jsonObj = T.concat ((TL.toChunks . TE.decodeUtf8 . encode) jsonObj)

fromFile :: FromJSON a => Filename -> Passphrase -> IO a
fromFile pth pass = do
  contents <- decrypt pth pass
  validFile $ fromTxt contents
  where
    validFile (Just a) = return a
    validFile (Nothing) = do
      putStrLn ("file " ++ (show pth) ++ "does not exist!")
      exitFailure
  

passDir :: IO System.IO.FilePath
passDir = do
  homeDir <- DIR.getHomeDirectory
  return $ homeDir ++ "/.ghpass/"

dataDir :: IO System.IO.FilePath
dataDir = do
  dir <- passDir
  return $ dir ++ "data/"

initStore :: IO ()
initStore = do
  dir <- passDir
  exists <- DIR.doesDirectoryExist dir
  if exists
    then do
    putStrLn $ dir ++ " already exists!"
    exitFailure
    else do
    passphrase <- setPassphrase
    initIndex passphrase dir
    where
      initIndex Nothing _ = do
        putStrLn "Passphrases do not match, exiting"
        exitFailure
      initIndex (Just pass) dir = do
        DIR.createDirectory dir
        dataDr <- dataDir
        DIR.createDirectory dataDr
        _ <- saveFile True (Index []) (dir ++ "index.gpg") pass
        putStrLn "Initialised new ghpass store"


nums :: [Char]
nums = ['0'..'9']

lowers :: [Char]
lowers = ['a'..'z']

uppers :: [Char]
uppers = ['A'..'Z']

specials :: [Char]
specials = ['!', '#', '$', '%', '^', '&', '*', '(', ')', '_', '-', '+', '=', '[', ']', '{', '}', ':', ';', '@',  '~', '#', '.', '?' ]

genPwdNoSpecials :: Int -> [Char] -> IO T.Text
genPwdNoSpecials = genPwd (nums ++ lowers ++ uppers)
genPwdSpecials :: Int -> [Char] -> IO T.Text
genPwdSpecials = genPwd (nums ++ lowers ++ uppers ++ specials)

genPwd :: [Char] -> Int -> [Char] -> IO T.Text
genPwd allChars len list = do
  maxIndex <- return ((length allChars) - 1)
  number <- getStdRandom (randomR (0,maxIndex))
  newList <- return $ (allChars !! number) : list
  if((length newList) < len)
    then genPwd allChars len newList
    else return $ T.pack newList
    
index :: Passphrase -> IO Index
index pass = do
  dr <- passDir
  fromFile (T.pack (dr ++ "index.gpg")) pass :: IO Index

getEntry :: Passphrase -> String -> IO Entry
getEntry pass fileName = do
  dr <- dataDir
  fromFile (T.pack (dr ++ fileName ++ ".gpg")) pass :: IO Entry

saveIndex :: Index -> Passphrase -> IO T.Text
saveIndex idx pass = do
  dr <- passDir
  saveFile True idx (dr ++ "index.gpg") pass
  
saveFile :: ToJSON a => Bool -> a -> String -> Passphrase -> IO T.Text
saveFile overwrite obj file pass = do
  fileExists <- DIR.doesFileExist file
  if (fileExists && (overwrite == False))
    then
    do
      putStrLn "Duplicate file, exiting!"
      exitFailure
    else
    encrypt (toTxt $ obj) (T.pack file) pass
