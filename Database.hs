{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database where

import Control.Applicative

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Control.Exception

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Text.Encoding
import Network ( withSocketsDo )
import Network.HTTP.Conduit ( simpleHttp, HttpException )

import           Database.SQLite.Simple

data EmailRaw = EmailRaw
   { erUidl :: T.Text
   , erDate :: T.Text
   , erSubject :: T.Text
   , erFrom :: T.Text
   , erTo :: T.Text
   , rawMessage :: BS.ByteString
   }

instance ToRow EmailRaw where
   toRow(EmailRaw u d s f t w) = toRow (u,d,s,f,t,w)

instance FromRow EmailRaw where
   fromRow = EmailRaw <$> field <*> field <*> field <*> field <*> field <*> field 

data EmailLinks = EmailLinks 
    { elUidl :: T.Text
    , elhttpLink :: T.Text
    }

instance ToRow EmailLinks where
  toRow (EmailLinks l r) = toRow (l,r)

data LinkRaw = LinkRaw 
    { lrHttpLink :: T.Text
    , lrRawPage  :: BS.ByteString
    }

instance ToRow LinkRaw where
  toRow (LinkRaw l r) = toRow (l,r)

instance FromRow LinkRaw where
   fromRow = LinkRaw <$> field <*> field 

dbWriteEmails :: [EmailRaw] -> IO ()
dbWriteEmails rawEmails = runDB $ \conn -> withTransaction conn $ 
          insertEmailRaw conn rawEmails
  
insertEmailRaw :: Connection -> [EmailRaw] -> IO ()
insertEmailRaw conn = 
  mapM_  (execute conn "insert into email_raw values (?,?,?,?,?,?)")

dbReadKeys :: IO [BS.ByteString]
dbReadKeys = runDB $ \conn -> do
    uidls :: [Only T.Text] <-  query_ conn "select uidl from email_raw"
    return $! map (encodeUtf8 . fromOnly ) uidls


-- dbStorePages :: IO ()
dbStorePages :: IO ()
dbStorePages = withSocketsDo $ runDB $ \conn -> do
  httpLinks :: [Only T.Text]
     <- query_ conn "SELECT DISTINCT http_Link FROM email_links \
                       \ where not exists(select * from link_raw where http_link = email_links.http_link)"
  mapM_ (\(Only link) ->
    do TIO.putStrLn link
       page <- catch ((simpleHttp $ T.unpack link) >>= return . Just)
         (\e -> do let err = show (e :: HttpException)
                   TIO.putStrLn "Failed!"
                   let fn = T.unpack $ snd $  T.breakOnEnd "/" link
                   writeFile fn  err            
                   -- putStrLn err
                   return Nothing)
       case page of
          Just p1 -> execute conn "insert into link_raw values (?,?)" $
                                LinkRaw link $ BL.toStrict p1
          Nothing -> return () -- $! 
                               ) $ httpLinks
--    insertLinkRaw conn linkRaw

insertLinkRaw :: Connection -> [LinkRaw] -> IO ()
insertLinkRaw conn = mapM_ (execute conn "insert into link_raw values (?,?)") 

insertEmailLinks :: Connection -> [EmailLinks] -> IO ()
insertEmailLinks conn = mapM_ (execute conn "insert into email_links values (?,?)") 


dbReadPages :: IO [LinkRaw]
dbReadPages = runDB $ \conn -> do
  pages :: [LinkRaw] <- query_ conn "select * from link_raw"
  return pages

-- | Fetch all the emails that do not have entries in DB table EmailLinks
dbEmailNoLinks :: Connection -> IO [EmailRaw]
dbEmailNoLinks conn = do
  rawEmails :: [EmailRaw] <- query_ conn
      "select * from email_raw \
        \ where not exists (select * from email_links where uidl = email_raw.uidl)"
  return rawEmails

testDb01 :: IO [EmailRaw]
testDb01 = runDB $ \conn -> do
  rawEmails <- dbEmailNoLinks conn
  return $! take 1 rawEmails
                

runDB :: (Connection -> IO b) -> IO b
runDB = withConnection "Email.sqlite3"

