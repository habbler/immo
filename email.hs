{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}


module Email where

import Control.Applicative
import Network.HaskellNet.POP3.SSL
import Network.HaskellNet.POP3.Connection
import qualified Data.ByteString as BS
import Data.Text.Encoding
-- import qualified Data.Text.IO as TIO
import Codec.MIME.Parse
import Codec.MIME.Type
-- import Codec.MIME.QuotedPrintable as QP
-- import qualified Data.Text as T

-- import System.IO
--import Data.Maybe
--import Network.HTTP.Types.URI

import ParseEmailHtml

import Database
import           Database.SQLite.Simple

import Data.List
import Data.Maybe

fEmailRaw :: POP3Connection -> (Int, BS.ByteString) -> IO EmailRaw
fEmailRaw con (nr1, uidl1) =
  do msg <- retr con nr1
     let out = parseMIMEMessage (decodeUtf8 msg)
         headers = mime_val_headers out
         findValue name = paramValue
                            $ fromJust $ find ((== name) . paramName) headers 
     return $! EmailRaw
                  (decodeUtf8 uidl1)
                  (findValue "date")
                  (findValue "subject")
                  (findValue "from")
                  (findValue "to")
                  msg

connectEmailServer :: Int -> IO POP3Connection
connectEmailServer index = do
  users  <- readIO =<< readFile "../users.txt"
  let (server, userN, password) = users !! index
  con <- connectPop3SSL server
  userPass con userN password 
  return con

-- test1 :: IO ()
fetchNewEmails :: IO [EmailRaw]
fetchNewEmails
      = do con <- connectEmailServer 1
           num <- list con 4
           print $ "num " ++ (show num)
           nr <- stat con
           print $ "stat " ++ (show nr)
           
           uidls <- allUIDLs con
           readUidls <- dbReadKeys
           let newUidls = filter (\(_, uidl') -> notElem uidl' readUidls) uidls
           print $ "new emails: " ++ (show $ length newUidls)
           emailRaw <- mapM (fEmailRaw con) newUidls
           -- let msg1 = parseMIMEMessage (decodeUtf8 msg)
           closePop3 con
           return $! emailRaw


test3 :: IO ()
test3 = dbWriteEmails =<< fetchNewEmails

-- test2  :: IO (BS.ByteString, BS.ByteString)
test2 :: IO EmailRaw
test2 = head <$> fetchNewEmails

readEmails :: IO [[BS.ByteString]]
readEmails
      = do con <- connectEmailServer 0
           (nr,_length1) <- stat con
           print $ "stat " ++ (show nr)
           let procEmail nr1
                  = do msg <- retr con nr1 
                       return $! msgRetrieveRefs
                           $ parseMIMEMessage (decodeUtf8 msg) 
           links <- mapM procEmail [1..nr]
           closePop3 con
           return links

test4 :: IO [MIMEParam]
test4
      = do con <- connectEmailServer 0
           (nr,_length1) <- stat con
           print $ "stat " ++ (show nr)
           msg <- retr con 2
           let out = parseMIMEMessage (decodeUtf8 msg)
               headers = mime_val_headers out
               
           closePop3 con
           return $! headers
-- ["return-path","received","dkim-signature","domainkey-signature","received","date","from","reply-to","to","message-id","subject","mime-version","content-type","x-mid","x-job","x-rpcampaign","x-orgid","list-unsubscribe","envelope-to","authentication-results","x-tdresult","x-tdcapabilities","x-ui-filterresults"]

msgRetrieveRefs :: MIMEValue -> [BS.ByteString]
msgRetrieveRefs msg = 
           let Multi content = mime_val_content msg
               -- Single out = mime_val_content $ content !! 0
               Single out1 = mime_val_content $ content !! 1
           --BS.writeFile "test.html" $ encodeUtf8 out1
           -- TIO.writeFile "test.txt" out1  
           in parseEmailHtml out1

dbCalcEmailRefs :: Monad m => [EmailRaw] -> m [EmailLinks]
dbCalcEmailRefs rawEmails = do
  return $! concatMap (\rawEmail -> 
                   let msg = parseMIMEMessage
                                  (decodeUtf8 $ rawMessage rawEmail)
                       refs = msgRetrieveRefs msg
                       uidl1 = erUidl rawEmail
                   in map (\ref -> EmailLinks uidl1 $ decodeUtf8 ref) refs) $ rawEmails

dbWriteLinks :: IO ()
dbWriteLinks = runDB $ \conn -> do
  -- rawEmails :: [Entity EmailRaw] <- selectList [] []
  rawEmails <- dbEmailNoLinks conn
  emailLinks <- dbCalcEmailRefs rawEmails 
  insertEmailLinks conn emailLinks

test10 :: IO [BS.ByteString]
test10 = runDB $ \conn -> do
  rawMsg1 :: [Only BS.ByteString] <- query_ conn
        "select raw_message from email_raw where \
          \ date = 'Tue, 4 Aug 2015 09:42:22 +0200 (CEST)'"
  let rawMsg = fromOnly $ head rawMsg1
      msg = parseMIMEMessage (decodeUtf8 $ rawMsg )
      refs = msgRetrieveRefs msg
      -- Multi content = mime_val_content msg
      -- Single out = mime_val_content $ content !! 0
      -- Single out1 = mime_val_content $ content !! 1
  return refs -- $ T.take 100 out1

--test11 = do out1 <- test10
--            TIO.writeFile "immo24.html" out1


