{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Database where

import Control.Applicative

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import           Database.Persist as P
import           Control.Monad.IO.Class  (MonadIO, liftIO)
-- import Control.Monad.Trans.Control
import           Database.Persist.Sqlite as Sql
import           Database.Persist.TH

import Control.Exception

import qualified Data.Text as T

import Data.Text.Encoding
import Network ( withSocketsDo )
import Network.HTTP.Conduit ( simpleHttp, HttpException )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
EmailRaw
    uidl T.Text
    date T.Text
    subject T.Text
    from T.Text
    to T.Text
    rawMessage BS.ByteString
    Primary uidl
    deriving Show
EmailLinks
    uidl T.Text
    httpLink T.Text
    Primary uidl httpLink
    deriving Show
LinkRaw
    httpLink T.Text
    rawPage BS.ByteString
    Primary httpLink
    deriving Show
|]
-- sqltype=varchar(255)
dbWriteEmails :: [EmailRaw] -> IO ()
dbWriteEmails rawEmails = runDB $ do
    insertMany_ $ rawEmails

dbReadKeys :: IO [BS.ByteString]
dbReadKeys = runDB $ do
    uidls <- selectKeysList [] []
    return $! map (encodeUtf8 . unEmailRawKey) uidls


dbStorePages = withSocketsDo $ runDB $ do
  httpLinks :: [Single PersistValue]
     <- rawSql "SELECT DISTINCT http_Link FROM email_links" []
  linkRaw <- mapM (\(Single (PersistText link)) ->
                     do page <- liftIO $ catch (simpleHttp $ T.unpack link)
                          (\e -> do let err = show (e :: HttpException)
                                    putStrLn err
                                    return "")                          
                        return $! LinkRaw link $ BL.toStrict page) httpLinks
  insertMany_ $ linkRaw

dbReadPages = runDB $ do
  pages <- selectList [] []
  return $! map (\(Entity _ (LinkRaw httpLink rawPage)) -> (httpLink, rawPage))
              pages
  

runDB x = runSqlite "Email.sqlite3" $ runMigration migrateAll >> x
     


{--
    johnId <- insert $ Person "John Doe" $ Just 35
    janeId <- insert $ Person "Jane Doe" Nothing

    insert $ BlogPost "My fr1st p0st" johnId
    insert $ BlogPost "One more for good measure" johnId

    oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [P.LimitTo 1]
    liftIO $ print (oneJohnPost :: [P.Entity BlogPost])

    john <- P.get johnId
    liftIO $ print (john :: Maybe Person)

    delete janeId
    P.deleteWhere [BlogPostAuthorId ==. johnId]
--}
