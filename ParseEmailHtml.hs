{-# LANGUAGE OverloadedStrings #-}
module ParseEmailHtml where

import Control.Applicative
import qualified Data.ByteString as BS
import Data.Text.Encoding
import qualified Data.Text as T



-- import System.IO
import Data.Maybe
import Network.HTTP.Types.URI
import Text.HTML.TagSoup

parseEmailHtml
  :: T.Text -> [BS.ByteString]
parseEmailHtml eHtml
  = mapMaybe fTitle $ parseTags eHtml

extractRef :: T.Text -> Maybe (Maybe BS.ByteString)
extractRef = lookup "kd" . parseQuery . (urlDecode False) . encodeUtf8

unForwardRef :: T.Text -> T.Text
unForwardRef ref =
  case T.breakOn "/expose/" $ T.takeWhile (/= '?') ref of
       (p1,p2) -> case T.breakOnEnd "/" p1 of
                         (p11, _) -> T.concat [T.replace "forward" "www" p11, T.tail p2] 
                            

fTitle :: Tag T.Text -> Maybe BS.ByteString 
fTitle tag = case tag of
              TagOpen "a" attrs
                -> case lookup "name" attrs of
                     Just "TITLE" -> (lookup "href" attrs) >>= extractRef >>= id
                     _ -> case lookup "class" attrs of
                             Just x | T.isPrefixOf "real-estate-title-link" x
                               -> encodeUtf8 <$> unForwardRef <$> lookup "href" attrs
                             _ -> Nothing 
              _ -> Nothing  


