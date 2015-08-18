{-# LANGUAGE OverloadedStrings #-}
module ParseEmailHtml where

-- import Control.Applicative
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
  = mapMaybe (fromJust . lookup "kd" . parseQuery . (urlDecode False) . encodeUtf8)
                        $ mapMaybe fTitle $ parseTags eHtml

fTitle :: Tag T.Text -> Maybe T.Text
fTitle tag = case tag of
                TagOpen "a" attrs -> case lookup "name" attrs of
                                            Just "TITLE" -> lookup "href" attrs
                                            _ -> Nothing   
                _ -> Nothing  
