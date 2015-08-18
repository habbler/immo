{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Immow where

import Control.Applicative
-- import Control.Arrow
import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
-- import System.IO
import Data.Maybe
import Data.List as L
import Network.HTTP.Conduit
-- import Network.HTTP.Types.URI
import Data.Char
import qualified Data.ByteString.Char8 as BC
import Network ( withSocketsDo )
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Csv as CS
import Text.HTML.Scalpel as Sc

import Email
import Database

eHead :: [Char] -> [t] -> t
eHead err [] = error err
eHead _ (h:_) = h        

getTagContents :: Eq str => str -> ([Attribute str] -> Bool) -> [Tag str]
               -> [[Tag str]]
getTagContents name pAttrs tags =
   map( takeWhile (not . tagCloseLit name) . drop 1 )
       $ sections (tagOpenLit name pAttrs) tags

getTagIDContent :: Eq str => str -> str -> str -> [Tag str] -> [Tag str]
getTagIDContent name aName aValue  =
   takeWhile (not . tagOpenLit name (anyAttrNameLit aName)) . drop 1 
       . mHead . sections (tagOpenLit name (anyAttrLit (aName,aValue)))
       where mHead  = eHead "getTagIDContent" 


getTagContents2 :: Eq str => ([Attribute str] -> Bool) -> [Tag str] -> [[Tag str]]
getTagContents2 pAttrs tags =
   map (\((TagOpen name _):rest) -> takeWhile (not . tagCloseLit name) rest)
       $ sections (tagOpen (const True) pAttrs) tags


nDiv :: String
nDiv  = "div"
nId :: String
nId = "id"
nClass :: String
nClass = "class"
nPara :: String
nPara = "p"

sMainStage :: Selector
sMainStage = (nDiv @: [nId @= "divMainStage"])
             // (nDiv @: [nClass @= "quickfacts left"])

test2b
  :: String -> IO [[Tag T.Text]]
test2b url =  withSocketsDo $
         do bs <- decodeUtf8 <$> BL.toStrict <$> simpleHttp url
            let tags = parseTags bs
                s1 = Sc.select sMainStage tags
            return s1    

scrapeUrl :: String -> IO (Maybe [(T.Text, T.Text)])
scrapeUrl url =  withSocketsDo $
         do putStrLn $ "Scraping: " ++ url
            bs <- catch (decodeUtf8 <$> BL.toStrict <$> simpleHttp url)
                        (\e -> do let err = show (e :: HttpException)
                                  putStrLn err
                                  return "")
            return $! parsePage bs

findPrefixed :: T.Text -> [T.Text] -> T.Text
findPrefixed prefix txts = case find (T.isPrefixOf prefix) txts of
                              Just txt -> (T.words txt) !! 1
                              Nothing -> ""

firstPrefixed :: T.Text -> T.Text -> [Tag T.Text] -> T.Text
firstPrefixed tt prefix = T.strip . fromTagText . (eHead "addr")
                 . (getTagContent tt 
                    (\attr' -> anyAttrValue (T.isPrefixOf prefix) attr'))


attrPrefix :: T.Text -> T.Text -> [Attribute T.Text] -> Bool
attrPrefix at prefix = anyAttr (\(n, v) -> n == at
                                              && T.isPrefixOf prefix  v)

parsePage :: T.Text -> Maybe [(T.Text, T.Text)]
parsePage bs =  
    let tags = parseTags bs
        s1 = Sc.select sMainStage tags
        txts = map T.strip $ mapMaybe maybeTagText tags in
    if null s1 then Nothing
    else
      let s1h = eHead "s1h" s1
          bj = findPrefixed "Baujahr" txts
          id1 = findPrefixed "Online-ID:" txts
          addr = firstPrefixed "div" "location" tags   
          hardfacts = getTagContents "div" (attrPrefix "class" "hardfact ") s1h
          hf = map (mapMaybe extractTexts) hardfacts
          hf1 = map (\([a,b]) -> (b,a)) hf            
 
          divImmo = getTagIDContent "div" "id" "divImmobilie" tags

          merkmale = getTagContents2 (attrPrefix "class" "merkmale") divImmo 
          mm = concatMap (mapMaybe extractTexts) merkmale
          mm1 :: [(T.Text, T.Text)]
          mm1 = map (, "x") $ concatMap (T.splitOn ",") mm 

          mietPreis = Sc.select (nPara @: [nClass @= "mietpres_legende"]) tags
          mp = firstTextWord mietPreis
      in  Just $ [("id", id1), ("addr", addr), ("baujahr", bj), ("Ave.P", mp)]
                     ++ hf1 ++  mm1


firstTextWord :: [[Tag T.Text]] -> T.Text
firstTextWord tags
    = if not $ null tags then
      ((!! 1) . T.words . (eHead "mp"))
       $ mapMaybe (removeEmptyString . Just . T.strip)
       $ mapMaybe maybeTagText $ eHead "mp1" tags
      else ""


bpair :: [T.Text] -> (T.Text, T.Text)
bpair (a:b:_) = (b,a)
bpair [x] = (x, "x")
bpair _ = error "bpair"

firstWord :: T.Text -> T.Text
firstWord = T.takeWhile (not . isSpace)

removeEmptyString :: Maybe T.Text -> Maybe T.Text
removeEmptyString = boolJust (not . T.null)

extractTexts :: Tag T.Text -> Maybe T.Text
extractTexts = removeEmptyString
                          . (fmap $ firstWord . T.strip)
                          . maybeTagText 

csvOptions :: CS.EncodeOptions
csvOptions = CS.defaultEncodeOptions { CS.encDelimiter = fromIntegral (ord ';')  }

csvExport
  :: [[(T.Text, T.Text)]]
     -> BL.ByteString
csvExport content = CS.encodeByNameWith csvOptions header values 
   where values = L.map (\rec1 -> M.fromList rec1 `M.union` defaultValues) content 
         names = allCollNames content
         leadingColumns = map encodeUtf8 ["url", "addr", "Kaufpreis"
                          , "Wohnfläche", "Ave.P", "baujahr", "Zimmer", "id"]
         orderedNames = leadingColumns  ++ (names \\ leadingColumns)
         header = CS.header orderedNames
         defaultValues = M.fromList $ map (\n -> (decodeUtf8 n,"")) orderedNames

allCollNames :: [[(T.Text, T.Text)]] -> [BS.ByteString]
allCollNames recs = Set.toList $ Set.unions $ map (Set.fromList . map (encodeUtf8 . fst)) recs

-- test3 url = csvExport <$> scrapeUrl url
test4 :: FilePath -> [String] -> IO ()
test4 fname urls = BL.writeFile fname =<< csvExport <$> catMaybes <$> mapM scrapeUrl urls

test5 :: FilePath -> IO ()
test5 fname
      = do mailLinks <- readEmails
           rawRecs <- mapM (\urls -> catMaybes
                                      <$> mapM (scrapeUrl . BC.unpack) urls)
                                           mailLinks
           writeFile fname $ show rawRecs 
           --BL.writeFile fname $ csvExport (concat rawRecs)
            
test6 :: FilePath -> IO ()
test6 fn = do rawRecs <- read <$> readFile "all.text"
              BL.writeFile fn $ csvExport $ concat rawRecs

test7 :: FilePath -> IO ()
test7 fn = do pages <- dbReadPages
              rawRecs <- mapM (\(url, page) -> do
                                 TIO.putStrLn url
                                 let p = parsePage $ decodeUtf8 page
                                 -- print p
                                 return $! (("url", url):) <$> p )
                   $ take 10000 pages
              putStrLn "Writing csv file"
              -- writeFile fn $ show rawRecs 
              BL.writeFile fn $ csvExport $ catMaybes rawRecs


boolJust :: (a -> Bool) -> Maybe a -> Maybe a
boolJust p (Just v) = if p v then Just v else Nothing
boolJust _ _ = Nothing

{--
te = do hSetEncoding stdout utf8
        hPutStrLn stdout tWohn   
        return ()

te1 = do txt <- readFile "Wohn.txt"
         writeFile "Wohn1.txt" txt  
         hPutStrLn stdout tWohn
         putStrLn txt

te2 = do let wohnQP = "Wohnfl=C3=A4che"
         BS.writeFile "Wohn2.txt" $ encodeUtf8 $ T.pack $ QP.decode wohnQP
--}         
