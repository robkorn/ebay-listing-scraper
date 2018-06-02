{-# LANGUAGE OverloadedStrings #-}

module Ebay.Scraper.Listing where

import           Text.HTML.Scalpel.Core
import           Control.Applicative          ((<|>))
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.ByteString.Lazy.Search  as BSLS
import           Data.ByteString.Lazy.Char8  (pack, unpack)
import           Data.Maybe                  (fromMaybe)
import           Extra.Util.Func             (getRequestBody, cleanupText, extractCurrency)
import qualified Network.Wreq                 as W

type Webpage = BSL.ByteString
type ListingID = BSL.ByteString
type URL = BSL.ByteString
type ZipCode = BSL.ByteString

-- | Contains all scraped data from the Ebay Listing Page
data EbListing = EbListing { title :: Maybe BSL.ByteString,
                             image :: Maybe BSL.ByteString,
                             note :: Maybe BSL.ByteString,
                             condition :: Maybe BSL.ByteString,
                             description :: Maybe BSL.ByteString,
                             price :: Maybe Float,
                             shipping :: Maybe Float,
                             totalPrice :: Maybe Float,
                             sellerName :: Maybe BSL.ByteString,
                             sellerRating :: Maybe BSL.ByteString,
                             listingID :: BSL.ByteString,
                             timeLeft :: Maybe BSL.ByteString,
                             isCAD :: Bool,
                             isAuction :: Bool
                           } deriving (Show)



-- | Creates an 'EbListing'
createEbListing :: ZipCode -> URL -> IO EbListing
createEbListing zipCode url = do
  webpage <- getRequestBody url
  let lID = fromMaybe " " $ scrapeID webpage 
  let price = scrapePrice webpage
  shipping <- scrapeShipping lID (isAmericanEbay url) zipCode
  description <- scrapeDescription lID
  return EbListing { title = scrapeTitle webpage,
                     image = scrapeImage webpage,
                     note = scrapeNote webpage,
                     condition = scrapeCondition webpage,
                     description = description,
                     price = price,
                     shipping = shipping,
                     totalPrice = (+) <$> price <*> shipping,
                     sellerName = scrapeSellerName webpage,
                     sellerRating = scrapeSellerRating webpage,
                     listingID = lID,
                     timeLeft = scrapeTimeLeft webpage,
                     isCAD = isCADListing webpage,
                     isAuction = isAuctionCheck webpage
                     }

-- | Returns whether the url provided is American or not.
-- Only supports ".ca" and ".com"
isAmericanEbay :: URL -> Bool
isAmericanEbay url = null (BSLS.indices ".ca" url)

-- | Verifies if the Ebay Listing is an auction.
isAuctionCheck :: Webpage -> Bool
isAuctionCheck webpage = case scrapeStringLike webpage (text auctionSelector) of
  Just t -> True
  Nothing -> False
  where auctionSelector = "span" @: ["id" @= "qty-test"]

-- | Returns whether the Ebay Listing is in CAD currency.
isCADListing :: Webpage -> Bool
isCADListing webpage = case scrapeStringLike webpage (text conversionSelector) of
  Just t -> False
  Nothing -> True
  where conversionSelector = "div" @: ["id" @= "prcIsumConv"]

-- | Scrapes title from Ebay Listing webpage html.
scrapeTitle :: Webpage -> Maybe (BSL.ByteString)
scrapeTitle webpage = cleanupTitle <$> scrapeStringLike webpage (text titleSelector)
  where titleSelector = "h1" @: ["id" @= "itemTitle"]
        cleanupTitle = cleanupText . (\x -> if detailsInTitle x then removeDetails x else x)
        detailsInTitle t = not $ null (BSLS.indices "Details about" t)
        removeDetails = BSL.drop 15

-- | Scrapes image from Ebay Listing webpage html.
scrapeImage :: Webpage -> Maybe (BSL.ByteString)
scrapeImage webpage = scrapeStringLike webpage (attr "src" imageSelector)
  where imageSelector = "img" @: ["id" @= "icImg"]

-- | Scrapes note from Ebay Listing webpage html.
scrapeNote :: Webpage -> Maybe (BSL.ByteString)
scrapeNote webpage = scrapeStringLike webpage (text noteSelector)
  where noteSelector = "span" @: ["class" @= "viSNotesCnt"]

-- | Scrapes condition from Ebay Listing webpage html.
scrapeCondition :: Webpage -> Maybe (BSL.ByteString)
scrapeCondition webpage = scrapeStringLike webpage (text conditionSelector)
  where conditionSelector =  "div" @: ["id" @= "vi-itm-cond"]

-- | Scrapes price from Ebay Listing webpage html.
scrapePrice :: Webpage -> Maybe Float
scrapePrice webpage = extractCurrency $ fromMaybe "" $ priceScrape usPriceSelector <|> priceScrape cadPriceSelector
  where priceScrape sel = scrapeStringLike webpage (text sel)
        usPriceSelector = "div" @: ["id" @= "prcIsumConv"]
        cadPriceSelector = "span" @: ["itemprop" @= "price"]
        salePriceSelector = "span" @: ["id" @= "mm-saleDscPrc"]


-- | Scrapes seller name from Ebay Listing webpage html.
scrapeSellerName :: Webpage -> Maybe (BSL.ByteString)
scrapeSellerName webpage = (BSL.drop 1) <$> scrapeStringLike webpage (text sellerNameSelector)
  where sellerNameSelector = "a" @: ["id" @= "mbgLink"]

-- | Scrapes seller rating from Ebay Listing webpage html.
scrapeSellerRating :: Webpage -> Maybe (BSL.ByteString)
scrapeSellerRating webpage = cleanupText <$> scrapeStringLike webpage (text sellerRatingSelector)
  where sellerRatingSelector = "div" @: ["id" @= "si-fb"]

-- | Scrapes ID from Ebay Listing webpage html.
scrapeID :: Webpage -> Maybe (BSL.ByteString)
scrapeID webpage = scrapeStringLike webpage (text listingIDSelector)
  where listingIDSelector = "div" @: ["id" @= "descItemNumber"] :: Selector

-- | Scrapes time left from Ebay Listing webpage html.
scrapeTimeLeft :: Webpage -> Maybe (BSL.ByteString)
scrapeTimeLeft webpage = cleanupText <$> scrapeStringLike webpage (text timeLeftSelector)
  where timeLeftSelector = "span" @: ["id" @= "vi-cdown_timeLeft"] :: Selector

scrapeShipping :: Webpage -> Bool -> ZipCode -> IO (Maybe Float)
scrapeShipping lID isAmerican zipCode = do
  webpage <- getRequestBody url
  let wp = cleanupText $ BSL.drop 48 webpage
  return $ getShipping $ fromMaybe " " $ shipScrape wp usShippingSelector <|> shipScrape wp cadShippingSelector
    where shipScrape wp sel = scrapeStringLike wp (text sel)
          usShippingSelector = "span" @: ["id" @= "convetedPriceId"]
          cadShippingSelector = "span" @: ["id" @= "fshippingCost"]
          getShipping t = if null (BSLS.indices "FREE" t) then extractCurrency t else Just 0.00
          url = BSL.concat ["http://www.ebay",domain,"/itm/getrates?item=",lID,"&country=",country,"&cb=&co=0", "&zipCode=", zipCode]
          domain = if isAmerican then ".com" else ".ca"
          country = if isAmerican then "1" else "2"

-- | Scrapes description based on Ebay 'ListingID'.
scrapeDescription :: ListingID -> IO (Maybe BSL.ByteString)
scrapeDescription lID = do
  webpage <- getRequestBody url
  return $ cleanupText <$> scrapeStringLike webpage (text descriptionSelector)
    where descriptionSelector =  "div" @: ["id" @= "ds_div"]
          url = BSL.concat ["http://vi.vipr.ebaydesc.com/ws/eBayISAPI.dll?ViewItemDescV4&item=", lID]

