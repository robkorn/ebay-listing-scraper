{-# LANGUAGE OverloadedStrings #-}

module Ebay.Scraper.Listing where

import           Control.Applicative          ((<|>))
import           Control.Monad.Reader         (Reader, runReader, asks)
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.ByteString.Lazy.Search  as BSLS
import           Data.ByteString.Lazy.Char8   (pack, unpack)
import           Data.Maybe                   (fromMaybe)
import           Extra.Util.Func              (getRequestBody, cleanupText, extractCurrency)
import qualified Network.Wreq                 as W
import           Text.HTML.Scalpel.Core       ((@:), (@=), Selector, scrapeStringLike, text, attr)

type ListingID = BSL.ByteString
type URL = BSL.ByteString
type ZipCode = BSL.ByteString

-- | Contains all scraped data from the Ebay Listing Page.
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
                             isAuction :: Bool,
                             locale :: SiteLocale
                           } deriving (Show)

-- | Contains intermediary data used for the environment for Reader.
data ListingData = ListingData { url :: URL,
                                 lID :: ListingID,
                                 zipCode :: ZipCode,
                                 listingHTML :: BSL.ByteString,
                                 shippingHTML :: BSL.ByteString,
                                 descriptionHTML :: BSL.ByteString
                               } deriving (Show)

-- | Type describing the locale of the listing.
data SiteLocale = US | Canada | Other deriving Show


-- | Creates an 'EbListing' with provided 'ZipCode'(for shipping prices) and with listing 'URL'
createEbListing :: ZipCode -> URL -> IO EbListing
createEbListing zipCode url = do
  listingHTML <- getRequestBody url
  let lID = fromMaybe " " $ scrapeID listingHTML
  descriptionHTML <- getRequestBody $ BSL.concat ["http://vi.vipr.ebaydesc.com/ws/eBayISAPI.dll?ViewItemDescV4&item=", lID]
  shippingHTML <- getRequestBody $ buildShippingURL zipCode url lID
  let lData = ListingData { url = url,
                            lID = lID,
                            zipCode = zipCode,
                            listingHTML = listingHTML,
                            descriptionHTML = descriptionHTML,
                            shippingHTML = cleanupText $ BSL.drop 48 $ shippingHTML}
  return $ runReader buildEbListing lData


-- | Builds EbListing via Reader
buildEbListing :: Reader ListingData EbListing
buildEbListing = do
  title <- scrapeTitle
  image <- scrapeImage
  note <- scrapeNote
  condition <- scrapeCondition
  description <- scrapeDescription
  price <- scrapePrice
  shipping <- scrapeShipping
  sellerName <- scrapeSellerName
  sellerRating <- scrapeSellerRating
  lID <- asks lID
  timeLeft <- scrapeTimeLeft
  isCAD <- isCADListing
  isAuction <- isAuctionCheck
  curLocale <- determineSiteLocale
  return EbListing { title = title,
                     image = image,
                     note = note,
                     condition = condition,
                     description = description,
                     price = price,
                     shipping = shipping,
                     totalPrice = (+) <$> price <*> shipping,
                     sellerName = sellerName,
                     sellerRating = sellerRating,
                     listingID = lID,
                     timeLeft = timeLeft,
                     isCAD = isCAD,
                     isAuction = isAuction,
                     locale = curLocale
                     }



-- | Determines site locale.
determineSiteLocale :: Reader ListingData SiteLocale
determineSiteLocale = do
  u <- asks url
  case null (BSLS.indices ".ca" u) of
    False -> pure Canada
    True -> case null (BSLS.indices ".com" u) of
      False -> pure US
      True -> pure Other

-- | Creates the 'URL' used to obtain accurate shipping rates for the given ebay domain.
buildShippingURL :: ZipCode -> URL -> ListingID -> URL
buildShippingURL zipCode listingURL lID = BSL.concat ["http://www.ebay",domain,"/itm/getrates?item=",lID,"&country=",country,"&cb=&co=0","&zipCode=", zipCode]
  where isAmerican = null (BSLS.indices ".ca" listingURL)
        domain = if isAmerican then ".com" else ".ca"
        country = if isAmerican then "1" else "2"

-- | Scrapes ID from Ebay Listing webpage html.
scrapeID :: BSL.ByteString -> Maybe (BSL.ByteString)
scrapeID webpage = scrapeStringLike webpage (text listingIDSelector)
  where listingIDSelector = "div" @: ["id" @= "descItemNumber"] :: Selector

-- | Returns whether the Ebay Listing is in CAD currency.
isCADListing :: Reader ListingData Bool
isCADListing = do
  lHTML <- asks listingHTML
  case scrapeStringLike lHTML (text conversionSelector) of
    Just t -> return False
    Nothing -> return True
  where conversionSelector = "div" @: ["id" @= "prcIsumConv"]

-- | Verifies if the Ebay Listing is an auction.
isAuctionCheck :: Reader ListingData Bool
isAuctionCheck = do
  lHTML <- asks listingHTML
  case scrapeStringLike lHTML (text auctionSelector) of
    Just t -> return True
    Nothing -> return False
  where auctionSelector = "span" @: ["id" @= "qty-test"]

-- | Scrapes title from Ebay Listing webpage html.
scrapeTitle :: Reader ListingData (Maybe BSL.ByteString)
scrapeTitle = asks listingHTML >>= return . fmap cleanupTitle . flip scrapeStringLike (text titleSelector)
  where titleSelector = "h1" @: ["id" @= "itemTitle"]
        cleanupTitle = cleanupText . (\x -> if detailsInTitle x then removeDetails x else x)
        detailsInTitle t = not $ null (BSLS.indices "Details about" t)
        removeDetails = BSL.drop 15

-- | Scrapes image from Ebay Listing webpage html.
scrapeImage :: Reader ListingData (Maybe BSL.ByteString)
scrapeImage = asks listingHTML >>= return . flip scrapeStringLike (attr "src" imageSelector)
  where imageSelector = "img" @: ["id" @= "icImg"]

-- | Scrapes note from Ebay Listing webpage html.
scrapeNote :: Reader ListingData (Maybe BSL.ByteString)
scrapeNote = asks listingHTML >>= return . flip scrapeStringLike (text noteSelector)
  where noteSelector = "span" @: ["class" @= "viSNotesCnt"]

-- | Scrapes condition from Ebay Listing webpage html.
scrapeCondition :: Reader ListingData (Maybe BSL.ByteString)
scrapeCondition = asks listingHTML >>= return . flip scrapeStringLike  (text conditionSelector)
  where conditionSelector =  "div" @: ["id" @= "vi-itm-cond"]

-- | Scrapes price from Ebay Listing webpage html.
scrapePrice :: Reader ListingData (Maybe Float)
scrapePrice = do
  lHTML <- asks listingHTML
  return $ extractCurrency $ fromMaybe "" $ priceScrape usPriceSelector lHTML <|> priceScrape cadPriceSelector lHTML <|> priceScrape salePriceSelector lHTML
  where priceScrape sel = flip scrapeStringLike (text sel)
        usPriceSelector = "div" @: ["id" @= "prcIsumConv"]
        cadPriceSelector = "span" @: ["itemprop" @= "price"]
        salePriceSelector = "span" @: ["id" @= "mm-saleDscPrc"]

-- | Scrapes seller name from Ebay Listing webpage html.
scrapeSellerName :: Reader ListingData (Maybe BSL.ByteString)
scrapeSellerName = asks listingHTML >>= return . fmap (BSL.drop 1) . flip scrapeStringLike (text sellerNameSelector)
  where sellerNameSelector = "a" @: ["id" @= "mbgLink"]

-- | Scrapes seller rating from Ebay Listing webpage html.
scrapeSellerRating :: Reader ListingData (Maybe BSL.ByteString)
scrapeSellerRating = asks listingHTML >>= return . fmap cleanupText . flip scrapeStringLike (text sellerRatingSelector)
  where sellerRatingSelector = "div" @: ["id" @= "si-fb"]

-- | Scrapes time left from Ebay Listing webpage html.
scrapeTimeLeft :: Reader ListingData (Maybe BSL.ByteString)
scrapeTimeLeft = asks listingHTML >>= return . fmap cleanupText . flip scrapeStringLike (text timeLeftSelector)
  where timeLeftSelector = "span" @: ["id" @= "vi-cdown_timeLeft"] :: Selector

-- | Scrapes shipping from Ebay Listing webpage html.
scrapeShipping :: Reader ListingData (Maybe Float)
scrapeShipping = do
  shipHTML <- asks shippingHTML
  return $ getShipping $ fromMaybe " " $ shipScrape shipHTML usShippingSelector <|> shipScrape shipHTML cadShippingSelector
    where shipScrape shipHTML sel = scrapeStringLike shipHTML (text sel)
          usShippingSelector = "span" @: ["id" @= "convetedPriceId"]
          cadShippingSelector = "span" @: ["id" @= "fshippingCost"]
          getShipping t = if null (BSLS.indices "FREE" t) then extractCurrency t else Just 0.00

-- | Scrapes description based on Ebay 'ListingID'.
scrapeDescription :: Reader ListingData (Maybe BSL.ByteString)
scrapeDescription = asks descriptionHTML >>=  return . fmap cleanupText . flip scrapeStringLike (text descriptionSelector)
    where descriptionSelector =  "div" @: ["id" @= "ds_div"]

