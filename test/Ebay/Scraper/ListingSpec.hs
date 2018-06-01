{-# LANGUAGE OverloadedStrings #-}
module Ebay.Scraper.ListingSpec (spec) where

import           Ebay.Scraper.Listing
import           Test.Hspec

spec :: Spec
spec = describe "scrapePrice Test" $ do
  it "US Price" $
    scrapePrice usdPriceHTML `shouldBe` Just 49.99
  it "CAD Price" $
    scrapePrice cadPriceHTML `shouldBe` Just 7.99

usdPriceHTML = "<span class='notranslate' id='prcIsum' itemprop='price' style='' content='49.99'>US $49.99</span>"
cadPriceHTML = "<span class='notranslate' id='prcIsum_bidPrice' itemprop='price' content='7.99'>C $7.99</span>"
