# Ebay Listing Scraper

A library designed to easily scrape an ebay listing page in Haskell.

Must use listing page urls with /itm/, such as: "https://www.ebay.com/itm/ListingIDHereXXXX"

Now officially supporting both ebay.com & ebay.ca with proper currency scraping based off of the url provided!

Data Scraped Includes:
- Listing Title
- Image Thumbnail URL
- Seller Note
- Condition
- Description
- Price
- Shipping Price
- Seller Name
- Listing ID
- Time Left 


## How To Use

The simplest way to use this library is to call `createEbListing`:
```
createEbListing :: ZipCode -> URL -> IO EbListing
```

ZipCode and URL are ByteStrings so to use it you simply:

```
createEbListing "12345" "https://www.ebay.com/itm/12345678910"
```

The first argument is the Zip Code for the area you are shipping to, and the second is the url to the ebay listing page. 

`IO (EbListing)` is returned and from there you can access all of the scraped data via record syntax as such:
```
listing <- createEbListing "12345" "https://www.ebay.com/itm/12345678910"
title listing
condition listing
price listing
shipping listing
timeLeft listing
```

You now have access to all the data that was scraped from the page.


## Documentation

After cloning the repository run the following command to generate documentation:
```
stack haddock ebay-listing-scraper 
```
