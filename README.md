# suspicious
### an R package for seeking out suspicious data frame qualities

based on [Quartz's](https://github.com/Quartz/bad-data-guide) guide to bad data, this package provides functions for doing data checks for common errors, such as the use of 99999 for NA, invalid or faked data (i.e. zip code is 12345 but you aren't in Schenectady) or problems with data tables being cut off (e.g. Apple Numbers app cuts off after 255 rows, which can result in missing data). 

The project is in development and is not yet in packaged form. 
