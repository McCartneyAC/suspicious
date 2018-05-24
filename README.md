# suspicious
### an R package for seeking out suspicious data frame qualities

Based on [Quartz's](https://github.com/Quartz/bad-data-guide) guide to bad data, this package provides functions for doing data checks for common errors, such as the use of 99999 for NA, invalid or faked data (i.e. zip code is 12345 but you aren't in Schenectady) or problems with data tables being cut off (e.g. Apple Numbers app cuts off after 255 columns, which can result in missing data). 

## installation

````r
install.packages("devtools")

devtools::install_github("McCartneyAC/suspicious")
````


## An Example

One of the checks available is a visual check, using `ggplot2`, of the adherence of a vector of numbers to Benford's Law. Inputting a vector plots the frequency of each initial digit against the frequencies theorized by Benford's Law. This is sometimes used forensically to uncover forged financial documents, but proceed with caution; it's not a foolproof test of tampered data. 

````r
dat<-runif(1000, min = 1000, max = 9999)

suspicious::suspect_benford(dat)
````
Should result in: 


![Benford's Law Visual Check Example](http://people.virginia.edu/~acm9q/img/benford_runif.png) 
