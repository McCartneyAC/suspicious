## suspicious
# An R Packages based on Quartz's Guide to Bad Data
# https://github.com/Quartz/bad-data-guide
# Contains functions to check for common dataset problems

#' Check Dataframe Dimensions for common cut offs
#' Spreadsheet has 65536 rows
#' The maximum number of rows an old-fashioned Excel spreadsheet was allowed to have was 65,536.
#' If you receive a dataset with that number of rows you have almost certainly been given
#' truncated data. Go back and ask for the rest. Newer versions of Excel allowed for 1,048,576
#' rows, so it's less likely you'll be working with data that hits the limit.
#
#' Spreadsheet has 255 columns
#' Apple's Numbers app can only handle spreadsheets with 255 columns, and the app will
#' truncate files that have more columns without warning the user. If you receive a dataset
#' with exactly 255 columns, ask if the file was ever opened or converted with Numbers.
#'
#' @param df a dataframe
#' @return an evaluation of whether the dataframe may have been cut off prior to import
#'
#' @example suspect_frame_dimensions(iris)
#'
suspect_frame_dimensions<-function(df){

  problems<-c(65536 , 1048576 , 65535 , 1048575, 255, 254)
  if (nrow(df) %in% problems) {
    print("your number of rows may be problematic")
  } else if (ncol(df) %in% problems){
    print("your number of columns may be problematic")
  } else {
    print("Data frame dimensions -- no problems detected")
  }
}




#' Check for Common Bad Numbers
#' Checks for numbers that are associated with missing data e.g. 99999 or are
#' associated with encoding limits such as 255 or 65535
#'
#' @param x a vector of class numeric
#' @return an evaluation of whether the vector contains any suspicious numbers
#'
#' @example suspect_numeric(iris$Sepal.Length)
#'
suspect_numeric<-function(x){
  numlist<-c(65535, 255, 2147483647, 4294967295, 99999, 00000, 9999, 0000, 999, 000)
  # common error values and placeholder values
  if (any(x %in% numlist)){
    return("Some of the numbers in this list look suspicious")
  } else {
    return("No suspicious numbers detected")
  }
}


#' Check for Fictitious Telephone Numbers
#' Checks for phone numbers containing the sequence 555 (in any location), which is commonly
#' used to denote a fictitious number. Also checks whether the common faux number 867-5309
#' is within the vector.
#'
#'
#' @param x a vector of phone numbers that may be stored as text.
#' @return an evaluation of whether the vector contains any suspicious numbers
#'
#'
#'
suspect_telephone<-function(x){
  # 555- area code is used for fictitious numbers
  # you should also see if Jenny is in your dataset: 867-5309
  nums<-gsub("\\D", "", x)
  if (sum(grepl('555', nums, fixed = TRUE)) != 0) {
    print("Check your numbers--some of them may be fictitious")
  } else if (sum(grepl('8675309', nums, fixed = TRUE)) != 0) {
    print("Check your numbers--Jenny is in your dataset")
  } else {
    ("No suspicious phone numbers detected")
  }
}



#' Check for Common Date Problems
#' Checks to determine whether any dates in a list are one of two problems: (1) are they the commonly-used
#' excel dates given when a date = 0 (which are different for windows and mac) or (2) the Unix Epoch of Jan
#' 1, 1970. The former is given as a class date, the later as a class date-time.
#'
#'
#' @param x a vector of class date or datetime
#' @return an evaluation of whether the vector contains any suspicious dates
#'
#'
#'
suspect_dates<-function(x){
  datelist<-as.Date(c("1900-01-01", "1904-01-01")) # Common start dates for excel
  datetimelist<-as.POSIXct(c("1970-01-01 00:00:00", "1969-12-31 23:59:59")) # an epoch change
  if (class(x)=="Date") {
    if (any(x %in% datelist)){
      print("Check your dates--are your data around the turn of the century?")
    } else {
      print("No suspicious dates detected")
    }
  } else if (sum(class(datetimelist)=="POSIXct") != 0) {
    if (any(x %in% datetimelist)) {
      print("Check your dates--are your data around 1970?")
    } else {
      print("No suspicious dates detected")
    }
  } else {
    print("vector must be of class date or datetime")
  }
}


#' Check for Unusual Longitude/Latitude
#' Simply checks if any columns of longitude and latitude data are simultaneously 0 and 0,
#' potentially indicating incorrectly coded data.
#'
#'
#' @param long a vector of longitude values
#' @param lat  a vector of latitude values
#' @return an evaluation of whether the two vectors contain 0N 0E
#'
suspect_locations<-function(long, lat){
  if (any(long ==0 & lat == 0)) {
    print("Are you at the poles?")
  }
  else {
    print("No suspicious locations detected")
  }
}


#' Check for Suspicious Zip Codes
#' Checks to determine whether potentially NA values are given (e.g. 99999) for a zip code, or
#' if 12345 or 90210 are given for a zip code.
#'
#'
#' @param x a vector of zip codes
#' @return an evaluation of whether the two vectors contain any of the four identified problems.
#'
suspect_zips<-function(x){
  ziplist<-c(12345, 90210, 00000, 99999) #Schenectady, NY and Beverly Hills, CA, plus two placeholders
  if(any(x %in% ziplist)) {
    print("Check your zip codes--are you in Schenectady or Beverly Hills?")
  } else {
    print("No suspicious Zip Codes Detected")
  }
}

#' Provides a visual test of Benford's Law
#' Requires ggplot2
#'
#' Benford's Law fails
#' Benford's Law is a theory which states that small digits (1, 2, 3) appear at the beginning of numbers much more
#' frequently than large digits (7, 8, 9). In theory Benford's Law can be used to detect anomalies in accounting
#' practices or election results, though in practice it can easily be misapplied. If you suspect a dataset has
#' been created or modified to deceive, Benford's Law is an excellent first test, but you should always verify
#' your results with an expert before concluding your data have been manipulated
#'
#' @param x a vector of class numeric
#' @return a ggplot image with frequencies of initial numerals plotted against the theoretical
#' values given by Benford's Law
#'
#'@example suspect_benford(iris$Sepal.Length)
suspect_benford<-function(x){
  bens<-c(.301, .176, .125, .097, .079, .067, .058, .051, .046) * length(x/9)
  benfords<-as.data.frame(cbind(n = 1:9, bens))
  txt <-substr(as.character(x), 1,1)
  dat <-as.data.frame(table(txt))
  ggplot2::ggplot(dat, ggplot2::aes(txt, Freq)) + ggplot2::geom_col() +
    ggplot2::geom_line(data=benfords, ggplot2::aes(x = n, y = bens))+
    ggplot2::geom_point(data=benfords, ggplot2::aes(x = n, y = bens)) + ggplot2::labs(
      title = "Benford's Law Visual Check",
      x = "digit",
      y = "frequency"
    )
}

