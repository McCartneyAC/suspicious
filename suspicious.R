## suspicious
# An R Packages based on Quartz's Guide to Bad Data
# https://github.com/Quartz/bad-data-guide
# Contains functions to check for common dataset problems 



check_frame_dimensions<-function(df){
  # Spreadsheet has 65536 rows
  # The maximum number of rows an old-fashioned Excel spreadsheet was allowed to have was 65,536. 
  # If you receive a dataset with that number of rows you have almost certainly been given 
  # truncated data. Go back and ask for the rest. Newer versions of Excel allowed for 1,048,576 
  # rows, so it's less likely you'll be working with data that hits the limit.
  # 
  # Spreadsheet has 255 columns
  # Apple's Numbers app can only handle spreadsheets with 255 columns, and the app will 
  # truncate files that have more columns without warning the user. If you receive a dataset 
  # with exactly 255 columns, ask if the file was ever opened or converted with Numbers.
  problems<-c(65536 , 1048576 , 65535 , 1048575, 255, 254)
  if (nrow(df) %in% problems) {
    print("your number of rows may be problematic")
  } else if (ncol(df) %in% problems){
    print("your number of columns may be problematic")
  } else {
    print("Data frame dimensions -- no problems detected") 
  }
} 

check_frame_dimensions(dat)
nrow(dat)
check_numeric<-function(x){
  numlist<-c(65535, 255, 2147483647, 4294967295, 99999, 00000, 9999, 0000, 999, 000)
  # common error values and placeholder values
  if (any(x %in% numlist)){
    return("Some of the numbers in this list look suspicious")
  } else {
    return("No suspicious numbers detected")
  }
}

check_telephone<-function(x){
  # 555- area code is used for fictitious numbers
  # you should also see if Jenny is in your dataset: 867-5309
  
}

check_dates<-function(x, type = isdate){
  require(lubridate) 
  # TODO: use with POSIXct instead of lubridate
  # TODO: check automatically for type = datetime / posixct
  datelist<-as_date(c("1900-01-01", "1904-01-01")) #Common start dates
  datetimelist<-as_datetime(c(ymd_hms("1970-01-01 00:00:00"), ymd_hms("1969-12-31 23:59:59"))) # an epoch change
  if (type == isdate) {
    if (any(x %in% datelist)){
      print("Check your dates--are your data around the turn of the century?")
    } else {
      print("No suspicious dates detected")
    }
  }
  else if (type == isdatetime) {
    if (any(x %in% datetimelist)) {
      print("Check your dates--are your data around 1970?")
    }
    else {
      print("No suspicious dates detected")
    }
  } else {
    print("argument `type` is required: isdate or isdatetime are the only arguments")
  }
}

# check_locations<-function(x){
# TODO: determine how to check longitudes and latitudes
# e.g. force user to input two arguments for long and lat
#   # loclist<-c(0°00'00.0"N+0°00'00.0"E, 0°N 0°E)
# }

check_zips<-function(x){
  ziplist<-c(12345, 90210, 00000, 99999) #Schenectady, NY and Beverly Hills, CA, plus two placeholders
  if(any(x %in% ziplist)) {
    print("Check your zip codes--are you in Schenectady or Beverly Hills?")
    } else {
      print("No suspicious Zip Codes Detected")
    }
  }
      


check_benford<-function(x){
  require(ggplot2)
  # Benford's Law fails
  # Benford's Law is a theory which states that small digits (1, 2, 3) appear at the beginning of numbers much more 
  # frequently than large digits (7, 8, 9). In theory Benford's Law can be used to detect anomalies in accounting 
  # practices or election results, though in practice it can easily be misapplied. If you suspect a dataset has 
  # been created or modified to deceive, Benford's Law is an excellent first test, but you should always verify 
  # your results with an expert before concluding your data have been manipulated.
  bens<-c(30.1, 17.6, 12.5, 9.7, 7.9, 6.7, 5.8, 5.1, 4.6)
  benfords<-as.data.frame(cbind(n = 1:9, bens))
  txt <-substr(as.character(x), 1,1)
  tab <-sort(table(txt))
  dat <-as.data.frame(tab)
  ggplot(dat, aes(txt, Freq)) + geom_col() + 
    geom_line(data=benfords, aes(x = n, y = bens))+
    geom_point(data=benfords, aes(x = n, y = bens)) + labs(
      title = "Benford's Law Visual Check", 
      x = "digit", 
      y = "frequency"
    )
}


# graph_benford<-function(df){
#   require(ggplot2)
# }