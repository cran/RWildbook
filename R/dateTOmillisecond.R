#' Transform a vector of date to a vector of millisecond.
#'
#' This function is to transform a period of time in date to millisecond according to the origin date and the format of date.
#'
#' A vector of size two represent a period of time. The start date of the period will be transform to the first millisecond of the date and the end of the period to the last millisecond.
#'
#' @param date A character vector represent a period of time from date[1] to date[2].
#'
#' @param origin A point of time which set to be zero in millisecond.
#'
#' @param format A format for date and origin arguments.
#'
#' @param interval A logical argument which equals to TRUE by default when the function transfer an period of dates to millisecond.
#' When interval=FALSE, the function returns the last minute of every elements of date to millisecons.
#'

dateTOmillisecond <-
function(date,origin="1970-01-01",format="%Y-%m-%d",interval=TRUE){
  #If length of "date" is one, the function returns the millisecond of the last minute of the date.
  #If length of "date" is two, the "date" argument means from date[1] to date[2].
  #This function will transform the start minute of date[1] to millisecond to the last minute of date[2] according to the origin date.
  #interval, A logical argument which equals to TRUE by default when the function transfer an period of dates to millisecond.
  #When interval=FALSE, the function returns the last minute of every elements of date to millisecons.
  if(interval==FALSE) {
    if(!is.null(date)){
      date1 <- (as.numeric(as.Date(date, origin = "1970-01-01", format))* 86400000)+86340000
    }else{
      date1 <- NULL
    }
  }else{
    if(length(date)==2){
      date1<-c((as.numeric(as.Date(date[1], origin = "1970-01-01", format))* 86400000),(as.numeric(as.Date(date[2], origin = "1970-01-01", format))* 86400000)+86340000)
      #The first element of "date" should smaller than the second one
      #Otherwise, function return the milliseconds in ascending order automatically with warning message
      if((date1[2]-date1[1]) < 0) {
        warning("The 'date' value should be in ascending order")
        date1 <- c((as.numeric(as.Date(date[2], origin = "1970-01-01", format))* 86400000),(as.numeric(as.Date(date[1], origin = "1970-01-01", format))* 86400000)+86340000)
      }
    }else date1 <- NULL
  }
  return(date1)
}
#examples
#dateTOmillisecond(date=c("1970-01-01","1970-02-01"),interval=TRUE)
##When the first date is after the second one
#dateTOmillisecond(date=c("1970-02-01","1970-01-01"),interval=TRUE)
#
##examples in different formats
#dateTOmillisecond(date=c("1970-01-01","1970-02-01"),format="%Y-%d-%m",interval=TRUE)
#dateTOmillisecond(date=c("1970/01/01","1970/02/01"),format="%Y/%m/%d",interval=TRUE)
#
##When "date" does not represent a period of time
#dateTOmillisecond(date=c("1970-01-01","1970-02-01"),interval=FALSE)
