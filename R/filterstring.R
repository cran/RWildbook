#' Generate the JDOQL part of a filter.
#'
#' This function is to generate the string in the JDOQL query for some given value of a filter.
#'
#' @param filtername A character which is for the responding variable name in Wildbook framework.
#'
#' @param filtervalues A vector of the value for the filter.
#'
#' @param logic A parameter which can be "&&" for the logical AND or "||" for the logical OR.
#'
#' @param bridge An operator to connect the name and the default value is "==".
#'


filterstring <-
  function(filtername,filtervalues,logic="||",bridge="=="){
    #This function is to generate the string in the JDOQL query for some given value of a filter.
    #"filtername" is a character which is for the responding variable name in Wildbook framework
    #"filtervalue" is a vector of value for the filter
    #The value of "logic" can be either "&&" for the logical AND or "||" for the logical OR.
    #"bridge" is the operator to connect the name and the default value is "==".
    numeric <- is.numeric(filtervalues)
    if(!missing(filtervalues)&&(!is.null(filtervalues))){
      if(numeric==FALSE) filtervalues<-paste0("'",filtervalues,"'")
      filterstring<-paste("(",filtername,bridge,filtervalues,")",collapse = logic)
      paste0("(",filterstring,")")
    } else return(NULL)

  }

#examples
#filterstring(filtername = "locationID", filtervalues = c("1","1a"))
