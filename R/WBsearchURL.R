#' Generate the search URL given the JDOQL query.
#'
#' This function helps users to generate the URL for data searching in the Wildbook framework with the account information of Wildbook, the URl of the desire wildbook and the JDOQL query.
#'
#' @param username The username in the Wildbook framework.
#'
#' @param password The password in the Wildbook framework.
#'
#' @param baseURL The URL represent the desire wildbook data base.
#'
#' @param jdoql The JDOQL string for data searching.
#'
#' @param protocol Defines communication protocol. either "http" or "https" (default).
#'
WBsearchURL <-
  function(username,
           password,
           baseURL,
           jdoql,
           protocol="https") {
    #This function is to call data from a wildbook site via JDO API
    #This function is for users who know JDOQL query language
    #The JDOQL query can be directly written by users
    #An example of "baseURL" is "whaleshark.org" (No "http://www.").

    if (!is.null(username) && !is.null(password)) {
      searchURL <- paste0(protocol,
                          "://",
                          username,
                          ":",
                          password,
                          "@",
                          baseURL,
                          "/rest/jdoql?",
                          jdoql)
    }
    else if(is.null(username) && is.null(password)) {
      searchURL <- paste0(protocol,"://www.", baseURL,
                          "/rest/jdoql?", jdoql)
    }
    else{
      stop(
        "You must supply both the username and password or neither.\n"
      )
    }
    return(as.character(searchURL))
  }
#examples
#searchURL1 <- WBsearchURL(username="xinxin",
#                         password="changeme",
#                         baseURL="whaleshark.org",
#                         jdoql="SELECT FROM org.ecocean.MarkedIndividual WHERE individualID == 'A-001'")
