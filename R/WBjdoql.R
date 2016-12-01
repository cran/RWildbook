#' Generate the JDOQL query for the search in the Wildbook framework.
#'
#' This function generate the JDOQL query string according to the filters specified by users. The JDOQL query is an essential part of the search URL.
#'
#' @param object can be either "encounter" for the encounter search or "individual" for the individual search.
#'
#' @param location A string of character contained in location names.
#'
#' @param locationID A character vector for filtering the locationID.
#'
#' @param sighting_date A character for filtering encounters which are sighted during a period of time.
#'
#' @param encounter_submission_dates A character for filtering encounters which are submitted during a period of time.
#'
#' @param date_format The format for all the arguments of date valule.
#'
#' @param sex A character vector of maximum size of three represents the value for the sex filter.
#'
#' @param status A character vector of maximum size of two represents the value for the encounter status.
#'
#' @param measurement A numeric object sets the minimum individual measurement when searching in the Wildbook framework.
#'
#' @param individualID A character vector for searching data of specific individual ID.
#'
#' @param encounterID A character vector for searching data of specific encounter ID.
#'
#' @param encounter_type A character vector of maximum size of three for searching data with specific encounter type.
#'
#' @param Date_of_birth A character vector for searching data of individual which is borned during a period of time.
#'
#' @param Date_of_death A character vector for searching data of individual which is dead during a period of time.
#'
#'
#'
WBjdoql <-
function(object="encounter",
         location=NULL,locationID=NULL,
         sighting_date=c("1964-01-01","2016-12-31"),encounter_submission_dates=c("2003-01-01","2016-12-31"), date_format="%Y-%m-%d",
         sex=c("male","female","unknown"),status=c("alive","dead"),
         measurement=NULL,individualID=NULL,
         encounterID=NULL,encounter_type=NULL,
         Date_of_birth=NULL,Date_of_death=NULL){
  #Set the comment part of the JDOQL query according to the object value
  if(tolower(object)=="encounter"){
    add_name<-NULL
    object<-"org.ecocean.Encounter"
    enc<-"catalogNumber != null && "
    variables<-NULL
  } else if(tolower(object)=="individual"){
    add_name<-"enc."
    object<-"org.ecocean.MarkedIndividual"
    enc<-"encounters.contains(enc) && "
    variables<-"VARIABLES org.ecocean.Encounter enc"
  } else {
    warning("A valid object value should be either 'encounter' or 'individual'.")
    break
  }
  if(!is.null(measurement)){
    variables <- c(variables,"org.ecocean.Measurement measurement0")
    measurement <- paste0(add_name,"measurements.contains(measurement0) && (measurement0.value >= ",measurement,") && (measurement0.type == 'length')")
  }
  variables<-paste(variables,collapse = ";")
  #Generate the corresponding filter strings in the JDOQL query according to the filter variables values.
  if(!is.null(location)){
    location<-paste(location, collapse = " ")
    location<-paste0(add_name,"verbatimLocality.toLowerCase().indexOf(??",location,"')")
    location<-filterstring(filtername = location, filtervalues = -1, bridge = "!=")
  }
  locationID<-filterstring(filtername = paste0(add_name,"locationID"), filtervalues = locationID)
  sighting_date<-dateTOmillisecond(date=sighting_date,origin="1970-01-01",format=date_format)
  sighting_date<-filterstring(filtername = paste0(add_name,"dateInMilliseconds"), filtervalues = sighting_date, logic="&&", bridge=c(">=","<="))
  encounter_submission_dates<-dateTOmillisecond(date=encounter_submission_dates,origin="1970-01-01",format=date_format)
  encounter_submission_dates<-filterstring(filtername = "dwcDateAddedLong", filtervalues = encounter_submission_dates, logic="&&", bridge=c(">=","<="))
  Date_of_birth<-dateTOmillisecond(date=Date_of_birth,origin="1970-01-01",format=date_format)
  Date_of_birth<-filterstring(filtername = paste0(add_name,"timeOfBirth"), filtervalues = Date_of_birth, logic="&&", bridge=c(">=","<="))
  Date_of_death<-dateTOmillisecond(date=Date_of_death,origin="1970-01-01",format=date_format)
  Date_of_death<-filterstring(filtername = paste0(add_name,"timeOfDeath"), filtervalues = Date_of_death, logic="&&", bridge=c(">=","<="))
  sex<-sexstring(sex)
  status_range<-c("alive","dead")
  if(length(status)<2){
    status<-paste0(add_name,"!livingStatus.startsWith('",status_range[-which(status_range==status)],"')")
  }else status<-NULL
  individualID<-filterstring(filtername = "individualID", filtervalues = individualID)
  encounterID<-filterstring(filtername = "EncounterID", filtervalues = encounterID)
  if(length(encounter_type) < 3){
    encounter_type<-filterstring(filtername = "state",filtervalues = encounter_type)
  }else encounter_type<-NULL
  #Create the filter vector with all the filter strings
  filters<-c(location,locationID,
             sighting_date,encounter_submission_dates,
             sex,status,measurement,individualID,encounterID,encounter_type,
             Date_of_birth,Date_of_death)
  #Keep those filters which are not "NULL"
  filters<-filters[!is.null(filters)]
  #Connect all the not "NULL" filter strings with "&&" which refers to logical AND
  filters<-paste0(filters,collapse = "&&")
  #Generate the complete JDOQL query according to the values in the argument
  filters<-paste("SELECT FROM", object, "WHERE", enc,  filters, variables)
  #"filters" is a character R object
  return(filters)
}
