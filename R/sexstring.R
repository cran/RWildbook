#' Generate the JDOQL part for the sex filter.
#'
#' This function is to generate the sex related portion in the JDOQL query.
#'
#' @param sex The value for the sex filter.
#'


sexstring <-
  function(sex){
    #This function is to generate the sex related portion in the JDOQL query
    sex_range<-c("male","female","unknown")
    sex_indicator<-apply(as.matrix(sex_range), MARGIN = 1, function(t) any(t==sex))
    sex<-c("!sex.startsWith('male')","!sex.startsWith('female')")
    if(sex_indicator[1]==1) sex[1]<-NA
    if(sex_indicator[2]==1) sex[2]<-NA
    if(sex_indicator[3]!=1) sex<-c(sex,"sex.startsWith('unknown') && sex != null")
    if(length(sex[!is.na(sex)])!=0){
      sex<-paste0(sex[!is.na(sex)],collapse = "&&")
      return(paste0("(",sex,")"))
    }else return(NULL)
  }
#examples
#sexstring("male")
#sexstring(sex="female")
#sexstring(sex="unknown")
#sexstring(c("male","female"))
#sexstring(c("female","male"))
#sexstring(c("female","unknown"))
#sexstring(c("male","unknown"))
#sexstring(sex=c("female","male","unknown"))
