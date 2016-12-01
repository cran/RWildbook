## Declare .SD from the data.table package as a global variable to pass CRAN check.
if(getRversion() >= "2.15.1")  utils::globalVariables(".SD","RWildbook",add=TRUE)

#' Format Wildbook data for mark-recaputure analysis
#'
#' Format data from \code{searchWB} function in \code{RWildbook} package for
#' mark-recapture analysis with \code{marked} and \code{RMark} packages.
#'
#' The \code{markedData} function format the wildbook data set that users search with
#' the \code{searchWB} fucntion for the mark-recapture analysis with \code{mark} and
#' \code{RMark} package. In \code{marked} package, users can process a certain form of
#' data set with \code{process.data} function in \code{marked} package. The \code{markedData}
#' function reutrns data set which can be the input data set of \code{process.data}.
#'
#' \strong{Defalut NULL value for end.dates argument}
#'
#' The default value for \code{end.date} argument are NULL which means the capture occasion intervals
#' are divided by the elements of \code{start.date} argument. In this case, the end date of the last
#' capture occasion is the value of \code{Sys.Date()}.
#'
#' \strong{The class of output}
#' The class of the ouput of \code{markedData} is "data.table" and "data.frame". With installing
#' the \code{data.table} package, the ouput is a data.table, otherwise it is a data.frame. That means
#' users can process the data with \code{data.table} package. Also users can directly process the output
#' with \code{process.data} function in \code{marked} package.
#'
#' @param data The raw data set from \code{searchWB} function in \code{RWildbook} package.
#'
#' @param varlist A character vector of the names of variables for mark-recapture analysis.
#'
#' @param varname_of_capturetime A character object which is the variable name for capture/encounter sighted time.
#'
#' @param start.dates A character vector of dates which are the start dates of the capture occasions.
#' The elements should be in the form of date_format.
#'
#' @param end.dates A character vector of dates which are the end dates of the capture occasions.
#' The elements should be in the form of date_format.
#'
#' @param date_format The format for all the arguments of date value.
#'
#' @param origin A point of time which set to be zero in millisecond.
#'
#' @param removeZeros If TRUE (default) then individuals with no captures are removed from the data.
#'
#' @importFrom data.table data.table
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' ## You will need to supply your own login information for whaleshark.org to
#' ## run these examples.
#' 
#' ## Load packages
#' library(marked)
#'
#' ## Extract data for individual A-001 through A-099
#' data1 <- searchWB(username="username",
#'                   password="password",
#'                   baseURL ="whaleshark.org",
#'                   object="Encounter",
#'                   individualID=paste0("A-0",rep(0:9,rep(10,10)),rep(0:9,10))[-1])
#'
#' ## Define start and end dates of capture occasions
#' start.dates1 <- paste0(1998:2016,"-01-01") #Define the start.date value
#' end.dates1 <- paste0(1998:2016,"-04-01") #Define the end.date value
#'
#' ## Format data for use in marked
#' markedData1.1 <- markedData(data = data1,
#'                              varname_of_capturetime = "dateInMilliseconds",
#'                              varlist = c("individualID"),
#'                              start.dates = start.dates1,
#'                              end.dates = NULL,
#'                              date_format = "%Y-%m-%d",
#'                              origin = "1970-01-01",
#'                              removeZeros = TRUE)
#'
#'
#' ## Fit simple CJS model in marked
#' markedData1.proc=process.data(markedData1.1,model="CJS",begin.time=1)
#' markedData1.ddl=make.design.data(markedData1.proc)
#' markedData1.cjs=crm(markedData1.proc,
#'                     markedData1.ddl,
#'                     model.parameters=list(Phi=list(formula=~time),p=list(formula=~time)))
#'
#' ## Format data including location as a covariate
#' markedData1.2 <- markedData(data = data1,
#'                            varname_of_capturetime = "dateInMilliseconds",
#'                            varlist = c("individualID","locationID"),
#'                            start.dates = start.dates1,
#'                            end.dates = end.dates1,
#'                            date_format = "%Y-%m-%d",
#'                            origin = "1970-01-01")
#' }                           
#'
markedData <-
  function(data,
           varname_of_capturetime = "dateInMilliseconds",
           varlist = c("individualID"),
           start.dates,
           end.dates = NULL,
           date_format = "%Y-%m-%d",
           origin = "1970-01-01",
           removeZeros = TRUE) {
    #This function is to process the raw data from searchWB{RWildbook} function for process.data{marked} function.
    #data, the raw dataset from searchWB{RWildbook} function.
    #varlist, a character vector of the names of variables for mark-recapture analysis
    #start.dates, a character vector of dates which are the start dates of the capture occasions.
    #end.dates, a character vector of dates which are the end dates of the capture occasions.
    #end.dates=NULL(default) means the end date of one capture occasion is the start point of the next capture occasion.
    #date_format sets the format of start.dates and end.dates.
    #origin sets the origin when transfering dates to milliseconds.

    #step 1. pull the needed variable from the raw data set.
    data <-
      data[c(varname_of_capturetime, varlist)] #abstract the encounter sighted date and individual ID.

    #Step 2. Transfer begin.date, end.date milliseconds.
    start.dates <-
      dateTOmillisecond(
        date = start.dates,
        format = date_format,
        origin = origin,
        interval = FALSE
      )
    end.dates <-
      dateTOmillisecond(
        date = end.dates,
        format = date_format,
        origin = origin,
        interval = FALSE
      )
    if (is.null(end.dates))
      end.dates <-
      c(start.dates[-1],
        dateTOmillisecond(date = format(Sys.Date(), date_format),
                          format = date_format,
                          origin = origin,
                          interval = FALSE))

    #Step 3. Check the validity of capture time interval.
    if (!length(start.dates) == length(end.dates)) {
      warning("start.dates and end.dates should have the same length")
      break()
    }
    if (any((end.dates - start.dates) < 0)) {
      warning("end.dates should be dates after start.dates")
      break()
    }

    #Step 4. generate the capture history of each individual from the encounter information.
    index1 <- outer(data[, 1], start.dates, FUN = ">")
    index2 <- outer(data[, 1], end.dates, FUN = "<")
    e.ch <- index1 * index2 #generate "ch" for each encounter

    #Step 5. turn encounter capture history to individual capture history
    ch.table <- data.table(e.ch, data[varlist])
    myfun <- function(x) as.numeric(any(x >= 1))
    mark.data <- ch.table[, lapply(.SD, myfun), by = varlist] #done!
    varname <- paste0("V", 1:length(start.dates))
    ch <-
      apply(mark.data[, varname, with = FALSE], 1, paste0, collapse =
              "")

    #Step 6. check for null capture histories
    if (removeZeros) {
      zeros <- which(ch == paste0(rep(0, length(start.dates)), collapse = ""))
      message("Note: Removing ",
              length(zeros),
              " individuals with no observed captures.\n")
    }
    else
      zeros <- c()

    # Step 7. create data table
    output <- data.table(mark.data[, varlist, with = FALSE], ch)

    # Return data
    if (length(zeros) > 0)
      return(output[-zeros,])
    else
      return(output)
  }
