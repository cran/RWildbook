#' Identify operating system as Mac, *nix, or Windows
#'
#' Copied from Hadley Wickham's rappdir package at his suggestion. The
#' rappdir package currently does not export this function
#'
#' @return Operating system name (character string).
#'
get_os <- function() {
  if (.Platform$OS.type == "windows") {
    "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "mac"
  } else if (.Platform$OS.type == "unix") {
    "unix"
  } else {
    stop("Unknown OS")
  }
}
