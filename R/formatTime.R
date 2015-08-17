#' @title Formats Elapsed Time
#' @param proctime a proc_time object
#' @param digits the number of digits to round the output time to
#' @description Internal function used for debugging and printing output.
#' Cleans up the elapsed time of a proc_time object and prints it in
#' a format consistent with the rest of the output stream.
#' @return a formated time with "seconds" appended at the send.

formatTime <- function(proctime, digits=2) {
  elapsed <- (proc.time() - proctime)[[3]]
  elapsed <- round(elapsed, digits)
  elapsed <- paste0(elapsed, " seconds")
  return(elapsed)
}