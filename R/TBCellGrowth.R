#' @docType package
#' @name TBCellGrowth
#' @title Cell Microscopy Feature Extraction and Growth Tracking
#' @description A suite of functions for processing cell microscopy data,
#' specifically developed for tuberculosis. Features include GLCM based
#' feature extraction and blob labeling as well as tracking of colony growth 
#' over many frames. Cell growth output is available as CSV as well as a
#' variety of image overlays and animations.
NULL

#' @docType data
#' @keywords datasets
#' @name nameList
#' @title First Names
#' @format A character vector of length 4368.
#' @description A vector of 4368 unique first names obtained from (...). These names
#' are used to identify individual cell colonies being tracked. First names are much
#' easier to remember than IDs generated from a hashing algorithm or numeric XY
#' coordinates.
NULL

# ----- Internal Package State -------------------------------------------------

# Hidden
TBCellGrowthEnv <- new.env(parent = emptyenv())
TBCellGrowthEnv$RunOptions <- list()
TBCellGrowthEnv$ProfileStart <- NULL
TBCellGrowthEnv$ProfileTimepoint <- NULL
TBCellGrowthEnv$ProfileSecs <- list()

#' @keywords environment
#' @export
#' @title Set Run Options
#' @param opt list of options
#' @description Store command line options such as \code{verbose} and \code{profile}
#' so that they are available internally.
#' @return None
setRunOptions <- function(opt) {
  TBCellGrowthEnv$RunOptions <- opt
  # Guarantee that some options exist
  TBCellGrowthEnv$RunOptions[['verbose']] <- ifelse(is.logical(opt$verbose),opt$verbose,FALSE)
  TBCellGrowthEnv$RunOptions[['profile']] <- ifelse(is.logical(opt$profile),opt$profile,FALSE)
  TBCellGrowthEnv$RunOptions[['debug_images']] <- ifelse(is.logical(opt$debug_images),opt$debug_images,FALSE)
}

#' @keywords environment
#' @export
#' @title Get Run Options
#' @param option name of a specific option
#' @description Return the value of a particular option or, if \code{param} is not specified,
#' a list of command line options such as \code{verbose} and \code{profile}.
#' @return List of command line flags.
getRunOptions <- function(option=NULL) {
  if (!is.null(option)) {
    return(TBCellGrowthEnv$RunOptions[[option]])
  } else {
    return(TBCellGrowthEnv$RunOptions)
  }
}

#' @keywords environment
#' @export
#' @title Get Profile Secs
#' @description Returns a named list containing the cumulative seconds associated
#' with a variety of tasks as determend by calls to profilePoint().
#' @return List of profiling stats.
#' @seealso profilePoint
getProfileSecs <- function() {
  return(TBCellGrowthEnv$ProfileSecs)
}

#' @keywords environment
#' @export
#' @title Start Profiling Timers
#' @description Sets internal *start* and *timepoint* times that 
#' can be used in scripts to calculate elapsed 'clock' time.
#' @return None
#' @seealso profilePoint
#' @seealso profileEnd
profileStart <- function() {
  TBCellGrowthEnv$ProfileStart <- Sys.time()
  TBCellGrowthEnv$ProfileTimepoint <- Sys.time()
}

#' @keywords environment
#' @export
#' @title Print Partial Elapsed Time
#' @param name named counter which will be incremented with secs since last timepoint
#' @param message character message
#' @description Returns the elapsed time since the
#' last time profilePoint() was called or since profileStart().
#' If \code{message} is not \code{NULL}, an message with this information is printed out.
#' @return Elapsed time in seconds (invisbly).
#' @seealso profileStart
#' @seealso profileEnd
profilePoint <- function(name='unknown', message=NULL) {
  now <- Sys.time()
  elapsed <- as.numeric( difftime(now, TBCellGrowthEnv$ProfileTimepoint, units='secs') )
  TBCellGrowthEnv$ProfileTimepoint <- now
  # Increment profile counter
  if (is.null(TBCellGrowthEnv$ProfileSecs[[name]])) {
    TBCellGrowthEnv$ProfileSecs[[name]] <- elapsed
  } else {
    TBCellGrowthEnv$ProfileSecs[[name]] <- TBCellGrowthEnv$ProfileSecs[[name]] + elapsed
  }
  # Print out a message if desired
  if (TBCellGrowthEnv$RunOptions$profile && !is.null(message)) {
    message(paste(round(elapsed,4), message))
  }
  return(invisible(elapsed))
}

#' @keywords environment
#' @export
#' @title Print Total Elapsed Time
#' @param message character message
#' @description Returns the elapsed time since profileStart().
#' If \code{message} is not \code{NULL}, a message with this information is printed out.
#' @return Elapsed time in seconds.
#' @seealso profileStart
#' @seealso profilePoint
profileEnd <- function(message=NULL) {
  now <- Sys.time()
  elapsed <- as.numeric( difftime(now, TBCellGrowthEnv$ProfileStart, units='secs') )
  # Print out a message if desired
  if (TBCellGrowthEnv$RunOptions$profile && !is.null(message)) {
    message(paste(round(elapsed,4), message))
  }
  # Print out all timers if desired
  if (getRunOptions('verbose')) {
    for (name in names(TBCellGrowthEnv$ProfileSecs)) {
      elapsed <- TBCellGrowthEnv$ProfileSecs[[name]]
      message(paste('\t',round(elapsed,4),'seconds on',name,'\n'))
    }
  }
  return(invisible(elapsed))
}
