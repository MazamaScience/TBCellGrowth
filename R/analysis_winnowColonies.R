#' @export
#' @title Remove Likely Artifacts
#' @param timeseries timeseries dataframe
#' @param maxDoublingTime colonies with doubling times larger than this are removed
#' @param removeOutliers flag indicating whether to remove colonies with doubling time outliers
#' @param minStartHour colonies not identified by this hour are removed
#' @description After image analysis, the TBCellGrowth package produces.csv files
#' which can be read in as a "timeseries" dataframe
#' with hours in the first column and colony sizes in all other columns.
#' Depending on run configuration, some portion of the colonies will be artifacts
#' -- bubbles, blotches, specks, etc.
#' 
#' This function helps to winnow down liktely artifacts and returns a dataframe
#' that likely contains a higher percentage of actual colonies.
#' 
#' The main way this function removes artifacts is by removing "colonies" that
#' do not grow. Doubling times for rapidly growing colonies will be low numbers
#' while doubling times for no or little growht will be very large numbers. 
#' Negative doubling times are found where identified "colonies" shrink over time.
#' 
#' This function identifies any colony with a negative doubling time as an artifact.
#' After removal of negative doubling times, the boxlplot() fucntion is used to 
#' identifiy outliers. These are subsequently removed.
#' 
#' An additional option is to remove all "colonies" that are not identified until
#' after a particular hour. (Sometimes, changes in the image quality cause new
#' "colonies" to be identified after a particular timestep.)
#' 
#' This funciton returns a list with two dataframes, one containing the retained
#' colonies and one containing the removed colonies.
#' 
#' @return List of two timeseries dataframes: "retained" and "removed".

analysis_winnowColonies <- function(timeseries, maxDoublingTime=NULL,
                                    removeOutliers=TRUE, minStartHour=NULL) {
  
  # Make sure the first column is numeric rather than "003", etc.
  timeseries[,1] <- as.numeric(timeseries[,1])
  
  # Get the doublingTime
  doublingTime <- analysis_doublingTime(timeseries)

  # Remove any NA doubling times
  missingDTMask <- is.na(doublingTime)
  
  # Alwaus remove negative doubling times
  negativeDTMask <- (doublingTime < 0)

  # Find colonies with doubling times above a threshold
  overMaxDTMask <- rep(FALSE,ncol(timeseries))[-1] # remove timestep column to match other masks
  if (!is.null(maxDoublingTime)) {
    overMaxDTMask <- (doublingTime > maxDoublingTime)
  }
  
  # Combine the above to create a "noGrowth" mask
  noGrowthMask <- negativeDTMask | overMaxDTMask
  
  # Find outliers in the non-negative doubling times
  outlierDTMask <- rep(TRUE,ncol(timeseries))[-1] # remove timestep column to match other masks
  if (removeOutliers) {
    b <- boxplot(doublingTime[!noGrowthMask], plot=FALSE)
    outlierDTMask <- (names(doublingTime) %in% names(b$out))
  }
  
  # Find colonies that existed before minStartHour
  earlyMask <- rep(TRUE,ncol(timeseries))[-1] # remove timestep column to match other masks
  if (!is.null(minStartHour)) {
    timeMask <- timeseries[,1] <= minStartHour
    earlyTimeseries <- timeseries[timeMask,]
    anyMask <- apply(earlyTimeseries, 2, function(x) {any(!is.na(x))})
    earlyNames <- names(timeseries[anyMask])
    earlyMask <- (names(timeseries) %in% earlyNames)[-1] # remove timestep column to match other masks
  }
  
  # Create the retained and removed dataframes (adding an initial TRUE to keep the timestep column)
  removedMask <- (missingDTMask | noGrowthMask | outlierDTMask | !earlyMask)
  retained <- timeseries[,c(TRUE,!removedMask)]
  removed <- timeseries[,c(TRUE,removedMask)]
  
  return(list(retained=retained,removed=removed))
  
}