#' @export
#' @title Remove Likely Artifacts
#' @param timeseries timeseries dataframe
#' @param minExpFitHour hour of first datapoint to include in doubling time exponential fit
#' @param maxExpFitHour hour of last datapoint to include in doubling time exponential fit
#' @param minDoublingTime colonies with doubling times smaller than this many hours are removed
#' @param maxDoublingTime colonies with doubling times larger than this many hours are removed
#' @param removeOutliers flag indicating whether to remove colonies with doubling time outliers
#' @param maxStartHour colonies not identified by this hour are removed
#' @description After image analysis, the TBCellGrowth package produces.csv files
#' which can be read in as a "timeseries" dataframe
#' with hours in the first column and colony sizes in all other columns.
#' Depending on run configuration, some portion of the colonies will be artifacts
#' -- bubbles, blotches, specks, etc.
#' 
#' This function helps to winnow down likely artifacts and returns a dataframe
#' that likely contains a higher percentage of actual colonies.
#' 
#' The main way this function removes artifacts is by removing "colonies" that
#' do not grow. Doubling times for rapidly growing colonies will be low numbers
#' while doubling times for no or little growth will be very large numbers. 
#' Negative doubling times are found where identified "colonies" shrink over time.
#' 
#' This function identifies any colony with a negative doubling time as an artifact.
#' After removal of negative doubling times, the boxlplot() function is used to 
#' identifiy outliers. These are subsequently removed.
#' 
#' An additional option is to remove all "colonies" that are not identified until
#' after a particular hour. (Sometimes, changes in the image quality cause new
#' "colonies" to be identified after a particular timestep.)
#' 
#' This funciton returns a list with two dataframes, one containing the retained
#' colonies and one containing the removed colonies.
#' @details The R boxplot() function is used to define outliers and has the following documentation
#' where IQR stands for Inter-Quartile Range:
#' 
#' The two 'hinges' are versions of the first and third quartile, i.e., close to 
#' quantile(x, c(1,3)/4). The hinges equal the quartiles for odd n (where n <- length(x)) 
#' and differ for even n. Whereas the quartiles only equal observations for n %% 4 == 1 (n = 1 mod 4), 
#' the hinges do so additionally for n %% 4 == 2 (n = 2 mod 4), and are in the middle 
#' of two observations otherwise.
#'
#' The notches (if requested) extend to +/-1.58 IQR/sqrt(n). This seems to be based on 
#' the same calculations as the formula with 1.57 in Chambers et al (1983, p. 62), given 
#' in McGill et al (1978, p. 16). They are based on asymptotic normality of the median 
#' and roughly equal sample sizes for the two medians being compared, and are said to 
#' be rather insensitive to the underlying distributions of the samples. The idea appears 
#' to be to give roughly a 95% confidence interval for the difference in two medians.
#' @seealso \link{analysis_doublingTime}
#' @return List of two timeseries dataframes: "retained" and "removed".

analysis_winnowColonies <- function(timeseries, minExpFitHour=0, maxExpFitHour=1e9,
                                    minDoublingTime=NULL, maxDoublingTime=NULL,
                                    removeOutliers=TRUE, maxStartHour=NULL) {
  
  # Make sure the first column is numeric rather than "003", etc.
  timeseries[,1] <- as.numeric(timeseries[,1])
  
  # Get the doublingTime
  doublingTime <- analysis_doublingTime(timeseries, minExpFitHour, maxExpFitHour)

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
  
  # Find colonies that existed before maxStartHour
  earlyMask <- rep(TRUE,ncol(timeseries))[-1] # remove timestep column to match other masks
  if ( !is.null(maxStartHour) ) {
    timeMask <- timeseries[,1] <= maxStartHour
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
