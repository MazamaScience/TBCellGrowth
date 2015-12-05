#' @export
#' @title Calculate Doubling Times
#' @param timeseries timeseries dataframe
#' @param minExpFitHour hour of first datapoint to include in doubling time exponential fit
#' @param maxExpFitHour hour of last datapoint to include in doubling time exponential fit
#' @description Given a "timeseries" dataframe with hours in the
#' first column and colony sizes in all other columns, return a 
#' vector of doubling times in hours.
#' @return Vector of doubling times

analysis_doublingTime <- function(timeseries, minExpFitHour=0, maxExpFitHour=1e9) {
  
  # Guarantee that additional arguments are numeric. (Defaults to alphabetic comparison and much confusion otherwise.)
  minExpFitHour <- as.numeric(minExpFitHour)
  maxExpFitHour <- as.numeric(maxExpFitHour)
  
  # Extract hours vector (converting from "003", etc.)
  hours <- as.numeric(timeseries[,1])
  hourMask <- (hours >= minExpFitHour) & (hours <= maxExpFitHour)
  
  # Sanity check
  if ( sum(hourMask) < 2 ) stop(paste0('Fewer than 2 timesteps between hour=',minExpFitHour,' and hour=',maxExpFitHour,'.'))
  
  # Reduced hours vector and timeseries matrix that also omits the 'hours' column
  hours <- hours[hourMask]
  m <- as.matrix(timeseries[hourMask,-1])
  
  # Calculate the doubling time for each colony
  # NOTE:  Based on http://www.pangloss.com/wiki/R_GrowthCurve
  # NOTE:  Do a linear fit on each column and pull out the slope 
  doublingTime <- vector('numeric',ncol(m))
  for ( j in 1:ncol(m) ) {
    result <- try( {
      doublingTime[j] <- log(2) / ( lm(log(m[,j]) ~ hours)$coef[2] )
    }, silent=TRUE)
    
    if ( class(result)[1] == "try-error" ) doublingTime[j] <- NA
  }
  
  # Add colony names from timeseries, omitting the first column which has hours
  names(doublingTime) <- names(timeseries)[-1]
  
  return(doublingTime)
  
}