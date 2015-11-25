#' @export
#' @title Calculate Doubling Times
#' @param timeseries timeseries dataframe
#' @param rowStart index of starting hour
#' @param rowStop index of ending hour
#' @description Given a "timeseries" dataframe with hours in the
#' first column and colony sizes in all other columns, return a 
#' vector of doubling times in hours.
#' @return Vector of doubling times

analysis_doublingTime <- function(timeseries, rowStart=1, rowStop=nrow(timeseries)) {
  
  # Extract hours vector and colony size matrix from timeseries
  hours <- timeseries[rowStart:rowStop,1]
  m <- as.matrix(timeseries[rowStart:rowStop,-1])
  
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