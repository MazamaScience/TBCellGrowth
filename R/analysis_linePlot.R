#' @export
#' @title Plot all Growth Curves
#' @param timeseries timeseries dataframe
#' @param rowStart index of starting hour
#' @param rowStop index of ending hour
#' @param logScale logical indicating whether to use a logarhithmic scale for Y.
#' @param filename path of the file to which the plot is saved (NULL will plot to screen)
#' @description Given a "timeseries" dataframe with hours in the
#' first column and colony sizes in all other columns, create a
#' plot of all columns.
#' @return Vector of doubling times

analysis_linePlot <- function(timeseries, rowStart=1, rowStop=nrow(timeseries),
                              logScale=TRUE, filename=NULL) {

  # Extract hours vector and colony size matrix from timeseries
  hours <- timeseries[rowStart:rowStop,1]
  m <- as.matrix(timeseries[rowStart:rowStop,-1])
  
  if (!is.null(filename)) png(filename)
  
  # semi-opaque colors
  gnat <- adjustcolor('black', 10/nrow(m))
  
  # limits
  ylim=c(0, max(as.numeric(m), na.rm=TRUE))
  if (logScale) ylim <- c(0,log(ylim[2]))
  
  plot(log(m[,1]) ~ hours, type='l', ylim=ylim,
       col=gnat,
       ylab='')
  
  for ( i in 2:ncol(m) ) {
    lines(log(m[,i]) ~ hours, col=gnat)
  }
  
  if (!is.null(filename)) dev.off()
  
}