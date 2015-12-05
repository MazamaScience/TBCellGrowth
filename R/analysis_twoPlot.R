#' @export
#' @title Two Plots of Growth Curves
#' @param timeseries timeseries dataframe
#' @param minExpFitHour hour of first datapoint to include in doubling time exponential fit
#' @param maxExpFitHour hour of last datapoint to include in doubling time exponential fit
#' @param title overall title
#' @param filename path of the file to which the plot is saved (NULL will plot to screen)
#' @param pngSize image width/height in pixels
#' @description Given a "timeseries" dataframe with hours in the
#' first column and colony sizes in all other columns, create growth curve plots for
#' all columns.
#' @return none

analysis_twoPlot <- function(timeseries, minExpFitHour=0, maxExpFitHour=1e9,
                              title='Title Goes Here', filename=NULL, pngSize=800) {
  
  
  # Get the doublingTime
  doublingTime <- analysis_doublingTime(timeseries, minExpFitHour, maxExpFitHour)
  
  # Extract hours vector and colony size matrix from timeseries
  hours <- timeseries[,1]
  m <- as.matrix(timeseries[,-1])
  
  # Create external file
  if (!is.null(filename)) png(filename, width=pngSize, height=pngSize)
  
  # semi-opaque colors
  gnat <- adjustcolor('black', 0.3 + 10/ncol(timeseries))
  gnat70 <- adjustcolor('black',0.7)
  
  layout(matrix(c(1,1,2:3),nrow=2,byrow=TRUE),heights=c(0.15,1))
  
  # ----- Header --------------------------------------------------------------
  
  par(mar=c(0,0,0,0))
  plot(c(0,1), c(0,1), col='transparent',
       axes=FALSE, xlab='', ylab='')
  text(0.5, 0.5, title, cex=2)  
  
  # ----- Growth Plot ---------------------------------------------------------
  
  # limits
  ylim=c(0, max(as.numeric(m), na.rm=TRUE))
  
  # Initial plot
  par(mar=c(5,4,4,6), mgp=c(2.5,1,0))
  plot(m[,1] ~ hours, type='l', ylim=ylim, bty='n', col=gnat,
       ylab='Area (pixels)',
       main='Growth')
  
  # All other colonies
  if (ncol(m) > 1) {
    for ( i in 2:ncol(m) ) {
      lines(m[,i] ~ hours, col=gnat)
    }
  }
  
  # Colony names
  xpos <- max(hours)
  ypos <- m[nrow(m),]
  text(xpos, ypos, colnames(m), pos=4, xpd=NA, col=gnat70)
  
  # Cleanup
  par(mar=c(5,4,4,2)+.1, mgp=c(3,1,0))
  
  
  # ----- Log of Growth -------------------------------------------------------
  
  log2m <- log2(m)
  ylim=c(0.9*min(as.numeric(log2m), na.rm=TRUE), max(as.numeric(log2m), na.rm=TRUE))
  
  # Initial plot
  par(mar=c(5,4,4,6), mgp=c(2.5,1,0))
  plot(log2m[,1] ~ hours, type='l', ylim=ylim, bty='n', col=gnat,
       ylab=expression(paste(log[2],' Area')),
       main="Log of Growth")
  
  # All other colonies
  if (ncol(log2m) > 1) {
    for ( i in 2:ncol(log2m) ) {
      lines(log2m[,i] ~ hours, col=gnat)
    }
  }
  
  # Colony names
  xpos <- max(hours)
  ypos <- log2m[nrow(log2m),]
  text(xpos, ypos, colnames(log2m), pos=4, xpd=NA, col=gnat70)
  
  # Cleanup
  par(mar=c(5,4,4,2)+.1, mgp=c(3,1,0))
  
  
  # Cleanup
  if (!is.null(filename)) dev.off()
  
}