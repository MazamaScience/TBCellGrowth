#' @export
#' @title Four Plots of Growth Curves and Doubling Times
#' @param timeseries timeseries dataframe
#' @param rowStart index of starting hour
#' @param rowStop index of ending hour
#' @param title overall title
#' @param filename path of the file to which the plot is saved (NULL will plot to screen)
#' @description Given a "timeseries" dataframe with hours in the
#' first column and colony sizes in all other columns, create a
#' plot of all columns.
#' @return Vector of doubling times

analysis_fourPlot <- function(timeseries, rowStart=1, rowStop=nrow(timeseries),
                              title='Title Goes Here', filename=NULL) {

  # Get the doublingTime
  doublingTime <- analysis_doublingTime(timeseries, rowStart, rowStop)
  
  # Extract hours vector and colony size matrix from timeseries
  hours <- timeseries[rowStart:rowStop,1]
  m <- as.matrix(timeseries[rowStart:rowStop,-1])
  
  # Create external file
  if (!is.null(filename)) png(filename)
  
  # semi-opaque colors
  gnat <- adjustcolor('black', 5/nrow(timeseries))
  gnat70 <- adjustcolor('black',0.7)
  
  layout(matrix(c(1,1,2:5),nrow=3,byrow=TRUE),heights=c(0.3,1,1))
  
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
  for ( i in 2:ncol(m) ) {
    lines(m[,i] ~ hours, col=gnat)
  }
  
  # Colony names
  xpos <- max(hours)
  ypos <- m[nrow(m),]
  text(xpos, ypos, colnames(m), pos=4, xpd=NA, col=gnat70)
  
  # Cleanup
  par(mar=c(5,4,4,2)+.1, mgp=c(3,1,0))
  
  
  # ----- Log of Growth -------------------------------------------------------
  
  logm <- log(m)
  ylim=c(0, max(as.numeric(logm), na.rm=TRUE))
  
  # Initial plot
  par(mar=c(5,4,4,6), mgp=c(2.5,1,0))
  plot(logm[,1] ~ hours, type='l', ylim=ylim, bty='n', col=gnat,
       ylab=expression(paste(log[10],' Area')),
       main="Log of Growth")
  
  # All other colonies
  for ( i in 2:ncol(logm) ) {
    lines(logm[,i] ~ hours, col=gnat)
  }
  
  # Colony names
  xpos <- max(hours)
  ypos <- logm[nrow(logm),]
  text(xpos, ypos, colnames(logm), pos=4, xpd=NA, col=gnat70)
  
  # Cleanup
  par(mar=c(5,4,4,2)+.1, mgp=c(3,1,0))
  
  
  # ----- Boxplot -------------------------------------------------------------
  
  b <- boxplot(doublingTime,
               ylab="Doubling Time (hours)",
               main='Doubling Time Outliers')
  
  xpos <- 1
  ypos <- b$out
  text(xpos, ypos, names(b$out), pos=4, col=gnat70)
  
  
  # ----- Histogram -----------------------------------------------------------
  
  DT_noOutliers <- doublingTime[!names(doublingTime) %in% names(b$out)]
  b <- boxplot(DT_noOutliers, plot=FALSE)
  
  hist(DT_noOutliers, n=length(DT_noOutliers),
       col='gray30', border='gray30',
       xlab='hours',
       main="Histogram of Doubling Times (outliers removed)")
#   xpos <- b$out
#   ypos <- 0.2 * par('usr')[4]
#   text(xpos, ypos, names(b$out), pos=3, srt=90, col=gnat70)
  
  
  
  # Cleanup
  if (!is.null(filename)) dev.off()

}