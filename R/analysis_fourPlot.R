#' @export
#' @title Four Plots of Growth Curves and Doubling Times
#' @param timeseries timeseries dataframe
#' @param minExpFitHour hour of first datapoint to include in doubling time exponential fit
#' @param maxExpFitHour hour of last datapoint to include in doubling time exponential fit
#' @param title overall title
#' @param filename path of the file to which the plot is saved (NULL will plot to screen)
#' @param pngSize image width/height in pixels
#' @description Given a "timeseries" dataframe with hours in the
#' first column and colony sizes in all other columns, create a
#' plot of all columns.
#' @return Vector of doubling times

analysis_fourPlot <- function(timeseries, minExpFitHour=0, maxExpFitHour=1e9,
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
  
  
  # ----- Boxplot -------------------------------------------------------------
  
  b <- boxplot(doublingTime,
               ylab="Doubling Time (hours)",
               main='Doubling Times')
  
  rug(doublingTime, side=2, ticksize=0.1)
  
  xpos <- 1
  ypos <- b$out
  text(xpos, ypos, names(b$out), pos=4, col=gnat70)
  
  
  # ----- Histogram -----------------------------------------------------------
  
  DT_noOutliers <- doublingTime[!names(doublingTime) %in% names(b$out)]

  hist(DT_noOutliers, n=length(DT_noOutliers),
       col='gray30', border='gray30',
       xlab='hours',
       main="Histogram of Doubling Times")

  # Add a bar for the outliers (rect uses left,bottom,right,top)
  usr <- par('usr')
  outlierCount <- length(b$out)
  if (length(outlierCount) > 0) {
    rect(usr[2],0,usr[1] + (usr[2]-usr[1])*1.01,outlierCount,col='salmon3',border='transparent', xpd=NA)
    xpos <- usr[2]
    ypos <- ifelse(outlierCount > usr[4], usr[4], outlierCount)
    text(usr[2], ypos, paste0(outlierCount,' outliers'), col='salmon4', pos=2, xpd=NA)
  }
  
  
  # Cleanup
  if (!is.null(filename)) dev.off()
  
}