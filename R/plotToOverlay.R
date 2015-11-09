#' @export
#' @title Convert Plot Function to Overlay Mask
#' @param plotf a function which plots lines, points, or text over the blank plot
#' @param dimx the image's x dimension
#' @param dimy the image's y dimension
#' @description This function runs plotting commands in \code{plotf} to add to an
#' otherwise blank plot of dimensions \code{dimx} X \code{dimy}. This image is 
#' written to disk and then read in with \code{EBImage::readImage} to create a 
#' a 2D mask that is 1 for each pixel that is part of the lines, points or text
# produced by \code{plotf}.
#' @return a \code{matrix} image.

plotToOverlay = function(plotf, dimx, dimy) {

  # Initialize temporary png
  png("temp345s45grf.png",width=dimx,height=dimy)
    
  # Save par
  oldPar <- par()
  
  # Set 0 margin parameters
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  
  # Blank plot with proper parameters
  plot(0,0, ylim=c(dimy,1), xlim=c(1,dimx), type="n", axes=T, xaxt="n", yaxt='n', xaxs='i', yaxs='i')
  
  # Execute plotting function
  plotf()
  dev.off()
  
  # Reset parameters
  try(par(oldPar), silent=TRUE)
  
  # Now load the png as an "image" type
  labels <- 1 - EBImage::readImage("temp345s45grf.png")[,,1]
  
  # Delete the file
  file.remove("temp345s45grf.png")

  # Return the mask
  return(labels)
  
}
