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
  
  # NOTE:  Put the work in a try block so we never skip removing the tmpfile
  
  result <- try({
    
    # Initialize temporary png
    filename <- tempfile(pattern='tmp_',tmpdir=getwd(),fileext=".png")
    png(filename,width=dimx,height=dimy)
    
    # Set 0 margin parameters
    par(mar=c(0,0,0,0), oma=c(0,0,0,0))
    
    # NOTE:  All plotting is in try blocks so that we will always dev.off()
    # Blank plot with proper parameters
    try( plot(0,0, ylim=c(dimy,1), xlim=c(1,dimx), type="n", axes=T, xaxt="n", yaxt='n', xaxs='i', yaxs='i') )
    
    # Execute plotting function
    try( plotf() )
    
    dev.off()
    
    # Reset parameters
    par(mar=c(5,4,4,2) + 0.1,
        oma=c(0,0,0,0))
    
    # Now load the png as an "image" type
    labelsMask <- 1 - EBImage::readImage(filename)[,,1]
    
  })
  
  if ( class(result)[1] == "try-error" ) {
    labelsMask <- matrix(data=0, nrow=dimx, ncol=dimy)
  }
  
  # Delete the file
  if (file.exists(filename)) file.remove(filename)
  
  # Return the mask
  return(labelsMask)
  
}
