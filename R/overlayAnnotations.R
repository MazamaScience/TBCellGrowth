#' @export
#' @title Add Annotations to an Image
#' @param image an image matrix
#' @param labeledImage a matrix of blob labels
#' @param id the blob id the image is focused on
#' @param infoList list of information to be used for annotations
#' @description Adds a bar to the bottom right of an image.
#' The bar is labeled according to the scale that is passed in.
#' @return a \code{matrix} image.

overlayAnnotations <- function(image, labeledImage, id, infoList) {
  
  # Pull variables out of annotations list
  distanceScale <- infoList$distanceScale
  distanceThresholds <- infoList$distanceThresholds
  size <- infoList$size
  time <- infoList$time
  centroids <- infoList$centroids
  
  # Get dimensions
  dimx <- dim(image)[1]
  dimy <- dim(image)[2]
  
  # Scale Bar ---------------------------------------------
  
  dimxum <- distanceScale * dimx
  distance <- distanceThresholds[dimxum > distanceThresholds + 10]
  distance <- distance[length(distance)]
  if (length(distance) < 1) distance <- 5
  barLength <- distance / distanceScale
  
  # Vital stats -------------------------------------------
  
  convertedSize <- round((distanceScale * sqrt(size))^2, 1)
  
  # Neighbor IDs ------------------------------------------
  
  # Which ids are present in this image
  # Subtract the first index, it's always 0
  indices <- na.exclude(unique(as.numeric(labeledImage)))[-1]
  
  # Subset centroids dataframe to those just found in the subset
  centroids <- centroids[centroids$index %in% indices,]
  
  otherColonies <- (nrow(centroids) > 1)
  
  # Extra work to label other colonies in this image
  if ( otherColonies ) {
    
    # Remove main id from the centroids dataframe
    centroids <- centroids[centroids$id != id,]
    rownames(centroids) <- centroids$id
    
    # Find new centroids for just this image
    for (tempId in centroids$id) {
      # Return the labeledImage x and y coordinates where this colonyIndex is found
      index <- centroids[tempId,'index']
      colonyLocation <- which(labeledImage == index, arr.ind=TRUE)
      # Assign new location information
      centroids[tempId,'x'] <- round(mean(colonyLocation[,1], na.rm=TRUE))
      centroids[tempId,'y'] <- round(mean(colonyLocation[,2], na.rm=TRUE))
      centroids[tempId,'xmin'] <- min(colonyLocation[,1], na.rm=TRUE)
      centroids[tempId,'xmax'] <- max(colonyLocation[,1], na.rm=TRUE)
      centroids[tempId,'ymin'] <- min(colonyLocation[,2], na.rm=TRUE)
      centroids[tempId,'ymax'] <- max(colonyLocation[,2], na.rm=TRUE)
    }
    
    # Only label things outside the annotations area
    ymask <- centroids$y > 40
    xmask <- centroids$x > 60
    centroids <- centroids[(xmask | ymask),]
    
  }
  
  # Check again to see if there are still colonies to be labeled
  otherColonies <- (nrow(centroids) > 1)
  
  result <- try({
    
    # Initialize temporary png
    filename <- tempfile(pattern='tmp_',tmpdir=getwd(),fileext=".png")
    png(filename,width=dimx,height=dimy)
    
    # Set 0 margin parameters
    par(mar=c(0,0,0,0), oma=c(0,0,0,0))
    
    # Blank plot with proper parameters
    plot(0,0, ylim=c(dimy,1), xlim=c(1,dimx), type="n", axes=T, xaxt="n", yaxt='n', xaxs='i', yaxs='i')
    
    # Scale Bar
    text(dimx-(barLength/2)-10, dimy-20, paste(distance,"\U00B5m"), cex=barLength/100) # \U00B5 = µ
    lines(c(dimx-barLength-10,dimx-10), rep(dimy-10,2), lwd=3)
    
    # Vital stats
    text(5,8,paste0(time,"hr"),cex=1,adj=c(0,NA))
    text(5,20,paste0(id),cex=1,adj=c(0,NA))
    text(5,32,paste0(convertedSize,"\U00B5m^2"),cex=1,adj=c(0,NA)) # \U00B5 = µ
    
    # Neighbor IDs (if there are any)
    if ( otherColonies ) {
      for ( i in 1:nrow(centroids) ) {
        cen <- centroids[i,]
        if (cen$x < dimx/2) {
          adj <- c(0,0.5)
        } else {
          adj <- c(1,0.5) 
        }
        x <- ifelse(cen$x < dimx/2, cen$xmax, cen$xmin)
        text(x=x, y=cen$y, labels=cen$id, adj=adj, cex=0.8)
      }
    }
    
    dev.off()
    
    # Reset parameters
    par(mar=c(5,4,4,2) + 0.1,
        oma=c(0,0,0,0))
    
    # Now load the png as an "image" type
    labeledImageMask <- 1 - EBImage::readImage(filename)[,,1]
    
  })
  
  if ( class(result)[1] == "try-error" ) {
    err_msg <- geterrmessage()
    cat(paste0('ERROR:  Cannot create overlay\n',err_msg))
    labeledImageMask <- matrix(data=0, nrow=dimx, ncol=dimy)
  }
  
  # Delete the file
  if (file.exists(filename)) file.remove(filename)
  
  # Sanity check -- mask has same dimensions as image
  if ( nrow(labeledImageMask) != dimx || ncol(labeledImageMask) != dimy ) {
    cat(paste0('ERROR:  image dimensions = (',dimx,',',dimy,'), mask = (',nrow(labeledImageMask),',',ncol(labeledImageMask),')\n'))
  }
  
  
  # Create a mask from the image where where foreground=1, background=0
  image[labeledImageMask > 0.4] <- 1
  
  return(image)
  
  
}