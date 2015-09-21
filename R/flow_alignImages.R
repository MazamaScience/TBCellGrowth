#' @export
#' @title Align A Series of Flow Images
#' @param images a list of lists of images with channel names as keys.
#' @param numTargets how many target features to compare
#' @param targetWidth the radius of alignment targets
#' @param searchSpace how far to search for best alignment. A smaller
#' value here yeilds faster searches.
#' @description Given a list of phase images and a list of zero or
#' more lists of dye images, aligns these images to the first image
#' of phase (assumed to be a background). Will also crop and rotate
#' the images if those are specified.
#' @return a \code{list} of two lists, \code{phase} and \code{dyes},
#' which will be the same lengths as the input.


flow_alignImages <- function(images, numTargets=12, targetWidth=30, searchSpace=30) {
  
  # TODO:  Should channelName be passed in as an argument?
  channelName <- 'phase'
  
  ptm <- proc.time()
  cat("\nFinding alignment targets...")
  
  # Is the xy pair in the given bounds?
  isInBounds <- function(bounds, xy) {
    if (is.null(xy) | length(xy) < 1) return(FALSE)
    return((xy[1] > 0) & (xy[1] < bounds[[1]]) & (xy[2] > 0) & (xy[2] < bounds[[2]]))
  }
  
  # First find some aligment targets in the background image
  edges <- filter_sobel(images[[channelName]][[1]])
  edges <- edges > 0.5
  edges <- EBImage::dilateGreyScale(edges, EBImage::makeBrush(7, 'disc'))
  edges <- EBImage::fillHull(edges)
  edges <- removeBlobs(edges, 500, 1000)
  edges <- EBImage::bwlabel(edges)
  
  # NOTE:  At this point, the 'edges' matrix consists of little blobs of integers swimming
  # NOTE:  in a sea of zeros. Each blob of connected pixels will have a unique integer.

  # Randomly pick features to track
  targetIds <- sample(1:max(edges), numTargets, replace=FALSE)

  # Create an empty list for the background samples
  bgSamples <- list('list',length(targetIds))

  # Define buffer around target centroid
  buffer <- 50+targetWidth+searchSpace
  
  bgIndex <- 1
  for ( targetId in targetIds ) {
    # Define a box centered on each target
    targetCells <- data.frame(which(edges==targetId, arr.ind=TRUE))
    targetX <- mean(targetCells$row)
    targetY <- mean(targetCells$col)
    left <- targetX - buffer
    right <- targetX + buffer
    top <- targetY + buffer
    bottom <- targetY - buffer
    # Skip targets whose search space falls out of bounds
    if ( dim(edges)[1] < left || dim(edges)[1] > right || dim(edges)[2] < bottom || dim(edges[2]) > top) {
      next
    } else {
      # Sample the first (background) image based on the alignment target
      bgSamples[[bgIndex]] <- images[[channelName]][[1]][(targetX-targetWidth):(targetX+targetWidth),
                                                         (targetY-targetWidth):(targetY+targetWidth)]
      bgIndex <- bgIndex + 1
    }
  }
  
  if (length(bgIndex) < 2) {
    stop(paste0('Not enough targets for alignment: ',length(bgIndex)))
  }
  
  # Vectors detailing how much to shift images
  offset.x <- numeric(length(images[[channelName]]))
  offset.y <- numeric(length(images[[channelName]]))  
  
  cat("\nFinding alignment offsets")
  
  for (i in 2:length(images[[channelName]])) {
    
    cat(".")
    
    image <- images[[channelName]][[i]]
    
    phaseSamples <- lapply(alignmentTargets, function(x) image[(x[[1]]-targetWidth-searchSpace):(x[[1]]+targetWidth+searchSpace),
                                                               (x[[2]]-targetWidth-searchSpace):(x[[2]]+targetWidth+searchSpace)])
    
    sampleDiffs <- matrix(NA,nrow=1 + searchSpace*2,ncol=1 + searchSpace*2)
    
    for (ii in seq(1,searchSpace*2, by=3)) {
      cat(".")
      for (jj in seq(1,searchSpace*2, by=3)) { 
        
        x1 <- ii
        x2 <- ii + targetWidth*2
        y1 <- jj
        y2 <- jj + targetWidth*2
        phaseSubset <- lapply(phaseSamples, function(x) x[x1:x2,y1:y2])
        
        # Find the total difference between samples
        diffs <- unlist(mapply(function(x,x1) sum(abs(x-x1)), bgSamples, phaseSubset, SIMPLIFY=FALSE))
        
        sampleDiffs[ii,jj] <- median(diffs)
        
      }
    }
    
    bestFit <- which(sampleDiffs == min(sampleDiffs, na.rm=T),arr.ind=T)
    
    offset.x[[i]] <- bestFit[[1]] - searchSpace - 1
    offset.y[[i]] <- bestFit[[2]] - searchSpace - 1
    
  }
  
  # Crop images based on offset
  cropY <- max(abs(offset.y)) + 1
  cropX <- max(abs(offset.x)) + 1
  
  dimx <- dim(images[[channelName]][[1]])[[1]]
  dimy <- dim(images[[channelName]][[1]])[[2]]
  
  cat("\nAligning images")
  
  for (ii in 1:length(images)) {
    
    for (jj in 1:length(images[[ii]])) {
      cat(".")
      im <- images[[ii]][[jj]]
      images[[ii]][[jj]] <- im[(cropX + offset.x[jj]):(dimx - cropX + offset.x[jj]),
                               (cropY + offset.y[jj]):(dimy - cropY + offset.y[jj])]
    }
    
  }
  
  cat(paste0("\nImages aligned in ", formatTime(ptm)))
  
  return(images)
  
}
