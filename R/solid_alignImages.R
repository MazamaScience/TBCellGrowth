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

solid_alignImages <- function(images, numTargets=12, targetWidth=50, searchSpace=50) {
  
  print("Finding alignment targets...")
  
  # Is the xy pair in the given bounds?
  isInBounds <- function(bounds, xy) {
    if (is.null(xy) | length(xy) < 1) return(FALSE)
    return((xy[1] > 0) & (xy[1] < bounds[[1]]) & (xy[2] > 0) & (xy[2] < bounds[[2]]))
  }
  
  dimx <- dim(images$phase[[1]])[1]
  dimy <- dim(images$phase[[1]])[2]
  
  # First find some aligment targets
  edges <- filter_sobel(images$phase[[1]])
  edges <- edges > 0.5
  edges <- EBImage::dilateGreyScale(edges, EBImage::makeBrush(7, 'disc'))
  edges <- EBImage::fillHull(edges)
  edges <- removeBlobs(edges, 750)
  edges <- EBImage::bwlabel(edges)
  
  # Pick seven random features to track
  alignmentTargets <- sample(1:max(edges), numTargets, replace=TRUE)
  
  # Find centroids of alignment targets
  alignmentTargets <- lapply(alignmentTargets, function(x) data.frame(which(edges==x, arr.ind=TRUE)))
  alignmentTargets <- lapply(alignmentTargets, function(x) round(c(mean(x$row), mean(x$col))))
  
  # Remove targets whose search space falls out of bounds
  alignmentTargets <- 
    alignmentTargets[unlist(lapply(alignmentTargets, function(x) isInBounds(dim(edges), (x-(targetWidth + searchSpace + 100)))))]
  alignmentTargets <- 
    alignmentTargets[unlist(lapply(alignmentTargets, function(x) isInBounds(dim(edges), (x+(targetWidth + searchSpace + 100)))))]
  
  # Sample the background image with alignment targets
  bgSamples <- lapply(alignmentTargets, function(x) images$phase[[1]][(x[1]-targetWidth):(x[1]+targetWidth),
                                                                      (x[2]-targetWidth):(x[2]+targetWidth)])
  
  # Vectors detailing how much to shift images
  offset.x <- numeric(length(images$phase))
  offset.y <- numeric(length(images$phase))  
  
  print("Finding alignment offsets...")
  
  for (i in 2:length(images$phase)) {
    
    cat(".")
    
    image <- images$phase[[i]]
    
    phaseSamples <- lapply(alignmentTargets, function(x) image[(x[[1]]-targetWidth-searchSpace+offset.x[[i-1]]):(x[[1]]+targetWidth+searchSpace+offset.x[[i-1]]),
                                                               (x[[2]]-targetWidth-searchSpace+offset.y[[i-1]]):(x[[2]]+targetWidth+searchSpace+offset.y[[i-1]])])
    
    sampleDiffs <- matrix(NA,nrow=1 + searchSpace*2,ncol=1 + searchSpace*2)
    
    for (ii in seq(1,searchSpace*2,by=2)) {
      for (jj in seq(1,searchSpace*2,by=2)) { 
        
        x1 <- ii
        x2 <- ii + searchSpace*2
        y1 <- jj
        y2 <- jj + searchSpace*2
        phaseSubset <- lapply(phaseSamples, function(x) x[x1:x2,y1:y2])
        
        # Find the total difference between samples
        diffs <- unlist(mapply(function(x,x1) sum(abs(x-x1), na.rm=TRUE), bgSamples, phaseSubset, SIMPLIFY=FALSE))
        
        sampleDiffs[ii,jj] <- median(diffs, na.rm=TRUE)
        
      }
    }
    
    bestFit <- which(sampleDiffs == min(sampleDiffs, na.rm=T),arr.ind=T)
    
    offset.x[[i]] <- bestFit[[1]] - searchSpace - 1
    offset.y[[i]] <- bestFit[[2]] - searchSpace - 1
    
    bgSamples <- lapply(alignmentTargets, function(x) images$phase[[i]][(x[1]-targetWidth+offset.x[[i]]):(x[1]+targetWidth+offset.x[[i]]),
                                                                        (x[2]-targetWidth+offset.y[[i]]):(x[2]+targetWidth+offset.y[[i]])])
    
  }
  
  offset.x <- cumsum(offset.x)
  offset.y <- cumsum(offset.y)
  
  # Crop images based on offset
  cropY <- max(abs(offset.y)) + 1
  cropX <- max(abs(offset.x)) + 1
  
  dimx <- dim(images$phase[[1]])[[1]]
  dimy <- dim(images$phase[[1]])[[2]]
  
  print("Aligning and cropping images...")
  
  for (ii in 1:length(images)) {
    
    for (jj in 1:length(images[[ii]])) {
      cat(".")
      im <- images[[ii]][[jj]]
      images[[ii]][[jj]] <- im[(cropX + offset.x[jj]):(dimx - cropX + offset.x[jj]),
                               (cropY + offset.y[jj]):(dimy - cropY + offset.y[jj])]
    }
    
  }
  
  return(images)
  
}

