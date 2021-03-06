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
  
  ptm <- proc.time()
  cat("\nFinding alignment targets")
  
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
  # NOTE:  Use set.seed() to guarantee reproducibility while doing "random" sampling
  set.seed(123456)
  alignmentTargets <- sample(1:max(edges), numTargets, replace=TRUE)
  
  # Find centroids of alignment targets
  alignmentTargets <- lapply(alignmentTargets, function(x) data.frame(which(edges==x, arr.ind=TRUE),stringsAsFactors=FALSE))
  alignmentTargets <- lapply(alignmentTargets, function(x) round(c(mean(x$row), mean(x$col))))
  
  # Remove targets whose search space falls out of bounds
  alignmentTargets <- 
    alignmentTargets[unlist(lapply(alignmentTargets, function(x) isInBounds(dim(edges), (x-(targetWidth + searchSpace + 50)))))]
  alignmentTargets <- 
    alignmentTargets[unlist(lapply(alignmentTargets, function(x) isInBounds(dim(edges), (x+(targetWidth + searchSpace + 50)))))]
  
  # Vectors detailing how much to shift images
  offset.x <- numeric(length(images$phase))
  offset.y <- numeric(length(images$phase))
  
  if (length(unique(alignmentTargets)) < 4) {
    cat("\nNot enough alignment targets found, aborting alignment")
    return(images)
  }
  
  cat("\nFinding alignment offsets")
  
  for (i in 2:length(images$phase)) {
    
    cat(".")
    
    image <- images$phase[[i]]
    
    phaseSamples <- lapply(alignmentTargets, function(x) {
      x1 <- (x[[1]]-targetWidth-searchSpace+offset.x[[i-1]])
      x2 <- (x[[1]]+targetWidth+searchSpace+offset.x[[i-1]])
      y1 <- (x[[2]]-targetWidth-searchSpace+offset.y[[i-1]])
      y2 <- (x[[2]]+targetWidth+searchSpace+offset.y[[i-1]])
      if (sum(c(x1,x2,y1,y2) < 0) > 0) return(NULL)
      return(images$phase[[i]][x1:x2,y1:y2])
#       result <- tryCatch({
#         sample <- images$phase[[i]][x1:x2,y1:y2]
#       }, error = function(e) {
#         sample <- NULL
#       })
#       return(sample)
    })
    
    bgSamples <- lapply(alignmentTargets, function(x) {
      x1 <- (x[[1]]-targetWidth+offset.x[[i-1]])
      x2 <- (x[[1]]+targetWidth+offset.x[[i-1]])
      y1 <- (x[[2]]-targetWidth+offset.y[[i-1]])
      y2 <- (x[[2]]+targetWidth+offset.y[[i-1]])
      if (sum(c(x1,x2,y1,y2) < 0) > 0) return(NULL)
      return(images$phase[[i-1]][x1:x2,y1:y2])
#       result <- tryCatch({
#         sample <- images$phase[[i-1]][x1:x2,y1:y2]
#       }, error = function(e) {
#         sample <- NULL
#       })
#       return(sample)
    })
    
    nullMask <- unlist(lapply(phaseSamples, is.null)) | unlist(lapply(bgSamples, is.null))
    
    phaseSamples[nullMask] <- NULL
    bgSamples[nullMask] <- NULL
    
    bgSamples <- lapply(bgSamples, function(x) filter_blur(x)^2 > 0.3)
    phaseSamples <- lapply(phaseSamples, function(x) filter_blur(x)^2 > 0.3)
    
    sampleDiffs <- matrix(NA,nrow=1 + searchSpace*2,ncol=1 + searchSpace*2)
    
    for (ii in seq(1,searchSpace*2,by=2)) {
      for (jj in seq(1,searchSpace*2,by=2)) { 
        
        x1 <- ii
        x2 <- ii + targetWidth*2
        y1 <- jj
        y2 <- jj + targetWidth*2
        phaseSubset <- lapply(phaseSamples, function(x) x[x1:x2,y1:y2])
        
        # Find the total difference between samples
        diffs <- unlist(mapply(function(x,x1) sum(abs(x-x1), na.rm=TRUE), bgSamples, phaseSubset, SIMPLIFY=FALSE))
        
        sampleDiffs[ii,jj] <- mean(diffs, na.rm=TRUE)
        
      }
    }
    
    bestFit <- which(sampleDiffs == min(sampleDiffs, na.rm=T),arr.ind=T)
    
    offset.x[[i]] <- offset.x[[i-1]] + bestFit[[1]] - searchSpace - 1
    offset.y[[i]] <- offset.y[[i-1]] + bestFit[[2]] - searchSpace - 1
    
  }
  
  
  # Crop images based on offset
  cropT <- abs(max(offset.y)) + 1
  cropB <- abs(min(offset.y)) + 1
  cropR <- abs(min(offset.x)) + 1
  cropL <- abs(max(offset.x)) + 1
  
  dimx <- dim(images$phase[[1]])[[1]]
  dimy <- dim(images$phase[[1]])[[2]]
  
  cat("\nAligning images")
  
  for (ii in 1:length(images)) {
    
    for (jj in 1:length(images[[ii]])) {
      cat(".")
      im <- images[[ii]][[jj]]
      padT <- cropT - offset.y[jj]
      padB <- cropB + offset.y[jj]
      padL <- cropL - offset.x[jj]
      padR <- cropR + offset.x[jj]
      
      im <- cbind(matrix(NA, nrow=dim(im)[[1]], ncol=padT), im)
      im <- cbind(im, matrix(NA, nrow=dim(im)[[1]], ncol=padB))
      im <- rbind(matrix(NA, nrow=padL, ncol=dim(im)[[2]]), im)
      im <- rbind(im, matrix(NA, nrow=padR, ncol=dim(im)[[2]]))
      
      images[[ii]][[jj]] <- im

    }
    
  }
  
  cat(paste0("\nImages aligned in ", formatTime(ptm)))
  
  return(images)
  
}


