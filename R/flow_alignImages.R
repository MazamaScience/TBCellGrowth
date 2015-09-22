#' @export
#' @title Align A Series of Flow Images
#' @param imageList list of images orgainzed as [[channel]][[timestep]]
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


flow_alignImages <- function(imageList, numTargets=12, targetWidth=30, searchSpace=30) {
  
  # TODO:  Should channel be passed in as an argument?
  channel <- 'phase'
  
  # Is the xy pair in the given bounds?
  isInBounds <- function(bounds, xy) {
    if (is.null(xy) | length(xy) < 1) return(FALSE)
    return((xy[1] > 0) & (xy[1] < bounds[[1]]) & (xy[2] > 0) & (xy[2] < bounds[[2]]))
  }
  
  # ----- Create background blobs -------------------------------------------
  
  if (getRunOptions('verbose')) cat('\tCreating background blobs ...\n')

  # Fubd edges in the background image
  edges <- filter_sobel(imageList[[channel]][[1]])
  edges <- edges > 0.5
  edges <- EBImage::dilateGreyScale(edges, EBImage::makeBrush(7, 'disc'))
  edges <- EBImage::fillHull(edges)
  edges <- removeBlobs(edges, 500, 1000)
  edges <- EBImage::bwlabel(edges)
  
  profilePoint('edges','seconds to create background blobs')
  
  if (getRunOptions('debug_image')) {
    chamber <- getRunOptions('chambers')[1] # TODO:  Is getRunOption('chambers') always singular?
    file <- paste0(getRunOptions('outputDir'),'/',chamber,'_',channel,'_000_alignEdges.jpg')
    EBImage::writeImage(edges, file)
    profilePoint('saveImages','seconds to save images')
  }
  
  # NOTE:  At this point, the 'edges' matrix consists of little blobs of integers swimming
  # NOTE:  in a sea of zeros. Each blob of connected pixels will have a unique integer.
  
  # ----- Picking alignment targets -------------------------------------------
  
  # Randomly pick features to track
  targetIds <- sample(1:max(edges), numTargets, replace=FALSE)
  
  # Create an empty list for the background samples
  targetCentroids <- list()
  bgSamples <- list()
  
  # Define buffer around target centroid
  buffer <- 50+targetWidth+searchSpace
  
  # Full image width and height
  imageWidth <- dim(edges)[1]
  imageHeight <- dim(edges)[2]
  
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
    if ( (left < 0) || (right > imageWidth) || (bottom < 0) || (top > imageHeight) ) {
      if (getRunOptions('verbose')) {
        boundaries <- paste(round(c(left,right,top,bottom),0),collapse=',')
        warning(paste0('Skipping targetId=',targetId,' as it is out of bounds, boundaries =',boundaries))
      }
      next
    } else {
      # Sample the first (background) image based on the alignment target
      bgSamples[[bgIndex]] <- imageList[[channel]][[1]][(targetX-targetWidth):(targetX+targetWidth),
                                                        (targetY-targetWidth):(targetY+targetWidth)]
      targetCentroids[[bgIndex]] <- c(targetX,targetY)
      bgIndex <- bgIndex + 1
    }
  }
  
  if (length(bgSamples) < 2) {
    stop(paste0('Not enough targets for alignment: ',length(bgIndex)))
  }

  profilePoint('alignment','seconds to pick alignment targets')
  
  
  # ----- Find alignment offsets ----------------------------------------------
  
  # Vectors detailing how much to shift images
  offset.x <- numeric(length(imageList[[channel]]))
  offset.y <- numeric(length(imageList[[channel]]))  
  
  if (getRunOptions('verbose')) cat('\tfinding aligment offsets ...\n')
  
  for (i in 2:length(imageList[[channel]])) {
    
    image <- imageList[[channel]][[i]]
    
    phaseSamples <- lapply(targetCentroids, function(x) image[(x[[1]]-targetWidth-searchSpace):(x[[1]]+targetWidth+searchSpace),
                                                              (x[[2]]-targetWidth-searchSpace):(x[[2]]+targetWidth+searchSpace)])
    
    sampleDiffs <- matrix(NA,nrow=1 + searchSpace*2,ncol=1 + searchSpace*2)
    
    for (ii in seq(1,searchSpace*2, by=3)) {
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
    
    if (getRunOptions('verbose')) cat(paste0('\t',names(imageList[[channel]])[i],' x=',offset.x[[i]],', y=',offset.y[[i]],'\n'))
    
  }
  
  profilePoint('alignment','seconds to calculate alignemnt offsets')
  
  
  # ----- Crop images ---------------------------------------------------------
  
  if (getRunOptions('verbose')) cat('\tcropping images ...\n')

    # Crop images based on offset
  cropY <- max(abs(offset.y)) + 1
  cropX <- max(abs(offset.x)) + 1
  
  dimx <- dim(imageList[[channel]][[1]])[[1]]
  dimy <- dim(imageList[[channel]][[1]])[[2]]
  
  if (getRunOptions('verbose')) cat('\taligning ...\n')

  for (channel in 1:length(imageList)) {
    for (timestep in 1:length(imageList[[channel]])) {
      im <- imageList[[channel]][[timestep]]
      imageList[[channel]][[timestep]] <- im[(cropX + offset.x[timestep]):(dimx - cropX + offset.x[timestep]),
                                             (cropY + offset.y[timestep]):(dimy - cropY + offset.y[timestep])]
    }
  }
  
  profilePoint('alignment','seconds to crop images')

    return(imageList)
  
}
