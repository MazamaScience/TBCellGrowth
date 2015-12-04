#' @export
#' @title Find Blob Centroids
#' @param image a labeled image to search
#' @description Finds the center, size, and bounds of every non zero cluster
#' of pixels in a labeled image. Image labeling is performed by \code{EBImage::bwlabel}.
#' @return A \code{dataframe} with a row for each unique blob.

getCentroids <- function(image) {
  
  # Number of blobs that were identified
  blobCount <- max(image)
  
  # Initialize vectors 
  x <- numeric(blobCount)
  y <- numeric(blobCount)
  xmin <- numeric(blobCount)
  xmax <- numeric(blobCount)
  ymin <- numeric(blobCount)
  ymax <- numeric(blobCount)
  size <- numeric(blobCount)
  id <- character(blobCount)
  index <- numeric(blobCount)
  
  # Image dimensions
  dimx <- dim(image)[1]
  dimy <- dim(image)[2]
  
  # Convert image matrix to a vector
  values <- as.vector(image)    # unraveled image matrix
  indices <- 1:length(values)   # associated 1-D indices
  mask <- values != 0
  values <- values[mask]
  indices <- indices[mask]
  
  # Loop through each blob index
  for (blobIndex in 1:blobCount) {
    
    # Find indices associated with this blob index
    ind <- indices[values==blobIndex]
    
    # Calculate the row and column for each pixel in this blob
    xx <- ind %% dimx
    xx[xx==0] <- dimx           # zero values need to be reset to dimx
    yy <- (ind-1) %/% dimx + 1  # columns start with 1, not zero

    # Fill in the arrays
    x[blobIndex] <- round(mean(xx))
    y[blobIndex] <- round(mean(yy))
    xmin[blobIndex] <- min(xx)
    xmax[blobIndex] <- max(xx)
    ymin[blobIndex] <- min(yy)
    ymax[blobIndex] <- max(yy)
    size[blobIndex] <- length(ind)
    id[blobIndex] <- paste0("id", paste0(sample(c(letters,LETTERS,0:9),12,replace=TRUE), collapse=""))
    # NOTE:  Generating an id by center position, which seems reasonable, generates the following error message:
    # NOTE:  
    # NOTE:  Error in fix.by(by.y, y) : 'by' must specify uniquely valid columns
    # NOTE:  Calls: generateBlobTimeseries -> merge -> merge.data.frame -> fix.by
    # NOTE:  In addition: There were 50 or more warnings (use warnings() to see the first 50)
    # NOTE:  Execution halted
    #### Create an "id" based on the center. Guaranteed to be unique as blobs cannot overlap
    ###id[blobIndex] <- paste0('x=',x[blobIndex],',y=',y[blobIndex])
    index[blobIndex] <- blobIndex
    
  } 
  
  # Create a dataframe
  df <- data.frame(x,y,xmin,xmax,ymin,ymax,size,id,index,
                   stringsAsFactors=FALSE)
  
  # Remove blobs that are of size 0
  df <- df[size>0,]
  
  return(df)
  
}

