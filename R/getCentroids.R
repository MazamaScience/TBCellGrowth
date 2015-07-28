#' @export
#' @title Finds Blob Centroids
#' @param image a labeled image to search
#' @param distanceScale the distance scale in pixels / micrometers
#' @description Finds the center, size, and bounds of every non zero cluster
#' of pixels. Requires the image was labeled using \code{EBImage::bwlabel}.
#' @return A \code{dataframe} with a row for each unique blob.

getCentroids <- function(image, distanceScale=1) {
  
  # Initialize vectors
  x <- numeric(max(image))
  y <- numeric(max(image))
  xmin <- numeric(max(image))
  xmax <- numeric(max(image))
  ymin <- numeric(max(image))
  ymax <- numeric(max(image))
  size <- numeric(max(image))
  id <- character(max(image))
  index <- numeric(max(image))
  
  # Function for generating an ID
  generateID <- function(x, y, z) {
    return(paste0("x", x, "y", y,"z",z))
  }
  
  
  dimx <- dim(image)[1]
  dimy <- dim(image)[2]
  ### TEST ###
  values <- as.vector(image)
  indices <- 1:length(values)
  mask <- values != 0
  values <- values[mask]
  indices <- indices[mask]
  for (i in 1:max(image)) {
    ind <- indices[which(values==i)]
    yy <- floor(ind / dimx)
    xx <- ind - (yy * dimx)
    x[[i]] <- round(mean(xx))
    y[[i]] <- round(mean(yy))
    xmin[[i]] <- min(xx)
    xmax[[i]] <- max(xx)
    ymin[[i]] <- min(yy)
    ymax[[i]] <- max(yy)
    size[[i]] <- round(length(ind)*distanceScale)
    id[[i]] <- generateID(x[[i]],y[[i]],size[[i]])
    index[[i]] <- i
    
  }
  
  
  
  # Create a dataframe
  df <- data.frame(x=x,y=y,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,size=size,id=id,index=index)
  
  # Remove blobs that are of size 0
  df <- df[size>0,]
  
  return(df)
  
}

