#' @export
#' @title Finds Blob Centroids
#' @param image a labeled image to search
#' @description Finds the center, size, and bounds of every non zero cluster
#' of pixels. Requires the image was labeled using \code{EBImage::bwlabel}.
#' @return A \code{dataframe} with a row for each unique blob.

getCentroids <- function(image) {
  
  # Initialize vectors
  x <- numeric(max(image))
  y <- numeric(max(image))
  ymin <- numeric(max(image))
  size <- numeric(max(image))
  id <- character(max(image))
  index <- numeric(max(image))
  
  # Function for generating an ID
  generateID <- function(x, y,z) {
    return(paste0("x", x, "y", y,"z",z))
  }
  
  # For each blob find centroids and size
  for (i in 1:max(image)) {
    m1 <- image == i
    ind <- which(m1, arr.ind=T)
    x[[i]] <- mean(ind[,1])
    y[[i]] <- mean(ind[,2])
    ymin[[i]] <- min(ind[,2])
    size[[i]] <- sum(m1)
    id[[i]] <- generateID(round(mean(ind[,1])),round(mean(ind[,2])),sum(m1))
    index[[i]] <- i
  }
  
  # Create a dataframe
  df <- data.frame(x=x,y=y,ymin=ymin,size=size,id=id,index=index)
  
  # Remove blobs that are of size 0
  df <- df[size>0,]
  
  return(df)
  
}