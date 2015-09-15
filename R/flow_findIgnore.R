#' @export
#' @title Find Boundaries of Ignored Areas
#' @param ignore a vector of ignore strings (i.e. "topRight")
#' @param dims the dimensions of the image
#' @description In some cases, Fluidomics image stitched together with 
#' 9 images by the microscope results in unusable sections. This function
#' takes strings such as "topRight" and "center" and returns pixel 
#' boundaries based on those strings.
#' @return A \code{data frame} of rectangle boundaries corresponing to 
#' ignore regions.

flow_findIgnore <- function(ignore, dims) {
  
  if (length(ignore) < 1) {
    return(data.frame(x1=c(),x2=c(),y1=c(),y2=c()))
  }

  boundaryList <- list("topLeft"=     c(1, dims[1]/3, 1, dims[2]/3),
                     "topCenter"=     c(dims[1]/3, 2*dims[1]/3, 1, dims[2]/3),
                     "topRight"=      c(2*dims[1]/3, dims[1], 1, dims[2]/3),
                     "left"=          c(1, dims[1]/3, dims[2]/3, 2*dims[2]/3),
                     "center"=        c(dims[1]/3, 2*dims[1]/3, dims[2]/3, 2*dims[2]/3),
                     "right"=         c(2*dims[1]/3, dims[1], dims[2]/3, 2*dims[2]/3),
                     "bottomLeft"=    c(1, dims[1]/3, 2*dims[2]/3, dims[2]),
                     "bottomCenter"=  c(dims[1]/3, 2*dims[1]/3, 2*dims[2]/3, dims[2]),
                     "bottomRight"=   c(2*dims[1]/3, dims[1], 2*dims[2]/3, dims[2]))
  
  boundaries <- do.call(rbind, lapply(ignore, function(x) floor(boundaryList[[x]])))
  colnames(boundaries) <- c("x1", "x2", "y1", "y2")
  
  return(boundaries)
  
}


