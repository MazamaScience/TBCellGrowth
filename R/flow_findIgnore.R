#' @export
#' @title Find Boundaries of Ignored Areas
#' @param ignore a vector of ignore strings (i.e. "topRight")
#' @param dimensions the dimensions of the image
#' @description In some cases, Fluidomics image stitched together with 
#' 9 images by the microscope results in unusable sections. This function
#' takes strings such as "topRight" and "center" and returns pixel 
#' boundaries based on those strings.
#' @return A \code{data frame} of rectangle boundaries corresponing to 
#' ignore regions.

flow_findIgnore <- function(ignore, dd) {
  
  if (length(ignore) < 1) {
    return(data.frame(x1=c(),x2=c(),y1=c(),y2=c()))
  }

  boundaryList <- list("topLeft"=     c(1, dd[1]/3, 1, dd[2]/3),
                     "topCenter"=     c(dd[1]/3, 2*dd[1]/3, 1, dd[2]/3),
                     "topRight"=      c(2*dd[1]/3, dd[1], 1, dd[2]/3),
                     "left"=          c(1, dd[1]/3, dd[2]/3, 2*dd[2]/3),
                     "center"=        c(dd[1]/3, 2*dd[1]/3, dd[2]/3, 2*dd[2]/3),
                     "right"=         c(2*dd[1]/3, dd[1], dd[2]/3, 2*dd[2]/3),
                     "bottomLeft"=    c(1, dd[1]/3, 2*dd[2]/3, dd[2]),
                     "bottomCenter"=  c(dd[1]/3, 2*dd[1]/3, 2*dd[2]/3, dd[2]),
                     "bottomRight"=   c(2*dd[1]/3, dd[1], 2*dd[2]/3, dd[2]))
  
  boundaries <- do.call(rbind, lapply(ignore, function(x) floor(boundaryList[[x]])))
  colnames(boundaries) <- c("x1", "x2", "y1", "y2")
  
  return(boundaries)
  
}


