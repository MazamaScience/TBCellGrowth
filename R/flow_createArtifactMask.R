#' @export
#' @title Create a Mask of non-Biological Artifacts
#' @param bg preprocessed background image
#' @param maskDarkLines logical specifying whether or not dark lines should be masked out
#' @description Fluidomics apparatus used in phase microscopy have a number of physical
#' features that must be masked out before image processing algorithms to identify bacteria
#' are applied. This function is tailored to remove *pillars*, *squares* and the dark lines
#' that appear between different levels in the apparatus.
#' @return A \code{matrix} of boolean values with TRUE values identifying non biological artifacts. 

flow_createArtifactMask <- function(bg, maskDarkLines=FALSE) {
  
  edges <- sobel2(bg) > 0.8
  edges <- EBImage::dilateGreyScale(edges, makeBrush(3, 'disc'))
  edges <- EBImage::fillHull(edges)
  edges <- EBImage::erodeGreyScale(edges, makeBrush(3, 'disc'))
  
  return (edges)
  
  bg1[edges > 0] <- 0
  
  # size of raster
  dimX <- dim(bg)[[1]]
  dimY <- dim(bg)[[2]]
  
  # What percent of this matrix is "dark"
  percentDark <- function(m) {
    return(sum(m<0.5, na.rm=TRUE) / (dim(m)[[1]] * dim(m)[[2]]))
  }
  
  # is a significantly darker than b?
  isDarker <- function(a,b) {
    return(percentDark(a) - percentDark(b) > 0.28)
  }
  
  ####################################
  ####### REMOVING THE SQUARES #######
  ####################################
  
  # copy bg so we don't edit the original
  squares <- bg
  
  # Find areas of significant change to search
  sqRow <- diff(apply(bg,2,sd) > 0.2)
  sqRowStart <- which(sqRow==1)
  
  sqCol <- diff(apply(bg,1,sd) > 0.17)
  sqColStart <- which(sqCol==1)
  
  # Expand the search range a little bit
  xIndices <- unlist(lapply(sqColStart, function(x) seq(x-3, x+3, by=3)))
  yIndices <- unlist(lapply(sqRowStart, function(x) seq(x-3, x+3, by=3)))
  
  # Search indices for "square-like" regions
  for (x in xIndices) {
    for (y in yIndices) {
      
      # size of squares
      sqX <- 230
      sqY <- 230 
      
      # make sure we aren't checking outside the canvas
      if(x < (dimX-sqX) & y < (dimY-sqY)) {
        
        # get current subset
        subset <- bg[x:(x+(sqX-1)),y:(y+sqY-1)]
        
        # get y strips for comparison
        y11 <- subset[0:20, ]
        y12 <- subset[20:40, ]
        y21 <- subset[(sqX-20):sqX, ]
        y22 <- subset[(sqX-40):(sqX-20), ]
        
        # get x strips for comparison
        x11 <- subset[ ,0:20]
        x12 <- subset[ ,20:40]
        x21 <- subset[ ,(sqX-20):sqX]
        x22 <- subset[ ,(sqX-40):(sqX-20)]
        
        # get center
        cx1 <- round(sqX/2 - 10)
        cx2 <- round(sqX/2 + 10)
        cy1 <- round(sqY/2 - 10)
        cy2 <- round(sqY/2 + 10)
        center <- subset[cx1:cx2,cy1:cy2]
        
        # rules that squares tend to follow
        if (isDarker(y12,y11) & isDarker(y22,y21) 
            & isDarker(x12, x11) & isDarker(x22, x21)
            & percentDark(center) < 0.4) {
          # create a square mask. the mask is a little smaller than the sample
          # because I only want to remove the dark parts
          sqMask <- matrix(0, sqX, sqY)
          sqMask[1:10, ] <- 1
          sqMask[(sqX-10):sqX, ] <- 1
          sqMask[,1:10] <- 1
          sqMask[,(sqY-10):sqY] <- 1
          squares[x:(x+(sqX-1)),y:(y+sqY-1)] <- squares[x:(x+(sqX-1)),y:(y+sqY-1)] * sqMask
        }
        
      }
      
    }
  }
  
  ####################################
  ####### REMOVING THE CIRCLES #######
  ####################################
  
  # create a texture mask
  # use the contrast statistic (layer 5)
  a <- glcm::glcm(bg, n_grey=10, statistics=c("homogeneity"))[,,1] < 0.5
  
  # dilate and erode to close gaps in the middle of circles
  b <- EBImage::closingGreyScale(a, EBImage::makeBrush(23, shape='disc'))
  
  # add the square mask
  b[squares==0] <- 1
  
  # close gaps between partial circles and squares
  c <- EBImage::closingGreyScale(b, EBImage::makeBrush(15, shape='disc'))
  
  # Expand the circles a bit
  d <- EBImage::dilateGreyScale(c, EBImage::makeBrush(7, shape='disc'))
  
  e <- d
  ### REMOVE WHITE LINES ###
  for(y in seq(5,dimY-20,by=2)) {
    sample <- d[,y:(y+5)]
    if( sum(sample) / (dimX*6) > 0.8) {
      e[,(y-5):(y+20)] <- 0
    }
  }
  
  return(e)
  
}

# Input: x and y dimension of matrix, circle radius
# Returns a x by y matrix with a circle of radius r at the center
# Values inside of the circle are 0, outside are 1
createFillCircle <- function(dimX, dimY, r) {
  
  # Get x and y points of circle
  x1 <- unlist(lapply(seq(0, 2*pi, 0.05), function(a) round(dimX/2 + (r * cos(a)))))
  y1 <- unlist(lapply(seq(0, 2*pi, 0.05), function(a) round(dimY/2 + (r * sin(a)))))
  
  # Put points in dataframe
  points <- data.frame(x1, y1)
  
  # Initialize empty circle
  circle <- matrix(0, dimX, dimX)
  
  for (x in 1:dimX) {
    # Y values with the current x value
    yRange <- points[points$x1==x,]$y1
    if(length(yRange) < 1) {
      # If there are no y values, entire row is 1
      circle[x,] <- 1
    } else {
      # If there are, fill area outside of that range with 1s
      yRange <- range(yRange)
      circle[x,1:yRange[[1]]] <- 1
      circle[x,yRange[[2]]:dimY] <- 1
    }
  }
  
  return(circle)
  
}
