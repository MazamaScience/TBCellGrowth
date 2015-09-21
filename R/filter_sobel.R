#' @export
#' @title Sobel Edge Detection Filter
#' @param image image matrix to detect edges on
#' @description This is an implementation of the 3x3 sobel filter which 
#' is used by Photoshop and EBImage. 
#' @return an image of the same dimensions. 

# based on photoshop and imagejay implementation
# http://imagejdocu.tudor.lu/doku.php?id=faq:technical:what_is_the_algorithm_used_in_find_edges
filter_sobel <- function(image, blur=7, scalar=1) {
  
  if (blur!=FALSE) image <- filter_blur(image, 7)
  
  # Shift up
  p1 <- cbind(rbind(image,0,0),0,0)
  p2 <- cbind(0,rbind(image,0,0),0)
  p3 <- cbind(0,0,rbind(image,0,0))
  
  # Shift center
  p4 <- cbind(rbind(0,image,0),0,0)
  p6 <- cbind(0,0,rbind(0,image,0))
  
  # Shift down
  p7 <- cbind(rbind(0,0,image),0,0)
  p8 <- cbind(0,rbind(0,0,image),0)
  p9 <- cbind(0,0,rbind(0,0,image))
  
  im2 <- cbind(0,rbind(0,image,0),0)
  
  sum1 <- scalar * (p1 + 2*p2 + p3 - p7 - 2*p8 - p9)
  sum2 <- scalar * (p1  + 2*p4 + p7 - p3 - 2*p6 - p9)
  
  sum <- sqrt(sum1*sum1 + sum2*sum2)[c(-1, -dim(sum1)[[1]]),c(-1, -dim(sum1)[[2]])]
  sum[1:3,] <- 0
  sum[,1:3] <- 0
  sum[(dim(sum)[[1]]-3):(dim(sum)[[1]]),] <- 0
  sum[,(dim(sum)[[2]]-3):(dim(sum)[[2]])] <- 0
  
  sum[is.na(sum)] <- 0
  
  return(sum)
  
}

