#' @export
#' @title Sobel Edge Detection Filter
#' @param image image matrix to detect edges on
#' @description This is an implementation of the 3x3 sobel filter which 
#' is used by Photoshop and EBImage. 
#' @return an image of the same dimensions. 

# based on photoshop and imagejay implementation
# http://imagejdocu.tudor.lu/doku.php?id=faq:technical:what_is_the_algorithm_used_in_find_edges
filter_LoG <- function(image) {
  
  image <- filter_blur(image, 5)
  
  # Shift up
  p2 <- cbind(0,rbind(image,0,0),0)
  p4 <- cbind(rbind(0,image,0),0,0)
  p5 <- cbind(0,rbind(0,image,0),0)
  p6 <- cbind(0,0,rbind(0,image,0))
  p8 <- cbind(0,rbind(0,0,image),0)
  
  
  im2 <- cbind(0,rbind(0,image,0),0)
  
  sum1 <- 4*p5 - p2 - p4 - p6 - p8
  
  sum1[is.na(sum)] <- 0
  
  return(sum1)
  
}

