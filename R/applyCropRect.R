# width = params$cropRectX
# height = params$cropRectY

applyCropRect <- function(im, width, height) {
  
  dimx <- dim(im)[[1]]
  dimy <- dim(im)[[2]]
  
  x1 <- floor(dimx/2) - floor(width/2)
  x2 <- floor(dimx/2) + floor(width/2)
  y1 <- floor(dimy/2) - floor(height/2)
  y2 <- floor(dimy/2) + floor(height/2)
  
    
  return(im[x1:x2,y1:y2])
  
  
}