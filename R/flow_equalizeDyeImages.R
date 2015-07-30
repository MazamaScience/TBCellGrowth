

flow_equalizeDyeImages <- function(im, artifactMask) {
  
#   im <- xy$green[[2]]
  
  im[artifactMask] <- NA
  
  im <- im - mean(im, na.rm=TRUE)
  im[im < 0] <- NA
  
  im[im > 0.1] <- 0.1
  
  h <- hist(im, breaks=seq(0,0.1, by=0.001), plot=FALSE)
  
  keybreak <- h$mids[h$counts < (max(h$counts)/200)][1]
  
  im <- im / keybreak
  
  return(im)
  
}

