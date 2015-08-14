#' @export
#' @title Adds ID Labels to Neighboring BLobs
#' @param image an image matrix
#' @param id the blob id the image is focused on
#' @param time the timestep at this frame
#' @param size the object size in um
#' @param distanceScale the distance conversion in um/pixel
#' @description Overylay a series of statistics to an image
#' from cropImageByID. TODO add more details when they are
#' avaiable
#' @return a \code{matrix} image.

overlayNeighborsIDs <- function(image, labels, id, centroids) {
  
  dimx <- dim(image)[[1]]
  dimy <- dim(image)[[2]]
  
  # Which ids are present in this image
  # Subtract the first index, it's always 0
  indices <- na.exclude(unique(c(labels)))[-1]
  
  # Subset centroids to those just found in the subset
  centroids <- centroids[which(centroids$index %in% indices),]
  
  # Remove main id from indices
  centroids <- centroids[centroids$index %in% indices[-which(indices==centroids[centroids$id==id,]$index)],]
  
  # Find new centroids for just this image
  for (index in centroids$index) {
    ind <- which(labels == index, arr.ind=T)
    centroids[centroids$index==index,]$x <- round(mean(ind[,1]))
    centroids[centroids$index==index,]$y <- round(mean(ind[,2]))
    centroids[centroids$index==index,]$xmin <- min(ind[,1])
    centroids[centroids$index==index,]$xmax <- max(ind[,1])
    centroids[centroids$index==index,]$ymin <- min(ind[,2])
    centroids[centroids$index==index,]$ymax <- max(ind[,2])
  }
  
  ymask <- centroids$y < 40
  xmask <- centroids$x < 60
  
  centroids <- centroids[!xmask & !ymask,]
  
  plotf <- function() {
    for (ii in 1:dim(centroids)[[1]]) {
      cen <- centroids[ii,]
      if (cen$x < dimx/2) {
        adj <- c(0,0.5)
      } else {
        adj <- c(1,0.5) 
      }
      x <- ifelse(cen$x < dimx/2, cen$xmax, cen$xmin)
      text(x=x, y=cen$y, labels=cen$id, adj=adj, cex=0.8)
    }
  }
  
  overlay <- plotToOverlay(plotf, dimx, dimy)
  
  image[overlay > 0.4] <- 1
  
}