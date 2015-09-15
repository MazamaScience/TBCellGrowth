#' @export
#' @title Adds ID Labels to Neighboring BLobs
#' @param image an image matrix
#' @param labels a matrix of blob labels
#' @param id the blob id the image is focused on
#' @param centroids a dataframe of blob centroids corresponding to this frame
#' @description Add text id labels to nearby blobs for cropped images.
#' @return a \code{matrix} image.


## DEBUG
# i = 2
# image <- color_phase[[i]]
# labels <- cropped_phase$labelFull[[i]]
# centroids <- output$centroids[[i]]

overlayNeighborsIDs <- function(image, labels, id, centroids) {
  
  if (sum(labels) < 1) return(image)
  
  #centroids = output$centroids[[1]]
  
  dimx <- dim(image)[[1]]
  dimy <- dim(image)[[2]]
  
  # Which ids are present in this image
  # Subtract the first index, it's always 0
  indices <- na.exclude(unique(c(labels)))[-1]
  
  # Subset centroids to those just found in the subset
  centroids <- centroids[which(centroids$index %in% indices),]
  
  # Remove main id from indices
  whichIsMain <- indices==centroids[centroids$id==id,]$index
  if (length(whichIsMain) > 0) {
    centroids <- centroids[centroids$index %in% indices[-which(whichIsMain)],]
  }
  
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
  
  ymask <- centroids$y > 40
  xmask <- centroids$x > 60
  
  centroids <- centroids[(xmask | ymask),]
  
  if (dim(centroids)[[1]] < 1) return(image)
  
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
  
  return(image)
  
}