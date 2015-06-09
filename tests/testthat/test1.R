# WORK COMPUTER
dataDirPhase <- "~/Desktop/TBData/xy6/Phase/"
dataDirGreen <- "~/Desktop/TBData/xy6/Green/"
dataDirRed <- "~/Desktop/TBData/xy6/Red/"

# LAPTOP
dataDirPhase <- "~/Desktop/tbtest/xy6/Phase/"
dataDirGreen <- "~/Desktop/tbtest/xy6/Green/"
dataDirRed <- "~/Desktop/tbtest/xy6/Red/"

# load images for each channel
phase <- loadImages(dataDirPhase, n=10, ext="tif")$images
green <- loadImages(dataDirGreen, n=10, ext="tif")$images
red   <- loadImages(dataDirRed, n=10, ext="tif")$images

# preprocess each channel
processed <- preprocessImages(phase, green, red, rotation=-1.2, crop=c(50,50,50,50))

# extract preprocessed channels
phase <- processed$phase
green <- processed$green
red   <- processed$red

# processed is big, remove it
rm(processed)

# create artifact from phase image
artifactMask <- createArtifactMask(phase[[1]])


phase.labeled <- lapply(phase, labelGroupsPhase, artifactMask)
green.labeled <- lapply(green, labelGroupsGreen, artifactMask)
red.labeled   <- lapply(red, labelGroupsGreen, artifactMask)


#### overlap
## dyeOverlap?

output <- generateBlobTimeseries(phase.labeled)


tf <- output$timeseries
tf[,] <- 0

for (i in 1:dim(tf)[[1]]) {
  
  g <- green.labeled[[i+1]]
  p <- phase.labeled[[i+1]]
  c <- output$centroids[[i+1]]

  for (j in 1:max(g)) {
    # overlap mask: which coordinates in phase are dyed?
    overlap <- p[ g == j ]
    # If at least 25% overlap
    if (sum(overlap>0) > length(overlap)/4) {
      overlap <- overlap[overlap>0]
      unq <- unique(overlap)
      index <- unq[which.max(tabulate(match(overlap, unq)))]
      id <- as.character(c[c$index == index,]$id)
      
      # WHY wouldn't this work
      if (id %in% names(tf)) tf[[id]][[i]] <- sum(overlap > 0)
      
    }
  }

}








EBImage::display(overlayBlobs(frames$images[[2]], frames.labeled[[2]]))
EBImage::display(overlayGrid(frames$images[[2]]))
EBImage::display(overlayTimestamp(frames$images[[2]], frames$labels[[2]]))
