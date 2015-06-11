# WORK COMPUTER
dataDirPhase <- "~/Desktop/TBData/xy6/Phase/"
dataDirGreen <- "~/Desktop/TBData/xy6/Green/"
dataDirRed <- "~/Desktop/TBData/xy6/Red/"

# LAPTOP
dataDirPhase <- "~/Desktop/tbtest/xy6/Phase/"
dataDirGreen <- "~/Desktop/tbtest/xy6/Green/"
dataDirRed <- "~/Desktop/tbtest/xy6/Red/"

# load images for each channel
phase <- loadImages(dataDirPhase, n=7, ext="tif")$images
green <- loadImages(dataDirGreen, n=7, ext="tif")$images
red   <- loadImages(dataDirRed, n=7, ext="tif")$images

# preprocess each channel
processed <- preprocessImages(phase, dyes=list(green, red), rotation=-1.2, crop=c(100,50,185,50), sample=c(600,100,100))

# extract preprocessed channels
phase <- processed$phase
dyes <- processed$dyes

# processed is big, remove it
rm(processed)

# create artifact from phase image
artifactMask <- createArtifactMask(phase[[1]])


phase.labeled <- lapply(phase, labelGroupsPhase, artifactMask)
dyes.labeled <- lapply(dyes, function(x) lapply(x, labelGroupsDye, artifactMask))


output <- generateBlobTimeseries(phase.labeled, minTimespan=1)

dyeOverlap <- lapply(dyes.labeled, findDyeOverlap, phase.labeled, output)










test <- function(p,g,r) {
  return(colorDyes(p,g,r))
}

ble <- mapply(test, phase,green.labeled, red.labeled, SIMPLIFY=FALSE)

createGif(ble, "sample.gif", rescale = 75)




EBImage::display(overlayBlobs(frames$images[[2]], frames.labeled[[2]]))
EBImage::display(overlayGrid(frames$images[[2]]))
EBImage::display(overlayTimestamp(frames$images[[2]], frames$labels[[2]]))
