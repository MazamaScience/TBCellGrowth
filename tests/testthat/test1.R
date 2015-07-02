# WORK COMPUTER
dataDirPhase <- "~/Desktop/TBData/xy6/Phase/"
dataDirGreen <- "~/Desktop/TBData/xy6/Green/"
dataDirRed <- "~/Desktop/TBData/xy6/Red/"

# LAPTOP
dataDirPhase <- "~/Desktop/tbtest/xy6/Phase/"
dataDirGreen <- "~/Desktop/tbtest/xy6/Green/"
dataDirRed <- "~/Desktop/tbtest/xy6/Red/"

# load images for each channel
phase <- loadImages(dataDirPhase, n=30, ext="tif")
green <- loadImages(dataDirGreen, n=30, ext="tif")
red   <- loadImages(dataDirRed, n=30, ext="tif")

# Get filenames except for background
filenames <- phase$filenames[-1]
stripFileNames <- function(f) {
  split1 <- strsplit(f, "/")[[1]] 
  split1 <- split1[length(split1)]
  split2 <- strsplit(split1, "_")[[1]][1]
  return(split2)
}
filenames <- unlist(lapply(filenames, stripFileNames))

# preprocess each channel
processed <- preprocessImages(phase$images, dyes=list(green=green$images, red=red$images), rotation=-1.2, crop=c(100,50,185,50), sample=c(600,100,100))

# extract preprocessed channels
phase <- processed$phase
dyes <- processed$dyes

# processed is big, remove it
rm(processed)

# create artifact from phase image
artifactMask <- createArtifactMask(phase[[1]])


phase.labeled <- lapply(phase, labelGroupsPhase, artifactMask)
dyes.labeled <- lapply(dyes, function(x) mapply(labelGroupsDye, x, phase.labeled, list(artifactMask), SIMPLIFY=FALSE))


output <- generateBlobTimeseries(phase.labeled, minTimespan=1)

dyeOverlap <- lapply(dyes.labeled, findDyeOverlap, phase.labeled, output)







buildDirectoryStructure(output, phase, phase.labeled, dyes.labeled, dyeOverlap, filenames)







# LAPTOP 1ST EXPERIMENT
dataDirPhase <- "~/Desktop/tbTest/tbdata/Phase 1/"

# load images for each channel
phase <- loadImages(dataDirPhase, n=40, ext="tif")

# Get filenames except for background
filenames <- phase$filenames[-1]
stripFileNames <- function(f) {
  split1 <- strsplit(f, "/")[[1]] 
  split1 <- split1[length(split1)]
  split2 <- strsplit(split1, "_")[[1]][1]
  return(split2)
}
filenames <- unlist(lapply(filenames, stripFileNames))

# preprocess each channel
processed <- preprocessImages(phase$images, dyes=list(), rotation=0.5, crop=c(50,150,575,150), sample=c(150,450,125))

# extract preprocessed channels
phase <- processed$phase
dyes <- processed$dyes

# processed is big, remove it
rm(processed)

# create artifact from phase image
artifactMask <- createArtifactMask(phase[[1]])


phase.labeled <- lapply(phase, labelGroupsPhase, artifactMask)
dyes.labeled <- lapply(dyes, function(x) mapply(labelGroupsDye, x, phase.labeled, list(artifactMask), SIMPLIFY=FALSE))


phase.labeled <- lapply(phase.labeled, function(x) )


output <- generateBlobTimeseries(phase.labeled, minTimespan=1, ignore=list(c(700,800),c(1600,1700)))

dyeOverlap <- lapply(dyes.labeled, findDyeOverlap, phase.labeled, output)





buildDirectoryStructure(output, phase, phase.labeled, list(), list(), filenames)












createGif(ble, "sample.gif", rescale = 75)

EBImage::display(overlayBlobs(frames$images[[2]], frames.labeled[[2]]))
EBImage::display(overlayGrid(frames$images[[2]]))
EBImage::display(overlayTimestamp(frames$images[[2]], frames$labels[[2]]))

