# WORK COMPUTER
dataDirPhase <- "~/Desktop/TBData/xy6/Phase/"
dataDirGreen <- "~/Desktop/TBData/xy6/Green/"
dataDirRed <- "~/Desktop/TBData/xy6/Red/"

# LAPTOP
dataDirPhase <- "~/Desktop/tbtest/xy6/Phase/"
dataDirGreen <- "~/Desktop/tbtest/xy6/Green/"
dataDirRed <- "~/Desktop/tbtest/xy6/Red/"

# Jon's desktop
if (FALSE) {
  dataDir <- '/Users/jonathancallahan/Projects/CIDR/ShermanData/Rif-INH_Drug_Tolerance_5-14-14_Chamber_C/'
  dataDirPhase <- paste0(dataDir,'xy6/Phase/')
  dataDirGreen <- paste0(dataDir,'xy6/Green/')
  dataDirRed <- paste0(dataDir,'xy6/Red/')
}

# load images for each channel
phase <- flow_loadImages(dataDirPhase, n=8, ext="tif")
green <- flow_loadImages(dataDirGreen, n=8, ext="tif")
red   <- flow_loadImages(dataDirRed, n=8, ext="tif")

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
dataDirPhase <- "~/Desktop/TBData/Phase 1/"

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
output$timeseries <- output$timeseries[,apply(output$timeseries, 2, function(x) sum(!is.na(x)) > 15)]


dyeOverlap <- lapply(dyes.labeled, findDyeOverlap, phase.labeled, output)

output$timeseries <- output$timeseries[,apply(output$timeseries, 2, function(x) sum(!is.na(x)) > 14)]




buildDirectoryStructure(output, phase, phase.labeled, list(), list(), filenames)





phase <- flow_loadImages("~/Desktop/TBData/Phase 1/", n=20, ext="tif", start=1)[[1]]


### AUTO NORMALIZATION TESTING
g <- green[[5]]
r <- red[[5]]
p <- phase[[5]]

# Reset median
g1 <- normalizeValues(green[[5]], 0.2)
r1 <- normalizeValues(red[[5]], 0.2)
p1 <- normalizeValues(phase[[5]], 0.5)

test <- lapply(phase, normalizeValues, 0.5)

normalizeValues <- function(m, med) {
  m <- m - min(m)
  m <- m * (med / median(m))
  return(m)
}



