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
processed <- preprocessImages(phase, dyes=list(green=green, red=red), rotation=-1.2, crop=c(100,50,185,50), sample=c(600,100,100))

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






######### TEST ##################
######### TEST ##################
######### TEST ##################
# Experiments with directory layout and image saving


outputDir <- "output"

dir.create(outputDir)

# Create subdirectories named for IDs 
for (id in names(output$timeseries)) {
  # Blob directory
  dir.create(paste0(outputDir, "/", id))  
  # Phase directory
  dir.create(paste0(outputDir, "/", id, "/phase"))
  # If there are dyes, make a directory for all colors
  if (length(names(dyeOverlap)) > 0) {
    dir.create(paste0(outputDir, "/", id, "/", "all"))
  }
  # Make a directory for each dye
  for (dye in names(dyeOverlap)) {
    dir.create(paste0(outputDir, "/", id, "/", dye))
  }
}


## INPUT
# centroids (position info and label id)
# phase (background image)
# labeled image (either phase.labeled or a dye)
# a blob id (you'd lapply this function)
# the size of the cropped labeled image
# a color?
## OUTPUT
# the cropped and colored image, ready to be saved

timeseries <- output$timeseries
centroids <- output$centroids
id <- "x1112y476z68"

# Ignore the first index here, it's empty (bg frame)
centroids[[1]] <- NULL

dimx <- dim(phase[[1]])[[1]]
dimy <- dim(phase[[1]])[[2]]

# The width and height of the cropped image
width <- 100
height <- 100

series <- timeseries[id]
cx <- lapply(centroids, function(x) x[x$id == id,])

# Find the mean center of this blob
meanx <- round(mean(unlist(lapply(cx, function(x) x$x))))
meany <- round(mean(unlist(lapply(cx, function(x) x$y))))

# Make sure center doesn't go off the edge of the image
meanx <- max(meanx, width+1)
meanx <- min(meanx, dimx - width)
meany <- max(meany, height+1)
meany <- min(meany, dimy - height)

# Crop the area
cropbg <- phase[[5+1]][(meanx-width):(meanx+width), (meany-height):(meany+height)]
labelbg <- phase.labeled[[5+1]][(meanx-width):(meanx+width), (meany-height):(meany+height)]
labelbg <- labelbg == cx[[5]]$index

test <- colorDyes("green", cropbg, labelbg)
test <- colorDyes("red", cropbg, labelbg)
test <- colorDyes("grey", cropbg, labelbg)
test <- colorDyes("orange", cropbg, labelbg)

######### TEST ##################
######### TEST ##################
######### TEST ##################

# COLORING FUNCTION


colorDyes <- function(color, bg, label) {
  
  colors <- list(green = c(0,0.5,0.1),
                 red = c(0.5,0,0),
                 orange = c(0.5,0.25,0),
                 grey = c(0.7,0.7,0.7))
  
  color <- colors[color][[1]]
  
  # Stack three layers of phase to make a 3d matrix
  stack <- simplify2array(list(bg,bg,bg))
  
  # With colormode=Color, the 3 matrix layers are interpreted as RBG channels
  image <- EBImage::Image(stack, colormode="Color") 
  
  # red, green, blue channels
  image[,,1] <- bg + (label > 0 ) * color[[1]]
  image[,,2] <- bg + (label > 0 ) * color[[2]]
  image[,,3] <- bg + (label > 0 ) * color[[3]]
  
  return(image)
}




######### TEST ##################
######### TEST ##################
######### TEST ##################






test <- function(p,g,r) {
  return(colorDyes(p,g,r))
}

ble <- mapply(test, phase,green.labeled, red.labeled, SIMPLIFY=FALSE)

createGif(ble, "sample.gif", rescale = 75)




EBImage::display(overlayBlobs(frames$images[[2]], frames.labeled[[2]]))
EBImage::display(overlayGrid(frames$images[[2]]))
EBImage::display(overlayTimestamp(frames$images[[2]], frames$labels[[2]]))
