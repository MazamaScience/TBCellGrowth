
dataDirPhase <- "~/Desktop/TBData/xy6/Phase/"
dataDirGreen <- "~/Desktop/TBData/xy6/Green/"
dataDirRed <- "~/Desktop/TBData/xy6/Red/"

phase <- loadImages(dataDirPhase, n=10, ext="tif")$images
green <- loadImages(dataDirGreen, n=10, ext="tif")$images
red   <- loadImages(dataDirRed, n=10, ext="tif")$images

bleh <- preprocessImages(phase, green, red, rotation=-1.5)

phase <- bleh$phase
green <- bleh$green
red   <- bleh$red

artifactMask <- createArtifactMask(phase[[1]])

phase.labeled <- lapply(phase, labelGroupsPhase, artifactMask)
green.labeled <- labelGroupsPhase(green, artifactMask)
red.labeled   <- labelGroupsPhase(red, artifactMask)




artifactMask <- createArtifactMask(frames$images[[1]])

frames.labeled <- lapply(frames$images, labelColonies, artifactMask)



EBImage::display(overlayBlobs(frames$images[[2]], frames.labeled[[2]]))
EBImage::display(overlayGrid(frames$images[[2]]))
EBImage::display(overlayTimestamp(frames$images[[2]], frames$labels[[2]]))
