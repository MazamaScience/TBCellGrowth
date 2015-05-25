
dataDir <- "~/projects/TBTracking/images/full_post_cropped"

frames <- loadImages(dataDir, n=10, ext="tif")


artifactMask <- createArtifactMask(frames$images[[1]])

frames.labeled <- lapply(frames$images, labelColonies, artifactMask)



EBImage::display(overlayBlobs(frames$images[[2]], frames.labeled[[2]]))
EBImage::display(overlayGrid(frames$images[[2]]))
EBImage::display(overlayTimestamp(frames$images[[2]], frames$labels[[2]]))
