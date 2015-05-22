
dataDir <- "~/projects/TBTracking/images/full_post_cropped"

frames <- loadImages(dataDir, n=10, ext="tif")

artifactMask <- createArtifactMask(frames[[1]])

frames.labeled <- lapply(frames, labelColonies, artifactMask)

