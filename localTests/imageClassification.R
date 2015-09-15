library(EBImage)
library(TBCellGrowth)

loadImages <- function(dir) {
  files <- list.files(dir, full.names = TRUE)
  return(lapply(files, function(x) readImage(x)@.Data))
}

compare <- function(im, label, index) {
  display(im[[index]])
  display(label[[index]])
}

im_e <- loadImages("localData/solid_early")
im_m <- loadImages("localData/solid_mid")
im_l <- loadImages("localData/solid_late")
im_h <- loadImages("localData/solid_harsh")
im_nd <- loadImages("localData/solid_nodata")

eq_e <- lapply(im_e, solid_equalizePhase)
eq_m <- lapply(im_m, solid_equalizePhase)
eq_l <- lapply(im_l, solid_equalizePhase)
eq_h <- lapply(im_h, solid_equalizePhase)
eq_nd <- lapply(im_nd, solid_equalizePhase)
