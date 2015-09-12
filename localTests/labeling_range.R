### Testing large range of possible images fed through 
### the equalization and labeling pipelines
loadImages_ <- function(dir) {
  files <- list.files(dir, full.names = TRUE)
  return(lapply(files, function(x) readImage(x)@.Data))
}

compare <- function(im, label, index) {
  display(im[[index]])
  display(label[[index]])
}

images <- loadImages_("localData/solid_phase_samples/")
equalized <- lapply(images, solid_equalizePhase)
labeled <- lapply(equalized, solid_labelPhase)
compare(equalized, labeled, 3)
