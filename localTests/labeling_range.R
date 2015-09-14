library(EBImage)
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
compare(equalized, labeled, 2)

means <- unlist(lapply(images, mean))
test <- lapply(images, as.numeric)
foo <- unlist(lapply(test, sample, 10000))
scalars <- means / mean(foo)

t1 <- mapply(function(m,s) m / s, images, scalars, SIMPLIFY=FALSE)

mins <-unlist(lapply(t1, min))

t2 <- mapply(function(m,s) m - s, t1, mins, SIMPLIFY=FALSE)

maxes <- unlist(lapply(t2, max))

t3 <- mapply(function(m,s) m / s, t2, maxes, SIMPLIFY=FALSE)

filtered <- lapply(t3, filter_sobel)
# quan <- lapply(equalized, quantile, na.rm=TRUE)
# plot(unlist(lapply(quan, function(x) x[[1]])), ylim=c(0,1))
# points(unlist(lapply(quan, function(x) x[[2]])))
# points(unlist(lapply(quan, function(x) x[[3]])))
# points(unlist(lapply(quan, function(x) x[[4]])))
# points(unlist(lapply(quan, function(x) x[[5]])))
