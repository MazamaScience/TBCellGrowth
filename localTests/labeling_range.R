library(EBImage)
### Testing large range of possible images fed through 
### the equalization and labeling pipelines
loadImages_ <- function(dir) {
  files <- list.files(dir, full.names = TRUE)
  return(lapply(files, function(x) readImage(x)@.Data))
}

labelImages_ <- function(image, eq) {
  
#   i <- 1
#   image = images[[i]]
#   eq = equalized[[i]]
#  
  filtered <- filter_sobel(eq)
  
  test <- eq + filtered
  test <- test ^ 2.5
  test <- test + filtered
  test[test > 1] <- 1
  test <- test > (0.2 + sd(filtered[filtered > 0.3]))
  
  test <- closingGreyScale(test, makeBrush(9, "disc"))
  
  test <- fillHull(test)
  test <- removeBlobs(test, 75)
  
  return(overlayOutlines(eq, test))
}

t1 <- unlist(lapply(equalized[1:4], function(x) {
  x1 <- filter_sobel(x)
  sd(x1[x1 > 0.5])
}))

test <- mapply(labelImages_, images, equalized, SIMPLIFY=FALSE)

unlist(lapply(c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), test))

#  1    2  3  4  5  6  7  8 
# .9   .5  x .8
# 

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



image <- (image - 0.24) * 5

