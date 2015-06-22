#' @export
#' @title Builds and fills
#' @param dir path to the directory containing images
#' @param ext file extension
#' @param n number of images to load into memory (defaults to all)
#' @return A \code{list} with two attributes, "images" and "filenames".
#' Images is a \code{list} of image matrices extracted from the \code{EBImage}
#' \code{image} object. Filenames is a vector of filenames that correponds
#' to images.

buildDirectoryStructure <- function(output, phase, phase.labeled, 
                                    dyes.labeled, filenames, outputDir="output") {
  
  dir.create(outputDir)
  
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
    
    # Crop and color phase images
    cropped <- cropImageByID(id, output, phase, phase.labeled)
    colored <- mapply(overlayColor, "phase", cropped$bg, cropped$label, SIMPLIFY=FALSE)
    
    # Write images 
    for (i in 1:length(colored)) {
      writeImage(colored[[i]], file=paste0(outputDir, "/", id, "/phase/", filenames[[i]], ".jpg"))
    }
    
    
    for (dye in names(dyes.labeled)) {
      dyes <- dyes.labeled[dye][[1]]
      
      # Crop and color phase images
      cropped <- cropImageByID(id, output, phase, dyes)
      colored <- mapply(overlayColor, dye, cropped$bg, cropped$label, SIMPLIFY=FALSE)
      
      # Write images 
      for (i in 1:length(colored)) {
        writeImage(colored[[i]], file=paste0(outputDir, "/", id, "/", dye, "/", filenames[[i]], ".jpg"))
      }
      
    }
    
  }
  
}
  