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
                                    dyes.labeled,  dyeOverlap, filenames, outputDir="output") {
  
  dir.create(outputDir)
  
  excel <- lapply(dyeOverlap, function(x) x)
  excel$phase <- output$timeseries
  
  dir.create(paste0(outputDir, "/all"))
  dir.create(paste0(outputDir, "/all/phase"))
  
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
    cropped_phase <- cropImageByID(id, output, phase, phase.labeled)
    colored_phase <- mapply(overlayColor, "phase", cropped_phase$bg, cropped_phase$label, SIMPLIFY=FALSE)
    
    # Write images 
    for (i in 1:length(colored)) {
      writeImage(colored_phase[[i]], file=paste0(outputDir, "/", id, "/phase/", filenames[[i]], ".jpg"))
    }
    
    
    
    
    
    for (dye in names(dyes.labeled)) {
      dyes <- dyes.labeled[dye][[1]]
      
      # Crop and color phase images
      cropped_dye <- cropImageByID(id, output, phase, dyes)
      colored_dye <- mapply(overlayColor, dye, cropped_dye$bg, cropped_dye$label, colored_phase, SIMPLIFY=FALSE)
      
      # Write images 
      for (i in 1:length(colored)) {
        writeImage(colored[[i]], file=paste0(outputDir, "/", id, "/", dye, "/", filenames[[i]], ".jpg"))
      }
      
    }
    
  }
  
  writeExcel(excel$phase, outputDir, "phase", filenames)
  for (dye in names(dyes.labeled)) {
    writeExcel(excel[[dye]], outputDir, dye, filenames)
  }
  
}

excelHyperlink <- function(url, text) {
  return(paste0('=HYPERLINK("',url,'","',text,'")'))
}

writeExcel <- function(df, outputBase, color, filenames) {
  
  cellHyperlinks <- function(id) {
    oDir <- paste0(id, "/", color)
    cols <- df[id]
    for (i in 1:length(cols[[1]])) {
      filename <- paste0(oDir, "/", filenames[[i]], ".jpg")
      cols[id][[1]][[i]] <- excelHyperlink(filename, cols[id][[1]][[i]])
    }
    return(cols)
  }
  
  colHyperlinks <- function(id) {
    oDir <- paste0(id, "/", color)
    return(excelHyperlink(paste0(id, "/", color),id))
  }
  
  
  df <- data.frame(lapply(names(df), cellHyperlinks))
  
  names(df) <- lapply(names(df), colHyperlinks)
  
  write.csv(df, paste0(outputBase, "/", color, ".csv"))
  
}

