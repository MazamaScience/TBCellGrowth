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
  
  dir.create(paste0(outputDir, "/fullFrame"))
  
  full_phase <- mapply(overlayColor, "phase", phase[-1], phase.labeled[-1], SIMPLIFY=FALSE)
  dir.create(paste0(outputDir, "/fullFrame/phase"))
  for (i in 1:length(full_phase)) {
    writeImage(full_phase[[i]], file=paste0(outputDir, "/fullFrame/phase/", filenames[[i]], ".jpg"))
  }
  
  for (dye in names(dyes.labeled)) {
    dir.create(paste0(outputDir, "/fullFrame/", dye))
    full_dye <- mapply(overlayColor, dye, phase[-1], dyes.labeled[[dye]][-1], full_phase, SIMPLIFY=FALSE)
    for (i in 1:length(full_dye)) {
      writeImage(full_dye[[i]], file=paste0(outputDir, "/fullFrame/", dye, "/", filenames[[i]], ".jpg"))
    }
  }
  
  # MULTIPLE dyes
  if (length(names(dyeOverlap)) > 0) {
    dir.create(paste0(outputDir, "/fullFrame/all"))
    # This mergewith list is added to each iteration with new dyes
    full_dye <- full_phase
    for (dye in names(dyes.labeled)) {
      dyes <- dyes.labeled[dye][[1]]
      full_dye <- mapply(overlayColor, dye, phase[-1], dyes.labeled[[dye]][-1], full_dye, SIMPLIFY=FALSE)
    }
    for (i in 1:length(full_dye)) {
      writeImage(full_dye[[i]], file=paste0(outputDir, "/fullFrame/all/", filenames[[i]], ".jpg"))
    }
  }
  
  remove(full_phase)
  remove(full_dye)
  
  for (id in names(output$timeseries)) {
    
    ####################################################
    ############## CREATE DIRECTORIES FOR EACH ID
    ####################################################
    
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
    
    
    
    
    ####################################################
    ############## ONLY PHASE
    ####################################################
    
    # Crop and color phase images
    cropped_phase <- cropImageByID(id, output, phase, phase.labeled)
    colored_phase <- mapply(overlayColor, "phase", cropped_phase$bg, cropped_phase$label, SIMPLIFY=FALSE)
    
    # Write phase images to directory
    for (i in 1:length(colored_phase)) {
      writeImage(colored_phase[[i]], file=paste0(outputDir, "/", id, "/phase/", filenames[[i]], ".jpg"))
    }
    
    ####################################################
    ############## SINGLE DYE + PHASE
    ####################################################
    
    for (dye in names(dyes.labeled)) {
      dyes <- dyes.labeled[dye][[1]]
      
      # Crop and color phase images
      cropped_dye <- cropImageByID(id, output, phase, dyes)
      colored_dye <- mapply(overlayColor, dye, cropped_dye$bg, cropped_dye$label, colored_phase, SIMPLIFY=FALSE)
      
      # Write images 
      for (i in 1:length(colored_dye)) {
        writeImage(colored_dye[[i]], file=paste0(outputDir, "/", id, "/", dye, "/", filenames[[i]], ".jpg"))
      }
      
    }
    
    ####################################################
    ############## ALL DYE + PHASE
    ####################################################
    
    # Write MULTIPLE dye images
    if (length(names(dyeOverlap)) > 0) {
      # This mergewith list is added to each iteration with new dyes
      mergeWith <- colored_phase
      for (dye in names(dyes.labeled)) {

        dyes <- dyes.labeled[dye][[1]]
        
        # Crop and color phase images
        cropped_dye <- cropImageByID(id, output, phase, dyes)
        mergeWith <- mapply(overlayColor, dye, cropped_dye$bg, cropped_dye$label, mergeWith, SIMPLIFY=FALSE) 
      }
      for (i in 1:length(mergeWith)) {
        writeImage(mergeWith[[i]], file=paste0(outputDir, "/", id, "/all/", filenames[[i]], ".jpg"))
      }
    }
    
  }
  
  ####################################################
  ############## CREATE EXCEL FILES
  ####################################################
  
  writeExcel(excel$phase, outputDir, "phase", filenames)
  for (dye in names(dyes.labeled)) {
    writeExcel(excel[[dye]], outputDir, dye, filenames)
  }
  
}

# Helper function, properly formats hyperlink function
excelHyperlink <- function(url, text) {
  return(paste0('=HYPERLINK("',url,'","',text,'")'))
}


# Accepts a table of values, the output directory, the current dye color,
# and a vector of times / filenames
writeExcel <- function(df, outputBase, color, filenames) {
  
  # Creates hyperlinks to specific images
  cellHyperlinks <- function(id) {
    oDir <- paste0(id, "/", color)
    cols <- df[id]
    for (i in 1:length(cols[[1]])) {
      filename <- paste0(oDir, "/", filenames[[i]], ".jpg")
      cols[id][[1]][[i]] <- excelHyperlink(filename, cols[id][[1]][[i]])
    }
    return(cols)
  }
  
  # Create hyperlinks on blob names
  colHyperlinks <- function(id) {
    oDir <- paste0(id, "/", color)
    return(excelHyperlink(paste0(id, "/", color),id))
  }
  
  # Create hyperlinks for full frames at time points
  timeHyperlinks <- function(time) {
    link <- paste0("fullFrame/", color, "/", time,".jpg")
    return(excelHyperlink(link,time))
  }
  
  df <- data.frame(lapply(names(df), cellHyperlinks))
  
  names(df) <- lapply(names(df), colHyperlinks)

  rownames(df) <- lapply(filenames, timeHyperlinks)
  
  write.csv(df, paste0(outputBase, "/", color, ".csv"))
  
}

