#' @export
#' @title Builds A Directory of Image With Excel UI
#' @param output an output object with timeseries and centroids from
#' \link{generateBlobTimeseries}
#' @param phase a list of phase contrast image matrices
#' @param phase.labeled a list of labeled phase images from
#' \link{flow_labelPhase}
#' @param dyes.labeled a list of labeled dye images from
#' \link{flow_labelDye}
#' @param dyeOverlap a \code{dataframe} of dye overlap values from
#' \link{findDyeOverlap}
#' @param filenames a vector of names equal to the length of the 
#' image lists. Usually timesteps.
#' @param outputDir the directory to build the file structure
#' @description write a lot more here TODO
#' @return none

buildDirectoryStructure <- function(output, phase, phase.labeled, 
                                    dyes.labeled,  dyeOverlap, filenames, outputDir="output") {
  
  dir.create(outputDir)
  
  excel <- lapply(dyeOverlap, function(x) x)
  excel$phase <- output$timeseries
  
  dir.create(paste0(outputDir, "/fullFrame"))
  
  full_phase <- mapply(overlayColor, "phase", phase[-1], phase.labeled[-1], SIMPLIFY=FALSE)
  dir.create(paste0(outputDir, "/fullFrame/phase"))
  for (i in 1:length(full_phase)) {
    EBImage::writeImage(full_phase[[i]], file=paste0(outputDir, "/fullFrame/phase/", filenames[[i]], ".jpg"))
  }
  
  makeGif(paste0(outputDir,"/fullFrame/phase/"), "series.gif")
  
  for (dye in names(dyes.labeled)) {
    dir.create(paste0(outputDir, "/fullFrame/", dye))
    full_dye <- mapply(overlayColor, dye, phase[-1], dyes.labeled[[dye]][-1], full_phase, SIMPLIFY=FALSE)
    for (i in 1:length(full_dye)) {
      EBImage::writeImage(full_dye[[i]], file=paste0(outputDir, "/fullFrame/", dye, "/", filenames[[i]], ".jpg"))
    }
    makeGif(paste0(outputDir,"/fullFrame/",dye), "series.gif")
    remove(full_dye)
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
      EBImage::writeImage(full_dye[[i]], file=paste0(outputDir, "/fullFrame/all/", filenames[[i]], ".jpg"))
    }
    makeGif(paste0(outputDir,"/fullFrame/all/"), "series.gif")
    
  }
  
  
  remove(full_phase)
  
  
  
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
      EBImage::writeImage(colored_phase[[i]], file=paste0(outputDir, "/", id, "/phase/", filenames[[i]], ".jpg"))
    }
    
    makeGif(paste0(outputDir,"/",id,"/phase/"), "series.gif")
    
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
        EBImage::writeImage(colored_dye[[i]], file=paste0(outputDir, "/", id, "/", dye, "/", filenames[[i]], ".jpg"))
      }
      
      makeGif(paste0(outputDir,"/",id,"/",dye,"/"), "series.gif")
      
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
        EBImage::writeImage(mergeWith[[i]], file=paste0(outputDir, "/", id, "/all/", filenames[[i]], ".jpg"))
      }
      
      makeGif(paste0(outputDir,"/",id,"/all/"), "series.gif")
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

# Create a gif in a directory from all of the images in that directory
makeGif <- function(dir, filename, ext="jpg", delay=15, rescale=80) {
  system(paste0('convert -resize "', rescale, '%" -delay  ', delay, ' ', dir, '*.', ext, " ", dir, filename))
}

