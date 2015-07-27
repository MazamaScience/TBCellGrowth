#' @export
#' @title Builds A Directory of Image With Excel UI
#' @param output an output object with timeseries and centroids from
#' \link{generateBlobTimeseries}
#' @param phase a list of phase contrast image matrices
#' @param labeled a list of labeled images from each channel
#' \link{flow_labelPhase}
#' @param dyeOverlap a \code{dataframe} of dye overlap values from
#' \link{findDyeOverlap}
#' @param filenames a vector of names equal to the length of the 
#' image lists. Usually timesteps.
#' @param outputDir the directory to build the file structure
#' @description write a lot more here TODO
#' @return none

buildDirectoryStructure <- function(output, phase, labeled, dyeOverlap, filenames, outputDir="output") {
  
  dir.create(outputDir)
  
  # Merge timeseries together
  excel <- lapply(dyeOverlap, function(x) x)
  excel$phase <- output$timeseries
  
  # Remove the background frames from images
  # TODO can't assume there are background frames because of solid images
#   phase <- phase[-1]
#   labeled <- lapply(labeled, function(x) x[-1])
  
  
  
  #################################################################
  #################################################################
  
  ### FULL FRAME ###
  dir.create(paste0(outputDir, "/fullFrame"))
  
  # Add overlays to phase
  # These overlays will also serve as a background to other channels
  #full_overlay <- mapply(overlayColor, "phase", phase, labeled$phase, SIMPLIFY=FALSE)
  full_overlay <- mapply(overlayOutlines, phase, labeled$phase, col="yellow", SIMPLIFY=FALSE)
  writeImages(full_overlay, outputDir, "fullFrame", "phase", filenames)
  
  # Write non phase channels
  for (cName in names(labeled)[names(labeled) != "phase"]) {
    channel <- labeled[[cName]]
    overlay <- mapply(overlayColor, cName, phase, channel, full_overlay, SIMPLIFY=FALSE)
    writeImages(overlay, outputDir, "fullFrame", cName, filenames)
  }
  
  # All dyes combined of there are enough channels
  if (length(names(labeled)) > 2) {  
    dir.create(paste0(outputDir, "/fullFrame/all"))
    for (cName in names(labeled)[names(labeled) != "phase"]) {
      channel <- labeled[[cName]]
      full_overlay <- mapply(overlayColor, cName, phase, channel, full_overlay, SIMPLIFY=FALSE)
    }
    writeImages(full_overlay, outputDir, "fullFrame", "all", filenames)
  }
  
  rm(full_overlay)
  
  
  
  for (id in names(output$timeseries)) {
    
    dir.create(paste0(outputDir,"/",id))
    
    ####################################################
    ############## ONLY PHASE
    ####################################################
    
    # Crop and color phase images
    cropped_phase <- cropImageByID(id, output, phase, labeled$phase)
#     colored_phase <- mapply(overlayColor, "phase", cropped_phase$bg, cropped_phase$label, SIMPLIFY=FALSE)
    color_phase <- mapply(overlayOutlines, cropped_phase$bg, cropped_phase$label, col="yellow", SIMPLIFY=FALSE)

    writeImages(colored_phase, outputDir, id, "phase", filenames)
#     
#     
#     ####################################################
#     ############## SINGLE DYE + PHASE
#     ####################################################
#     
#     for (dye in names(dyes.labeled)) {
#       dyes <- dyes.labeled[dye][[1]]
#       
#       # Crop and color phase images
#       cropped_dye <- cropImageByID(id, output, phase, dyes)
#       colored_dye <- mapply(overlayColor, dye, cropped_dye$bg, cropped_dye$label, colored_phase, SIMPLIFY=FALSE)
#       
#       # Write images 
#       for (i in 1:length(colored_dye)) {
#         EBImage::writeImage(colored_dye[[i]], file=paste0(outputDir, "/", id, "/", dye, "/", filenames[[i]], ".jpg"))
#       }
#       
#     
#       
#     }
# 
#     ####################################################
#     ############## ALL DYE + PHASE
#     ####################################################
#     
#     # Write MULTIPLE dye images
#     if (length(names(dyeOverlap)) > 0) {
#       # This mergewith list is added to each iteration with new dyes
#       mergeWith <- colored_phase
#       for (dye in names(dyes.labeled)) {
# 
#         dyes <- dyes.labeled[dye][[1]]
#         
#         # Crop and color phase images
#         cropped_dye <- cropImageByID(id, output, phase, dyes)
#         mergeWith <- mapply(overlayColor, dye, cropped_dye$bg, cropped_dye$label, mergeWith, SIMPLIFY=FALSE) 
#       }
#       for (i in 1:length(mergeWith)) {
#         EBImage::writeImage(mergeWith[[i]], file=paste0(outputDir, "/", id, "/all/", filenames[[i]], ".jpg"))
#       }
#       
    
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
writeExcel <- function(df, outputDir, channel, filenames) {
  
  write.csv(df, paste0(outputDir, "/", channel, "_noLinks.csv"))
  
  # Creates hyperlinks to specific images
  cellHyperlinks <- function(id) {
    oDir <- paste0(id, "/", channel)
    cols <- df[id]
    for (i in 1:length(cols[[1]])) {
      filename <- paste0(oDir, "/t_", filenames[[i]], ".jpg")
      cols[id][[1]][[i]] <- excelHyperlink(filename, cols[id][[1]][[i]])
    }
    return(cols)
  }
  
  # Create hyperlinks on blob names
  colHyperlinks <- function(id) {
    oDir <- paste0(id, "/", color)
    return(excelHyperlink(paste0(id, "/g_", channel),id))
  }
  
  # Create hyperlinks for full frames at time points
  timeHyperlinks <- function(time) {
    link <- paste0("fullFrame/", channel, "/", time,".jpg")
    return(excelHyperlink(link,time))
  }
  
  df <- data.frame(lapply(names(df), cellHyperlinks))
  
  names(df) <- lapply(names(df), colHyperlinks)

  rownames(df) <- lapply(filenames, timeHyperlinks)
  
  write.csv(df, paste0(outputDir, "/", channel, ".csv"))
  
 
  
}



writeImages <- function(images, outputDir, id, channel, filenames) {
  dir.create(paste0(outputDir, "/", id, "/", channel))
  for (i in 1:length(images)) {
    file <- paste0(outputDir, "/", id, "/", channel, "/t_", filenames[[i]], ".jpg")
    EBImage::writeImage(images[[i]], file=file)
  }
  createGif(paste0(outputDir, "/", id, "/", channel, "/"), paste0("g_",id,".gif"))
}