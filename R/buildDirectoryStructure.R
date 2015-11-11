#' @export
#' @title Builds A Directory of Image With Excel UI
#' @param output a List with timeseries and centroids from
#' \link{generateBlobTimeseries}
#' @param phase a list of phase contrast image matrices
#' @param labeled a list of labeled images from each channel
#' \link{flow_labelPhase}
#' @param dyeOverlap a \code{dataframe} of dye overlap values from
#' \link{findDyeOverlap}
#' @param filenames a vector of names equal to the length of the 
#' image lists. Usually timesteps.
#' @param outputDir the directory to build the file structure
#' @param distanceScale the distance conversion in um/pixel
#' @param chamber the current chamber id
#' @description write a lot more here TODO
#' @return none

buildDirectoryStructure <- function(output, phase, labeled, dyeOverlap, filenames, 
                                    outputDir="output", distanceScale=NULL, chamber="") { 
  
###  directoryTime <- proc.time()
  if (getRunOptions('verbose')) cat("\tBuilding directory structure ...\n")
  
  #################################################################
  #################################################################
  
###  ptm <- proc.time()
  
  # ----- Create excel files --------------------------------------------------
  
  # phase channel
  writeExcel(output$timeseries, outputDir, "phase", filenames, chamber=chamber)
  
  # All dye channels
  for (name in names(dyeOverlap)) {
    writeExcel(dyeOverlap[[name]], outputDir, name, filenames, chamber=chamber)
  }
  
  
  # ----- Create full frame images --------------------------------------------
  
  if (getRunOptions('verbose')) cat('\tCreating full frames ...\n')
  
  # Add overlays to phase
  # These overlays will also serve as a background to other channels
  
  full_overlay <- mapply(overlayOutlines, phase, labeled$phase, col="yellow", SIMPLIFY=FALSE)
  ### full_overlay <- lapply(full_overlay, overlayScaleBar, distanceScale)
  writeImages(images=full_overlay, outputDir=outputDir, id="fullFrame", channel="phase", filenames)
  
  profilePoint('saveImages','seconds to save outlined phase images')
  
  # Write non phase channels
  for (cName in names(labeled)[names(labeled) != "phase"]) {
    if (getRunOptions('verbose')) cat(paste0('\tWriting ',cName,' ...\n'))
    channel <- labeled[[cName]]
    overlay <- mapply(overlayColor, cName, phase, channel, full_overlay, SIMPLIFY=FALSE)
    writeImages(overlay, outputDir, id="fullFrame", cName, filenames)
    profilePoint('saveImages',paste('seconds to save outlined',cName,'images'))
  }
  
  # All dyes combined if there are enough channels
  if (length(names(labeled)) > 2) {
    ptm <- proc.time()
    cat("\nallDyes ")
    dir.create(paste0(outputDir, "/fullFrame/all"), showWarnings=FALSE, recursive=TRUE)
    for (cName in names(labeled)[names(labeled) != "phase"]) {
      channel <- labeled[[cName]]
      full_overlay <- mapply(overlayColor, cName, phase, channel, full_overlay, SIMPLIFY=FALSE)
    }
    writeImages(full_overlay, outputDir, "fullFrame", "all", filenames)
###    cat(formatTime(ptm))
  }
  
  rm(full_overlay)
    

# ----- Create individual images ----------------------------------------------

###  ptm <- proc.time()
  if (getRunOptions('verbose')) cat("\tSaving images for individual ids ...\n")
  
  dir.create(paste0(outputDir, "/individual"), showWarnings=FALSE, recursive=TRUE)
  
  counter <- 0
  coloniesCount <- length(names(output$timeseries))
  for (id in names(output$timeseries)) {
    
    counter <- counter + 1
    if (getRunOptions('verbose')) {
      pct <- round(1000*(counter/coloniesCount))
      if ( (pct %% 100) == 0 ) cat(paste0('\t',pct/10,'%\n'))
    }
    
    dir.create(paste0(outputDir,"/individual/",id), showWarnings=FALSE, recursive=TRUE)
    
    sizes <- numeric(length(filenames))
    for (ii in 1:length(output$centroids)) {
      centroids <- output$centroids[[ii]]
      mask <- id==centroids$id
      if (sum(mask) > 0) {
        sizes[ii] <- centroids[mask,]$size
      } else {
        sizes[ii] <- 0
      }
    }
    
    ####################################################
    ############## ONLY PHASE
    ####################################################
    
    # Crop and color phase images
    cropped_phase <- cropImageByID(id, output, phase, labeled[[1]])
#     colored_phase <- mapply(overlayColor, "phase", cropped_phase$bg, cropped_phase$label, SIMPLIFY=FALSE)
    color_phase <- mapply(overlayOutlines, cropped_phase$bg, cropped_phase$labelSingle, col="yellow", thick=FALSE, SIMPLIFY=FALSE)
    color_phase <- lapply(color_phase, overlayScaleBar, distanceScale)
    color_phase <- mapply(overlayVitalStats, color_phase, id, filenames, sizes, distanceScale, SIMPLIFY=FALSE)
    color_phase <- mapply(overlayNeighborsIDs, color_phase, cropped_phase$labelFull, id, output$centroids, SIMPLIFY=FALSE)
    
    writeImages(color_phase, paste0(outputDir,"/individual"), id, "phase", filenames)
    
    # Write non phase channels
    for (cName in names(labeled)[-1]) {
      channel <- labeled[[cName]]
      cropped_dye <- cropImageByID(id, output, phase, channel)$labelFull
      color_dye <- mapply(overlayOutlines, color_phase, cropped_dye, col=cName, thick=FALSE, SIMPLIFY=FALSE)
      writeImages(color_dye, paste0(outputDir,"/individual"), id, cName, filenames)
    }
    
  }
  
  if (getRunOptions('verbose')) cat("\n")
  
  profilePoint('saveImages',paste('seconds to save individual images'))
  
###  cat(paste0("\tFull directory built in ", formatTime(directoryTime),'\n'))
  
}


###############################################################################

writeImages <- function(images, outputDir, id, channel, filenames) {
  
  # Create the directory
  dir <- paste0(outputDir, "/", id, "/", channel)
  result <- try( dir.create(dir, showWarnings=FALSE, recursive=TRUE) )
  
  if ( class(result)[1] == "try-error" ) {
    err_msg <- geterrmessage()
    cat(paste0('\nWARNING:  Could not create directory "',dir,'"\n',err_msg,'\n'))   
  }
  
  # Create individual timestep .jpg files
  for (i in 1:length(images)) {
    
    file <- paste0(outputDir, "/", id, "/", channel, "/t_", filenames[[i]], ".jpg")
    result <- try( EBImage::writeImage(images[[i]], file=file) )
    
    if ( class(result)[1] == "try-error" ) {
      err_msg <- geterrmessage()
      cat(paste0('\nWARNING:  Could not write "',file,'"/\n',err_msg,'\n'))   
    }
    
  }
  
  # Create animated .gif image
  dir <- paste0(outputDir, "/", id, "/", channel)
  filename <- paste0("g_",id,".gif")
  result <- try( createGif(dir,filename) )
  
  if ( class(result)[1] == "try-error" ) {
    err_msg <- geterrmessage()
    cat(paste0('\nWARNING:  Could not write "',paste(dir,filename,sep='/'),'"/\n',err_msg,'\n'))   
  }
  
}

