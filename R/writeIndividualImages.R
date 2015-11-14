#' @export
#' @title Write Individual Images
#' @param timeseriesList a List with timeseries and centroids from
#' \link{generateBlobTimeseries}
#' @param phase a list of phase contrast image matrices
#' @param labeledImageList a list of lists of labeled images for each channel
#' \link{flow_labelPhase}
#' @param dyeOverlap a \code{dataframe} of dye overlap values from
#' \link{findDyeOverlap}
#' @param filenames a vector of names equal to the length of the 
#' image lists. Usually timesteps.
#' @param outputDir the directory to build the file structure
#' @param distanceScale the distance conversion in um/pixel
#' @param chamber the current chamber id
#' @description Creates individual directories and populates each
#' with colony thumbnail images with outlines and other labeling.
#' @return none

writeIndividualImages <- function(timeseriesList, phase, labeledImageList, dyeOverlap, filenames, 
                                  outputDir="output", distanceScale=NULL, chamber="") { 
  
  dir.create(paste0(outputDir, "/individual"), showWarnings=FALSE, recursive=TRUE)
  
  counter <- 0
  colonyCount <- length(names(timeseriesList$timeseries))
  everyNth <- round(colonyCount/20)
  
  # For each named colony
  for ( id in names(timeseriesList$timeseries) ) {
    
    # For down-in-the-weeds debugging only
    ###print(paste0('Working on "',id,'"'))
    
    counter <- counter + 1
    if (getRunOptions('verbose')) {
      if ( (counter %% everyNth) == 0 ) {
        cat(paste0('\t',counter,'/',colonyCount,' colonies\n'))
        profilePoint('saveImages',paste('seconds to save individual images'))
      }
    }
    
    dir.create(paste0(outputDir,"/individual/",id), showWarnings=FALSE, recursive=TRUE)
    
    debug_text <- ""
    result <- try({

      # Crop phase images
      # NOTE:  This functions works on the entire list so that it can choose the best bounding rectangle.
      cropped_phase <- cropImageByID(id, timeseriesList$centroids, phase, labeledImageList[[1]])
      debug_text <- paste0(debug_text,"; cropped phase")
      
      # Loop over timesteps
      color_phase <- vector("list", length(timeseriesList$centroids))
      for (i in 1:length(timeseriesList$centroids)) {
        
        centroids <- timeseriesList$centroids[[i]]
        mask <- id == centroids$id
        size <- ifelse( sum(mask) > 0, centroids[mask,]$size, 0)
        infoList <- list(distanceScale=distanceScale,
                         distanceThresholds=c(10,50,100),
                         size=size,
                         time=filenames[i],
                         centroids=centroids)
        
        # Add colony outlines (uses EBImage)
        color_phase[[i]] <- overlayOutlines(cropped_phase$bg[[i]], cropped_phase$labelSingle[[i]], color="yellow", thick=FALSE)
        
        debug_text <- paste0(debug_text,"; overlayOutlines [",i,"]")
        
        # Add text annotations (requires disk write/read)
        if ( !getRunOptions('noLabels') ) {
          color_phase[[i]] <- overlayAnnotations(color_phase[[i]], cropped_phase$labelFull[[i]], id, infoList)
          debug_text <- paste0(debug_text,"; overlayAnnotations [",i,"]")
        }    
        
      }

    }, silent=FALSE)
    
    if ( class(result)[1] == "try-error" ) {
      
      err_msg <- paste(debug_text,geterrmessage(),sep="\n")
      cat(paste0("\tWARNING:  Unable to label individual images for ",id,"\n\t",err_msg,"\n"))
      
    } else {
      
      result <- try( writeImages(color_phase, paste0(outputDir,"/individual"), id, "phase", filenames),
                     silent=FALSE )
      
      if ( class(result)[1] == "try-error" ) {
        err_msg <- geterrmessage()
        cat(paste0("\tWARNING:  Unable to write individual images for ",id,"\n\t",err_msg,"\n"))
      }
      
      # Write non phase channels
      for (cName in names(labeledImageList)[-1]) {
        channel <- labeledImageList[[cName]]
        cropped_dye <- cropImageByID(id, timeseriesList$centroids, phase, channel)$labelFull
        color_dye <- mapply(overlayOutlines, color_phase, cropped_dye, col=cName, thick=FALSE, SIMPLIFY=FALSE)
        
        result <- try( writeImages(color_dye, paste0(outputDir,"/individual"), id, cName, filenames),
                       silent=FALSE )
        
        if ( class(result)[1] == "try-error" ) {
          err_msg <- geterrmessage()
          cat(paste0("\tWARNING:  Unable to write individual images for ",id,"_",cName,"\n\t",err_msg,"\n"))
        }
      }
      
    } # END of no-error block
    
  } # END of colony loop
  
}


