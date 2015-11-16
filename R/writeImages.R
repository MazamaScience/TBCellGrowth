#' @export
#' @title Creates Images for Excel Hyperlinks
#' @param images list of images
#' @param outputDir the directory to build the file structure
#' @param id cell colony name (column headers in the Excel file)
#' @param channel channel name e.g. "phase","green","red"
#' @param filenames a vector of names equal to the length of the 
#' image lists. Usually timesteps.
#' @description Creates a subdirectory based on \code{outputDir},
#' \code{id} and \code{channel} and then populates that directory
#' with the .jpg versions of the \code{images}, each labeled
#' according to the associated \code{filenames}.
#' 
#' Hyperlinks for these images are generated in writeExcel().
#' @return none

writeImages <- function(images, outputDir, id, channel, filenames) {
  
  # Sanity check
  if ( length(images) != length(filenames) ) {
    cat(paste0('\tWARNING:  found ',length(images),' images and ',length(filenames),' filenames. Creating artificial filenames.\n'))
    filenames <- seq(length(images))
  }
    
  # Create the directory
  dir <- paste0(outputDir, "/", id, "/", channel)
  result <- try( dir.create(dir, showWarnings=FALSE, recursive=TRUE) )
  
  if ( class(result)[1] == "try-error" ) {
    err_msg <- geterrmessage()
    cat(paste0('\nWARNING:  Could not create directory "',dir,'"\n',err_msg,'\n'))   
  }
  
  # Create individual timestep .jpg files
  for (i in 1:length(images)) {
    
    file <- paste0(outputDir, "/", id, "/", channel, "/t_", filenames[i], ".jpg")
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

