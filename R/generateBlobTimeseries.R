#' @export
#' @title Searches an image for dark cell colonies and incrementally labels each colony.
#' @param images a list of labeled images See \link{flow_labelPhase}.
#' @param minTimespan remove blobs from output which aren't found in at least
#' n sequential images
#' @param maxDistance the cutoff for distance between two blobs
#' @return A \code{list} with elements \code{timeseries}, a dataframe of blob IDs 
#' and blob sizes at each timestep (in pixels) and \code{centroids}, a \code{list}
#' of dataimages with centroids, blob ID's and original integer labels for mapping
#' output column names back to the original images

generateBlobTimeseries <- function(images, minTimespan=8, maxDistance=50) {
    
  # NOTE:  Include a timestep column so that we can sort if we have to.
  # NOTE:  The documentation for merge.data.frame() says that for 'sort=FALSE'
  # NOTE:  rows are returned "in an inspecified order".
  
  # Get centroids for first frame (assuming empty background frame is in images[[1]])
  centroidsBefore <- getCentroids(images[[1]])
  
  # Initialize return timeseries dataframe with sizes from the first timestep
  sizeMatrix <- matrix(c(1,centroidsBefore$size),nrow=1)
  colnames(sizeMatrix) <- c('timestep',centroidsBefore$id)
  DF <- as.data.frame(sizeMatrix,stringsAsFactors=FALSE)
  
  # Initialize list of centroid data.frame. We'll also be returning this so we can map
  # the timeseries back to the original images
  centroids <- vector("list", length(images))
  centroids[[1]] <- centroidsBefore
  
  if (getRunOptions('verbose')) cat('\tTracking 1 ...\n')
  
  # Now track blobs between each pair of images
  for (i in 2:length(images)) {
    
    if (getRunOptions('verbose')) cat(paste0('\tTracking ',i,' ...\n'))
    
    centroidsAfter <- getCentroids(images[[i]])
    
    # Find groups that are determined to be the same between the two images
    groups <- findSimilarGroups(centroidsBefore,centroidsAfter,maxDistance)
    
    # For those continued groups, give them the ID's from the previous frame
    centroidsAfter <- updateCentroidIDs(centroidsAfter, groups)
    
    # Create a new row as a dataframe
    sizeMatrix <- matrix(c(i,centroidsAfter$size),nrow=1)
    colnames(sizeMatrix) <- c('timestep',centroidsAfter$id)
    newRowDF <- as.data.frame(sizeMatrix,stringsAsFactors=FALSE)
    
    # Append the new row (dataframe), retaining all columns and rows, inserting NA where necessary
    DF <- merge(DF,newRowDF,all=TRUE,sort=FALSE)
    
    # Save centroids
    centroids[[i]] <- centroidsAfter
    
    # Reassign "before" centroids to the current frame
    centroidsBefore <- centroidsAfter
    
  }
  
  profilePoint('generateBlobTimeseries','seconds to track blobs')   
  
  # Sanity check -- sort rows just in case they got messed up somehow
  rownames(DF) <- DF$timestep
  DF <- DF[sort(DF$timestep),]

  # Now remove the 'timestep' column
  DF <- DF[,-1]
  
  # Apply minimum timespan, only saving groups that are identified for at least n images
  allBlobCount <- ncol(DF) - 1 # omit 'timespan'
  DF <- DF[,apply(DF, 2, function(x) sum(!is.na(x)) >= minTimespan)]  
  goodBlobCount <- ncol(DF) - 1 # omit 'timespan'
  
  if (getRunOptions('verbose')) cat(paste0('\tRetaining',goodBlobCount,' of ',allBlobCount,' blobs found in at least ',minTimespan,' frames\n'))
  
  # Sort DF by linear growth slope
  sorted <- apply(log(DF), 2, function(x) { 
    x <- x[!is.na(x)]
    y <- 1:length(x)
    m <- lm(x ~ y)$coef[[1]]
    return(m)
  })
  sorted <- sort(sorted,TRUE)
  DF <- DF[,names(sorted)]
  
  profilePoint('generateBlobTimeseries','seconds to build timeseries')   
  
  # ----- Add human column names ----------------------------------------------

  # TODO:  We should be able to avoid loading the names
  data(nameList, envir=environment())
  
  # Find all IDs that are in use
  ids <- unique(unlist(lapply(centroids, function(x) as.character(x$id))))
  
  # Make a dictionary mapping old ids to names
  if (length(ids) > length(nameList)) {
    tempNames <- nameList
    times <- ceiling(length(ids) / length(nameList))
    for (ii in 1:times) {
      nameList <- c(nameList, paste0(tempNames, "_", ii))
    }
  } 
  newNames <- nameList[sample(1:length(nameList),length(ids))]
  names(newNames) <- ids
  
  # Apply new names to centroids
  for (ii in 1:length(centroids)) {
    cen <- centroids[[ii]]
    ids <- as.character(cen$id)
    centroids[[ii]]$id <- unlist(newNames[ids])
  }
  
  # Apply new names to timeseries dataframe
  colnames(DF) <- newNames[colnames(DF)]
  
  # ----- return --------------------------------------------------------------
  
  return(list(
    timeseries = DF,
    centroids = centroids
  ))
  
}




###############################################################################

# Find the best fit between two images of centroids based on 
# distance and size. Returns a best guess of which blobs became which
findSimilarGroups <- function(c1, c2, maxDistance) {
  
  # Initialize dataframe with each combination of the two indices
  ###df <- expand.grid(index1=seq(1,dim(c1)[1]), index2=seq(1,dim(c2)[1]))
  df <- expand.grid(index1=1:nrow(c1), index2=1:nrow(c2))
  df$id1 <- c1$id[df$index1]
  df$id2 <- c2$id[df$index2]
  
  # Get coordinate arrays for all combinations
  x1 <- c1$x[df$index1]
  y1 <- c1$y[df$index1]
  x2 <- c2$x[df$index2]
  y2 <- c2$y[df$index2]
  
  # Calculate the distances
  df$dist <- sqrt( (x1-x2)^2 + (y1-y2)^2 )
  
  size1 <- c1$size[df$index1]
  size2 <- c2$size[df$index2]
  
  # Calculate the growth
  df$growthPer <- size2 / size1
  df$growthRel <- unlist(lapply(df$growthPer, function(x) if(x > 1) { return(1/x)  } else { return(x) }))
  
  # Calculate "score" of group fit
  df$score <- (0.5^(df$dist/20)) * (df$growthRel^0.2)
  
  # Remove undefined scores
  df <- df[!is.nan(df$score),]
  
  # TODO:  Put hardcoded 'growthPer' threshold parameters in the configuration options
  
  # Remove unrealistic growth
  df <- df[df$growthPer < 2.5 & df$growthPer > 0.5,]
  
  # Remove unrealistic distance travelled
  df <- df[df$dist < maxDistance,]
  
  # Sort scores
  df <- df[order(-df$score),]
  
  # Remove duplicated ids. This removes weaker connections
  df <- df[!duplicated(df$index1),]
  df <- df[!duplicated(df$index2),]
  
  return(df)
  
}


###############################################################################

# Updates a centroid dataframe with IDs that are determined to be
# the same from the previous frame
updateCentroidIDs <- function(c1, g) {
  
  # select columns that continue between frame1 & 2
  # NOTE:  Conversion to character should not be needed if we always create dataframes with stringsAsFactors=FALSE
  idList <- as.character(c1$id)
  id1 <- as.character(g$id1)
  id2 <- as.character(g$id2)
  c1$id <- as.character(lapply(idList, function(x) if(x %in% id2) id1[which(id2==x)] else x))
  
  return(c1)
  
}
