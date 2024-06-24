

# Function to compute pHash for an image
compute_phash <- function(img_path) {
  
  require(OpenImageR)
  # img_path <- image_files[1]
  # img_path_small <- "/Users/andrewyong/Documents/GitHub/Mosaic/Thumbnails/1.jpg"
  # img_path_large <- "/Users/andrewyong/Documents/GitHub/Mosaic/Images/PXL_20210728_173318420.MP.jpg"
  
  # img <- load.image(img_path)
  img <- image_read(img_path)
  
  # Convert to grayscale
  img_gray <- image_convert(img, type = 'grayscale')
  img_data_gray <- image_data(img_gray)
  
  # Check dimensions of img_data_gray
  if (length(dim(img_data_gray)) == 3 && all(dim(img_data_gray) > 0)) {
    
    # Reshape to 2D matrix (assuming single-channel grayscale)
    img_matrix <- array(img_data_gray, dim = c(dim(img_data_gray)[2], 
                                               dim(img_data_gray)[3]))
    
    # Compute perceptual hash (phash)
    phash_value <- as.character(phash(img_matrix))
    return(phash_value)
    
  } else {
    
    stop("Error: Image data is empty or not in the correct format.")
    
  }
}


# Function to find similar images and remove them
remove_similar_images <- function(source_folder, similarity_threshold = 5,
                                  move_file = TRUE, 
                                  dump_folder = "NearDuplicates") {
  
  require(stringdist)
  
  # Ensure the output folder exists
  dump_folder <- paste0(source_folder, "/", dump_folder)
  if (!dir.exists(dump_folder)){
    dir.create(dump_folder, recursive = TRUE)
  }
  
  # List all image files in the folder
  image_files <- list.files(source_folder, full.names = TRUE, 
                            pattern = "\\.(jpg|jpeg|png|gif)$")
  
  # Initialize a list to store unique images
  unique_images <- list()
  phash_values <- character()
  
  # Compute pHash for each image and compare
  for (img_path in image_files) {
    
    # compute the perceptual has of this image
    phash_value <- compute_phash(img_path)
    
    # Check similarity with existing images
    similar <- FALSE
    for (existing_ph in phash_values) {
      
      # compare if the phash of this images is similar to those of others
      dist <- stringdist(phash_value, existing_ph, method = "hamming")
      # if (hamming(phash, existing_ph) <= similarity_threshold) {
      if (dist <= similarity_threshold) {
        similar <- TRUE
        break
      }
    }
    
    # If not similar, add to unique images and store phash
    if (!similar) {
      
      # the images are not similar
      unique_images[[length(unique_images) + 1]] <- img_path
      phash_values <- c(phash_values, phash_value)
      
    } else {
      
      # the images are similar, move or remove them
      
      if(move_file){
        
        # Move the file to the new file path
        file.rename(img_path, paste0(dump_folder, "/", basename(img_path)))
        
        # notify the user
        cat("Moved similar image:", img_path, " to ", dump_folder, "\n")
        
      } else {
        
        # Remove the file
        file.remove(img_path)
        
      }
    }
  }
  
  return(unique_images)
}

# Example usage:
dump_folder <- "NearDuplicates"
similarity_threshold <- 10  # Adjust as needed

# Remove similar images from the source folder
unique_images <- remove_similar_images(source_folder = image_folder, 
                                       similarity_threshold,
                                       move_file = TRUE, 
                                       dump_folder)

# # unique_images now contains paths to non-similar images
# print(unique_images)

