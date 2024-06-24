# Install required packages if not already installed
if (!require("magick")) install.packages("magick")
if (!require("imager")) install.packages("imager")
if (!require("jpeg")) install.packages("jpeg")

# Load the packages
library(magick)
library(imager)
library(jpeg)

# Find a way to make small thumbnails of all images once, so next runs will be faster
# Find a way to keep the aspect ratio of the image and only *crop* this to a square

rescale_image_and_save <- function(img_path, folder_name, tile_size, fileName) {
  
  img <- image_read(img_path)
  
  # obtain info on dimensions of image
  imageInfo <- image_info(img)
  
  # scaled based on width or height, depending on which one is bigger 
  new_img <- image_scale(img, ifelse(imageInfo$width <= imageInfo$height, 
                                     paste0(tile_size, "x"), paste0("x", tile_size)))
  
  # crop the image
  new_img <- image_crop(new_img, paste0(tile_size, "x", tile_size, "+0+0"))
  
  # store images
  image_write(new_img, paste0(folder_name, "/", fileName, ".jpg"))
  
}

# Function to resize images
resize_images <- function(image_folder, folder_name = "Thumbnails",
                          tile_size){
  
  # List all image files in the folder
  image_files <- list.files(image_folder, full.names = TRUE, 
                            pattern = "\\.(jpg|jpeg|png|gif)$")
  
  # resize all images
  fileName <- 0
  for (image_file in image_files){
    
    fileName <- fileName + 1
    rescale_image_and_save(image_file, folder_name, tile_size, fileName)
    
  }
  
}

# resize images unless there already are images 
get_resized_images <- function(image_folder, folder_name = "Thumbnails", 
                               tile_size){
  
  # check if folder_name contains images 
  folder_name_full_path <- paste0(getwd(), "/", folder_name)
  if(dir.exists(folder_name_full_path)){
    
    allFiles <- list.files(folder_name_full_path, full.names = TRUE, 
                              pattern = "\\.(jpg|jpeg|png|gif)$")
    
    # check if there are 100 images at least
    if (length(allFiles) < 100){
      
      # there are no pregenerated thumbnails, generate these
      resize_images(image_folder, folder_name, tile_size)
      
    }
    
  } else {
    
    # there are no pregenerated thumbnails, generate these
    resize_images(image_folder, folder_name, tile_size)
  
  }
  
  # get the list of all images in the thumbnails folder
  allFiles <- list.files(folder_name_full_path, full.names = TRUE, 
                         pattern = "\\.(jpg|jpeg|png|gif)$")
  
  # there are enough thumbnails, get the pregenerated rescaled images
  small_images <- lapply(allFiles, image_read)
  
  # TODO I need to add the average color of each tile and path to a list
  small_images <- lapply(image_files, function(img_path) {
    img <- image_read(img_path)
    img_resized <- image_scale(img, paste0(tile_size, "x", tile_size, "!"))
    list(image = img_resized, color = average_color(img_resized), path = img_path)
  })
  
  return(small_images)
  
}
# 
# # Function to create a photomosaic
# create_photomosaic3 <- function(target_image_path, image_folder, output_file, 
#                                 tile_size = 32, output_resolution = c(640, 640)) {
#   # Read the target image
#   target_image <- image_read(target_image_path)
#   target_image <- image_scale(target_image, paste0(output_resolution[1], "x", 
#                                                    output_resolution[2], "!"))
#   
#   # Call the function to resize all the images, however, if they have been resized 
#   # before, these only need to be loaded, unless it is forced to resize everything.
#   small_images <- get_resized_images(image_folder, "Thumbnails", tile_size)
#   
#   # Get the dimensions of the target image
#   target_info <- image_info(target_image)
#   target_width <- target_info$width
#   target_height <- target_info$height
#   
#   # Create a blank canvas for the mosaic
#   mosaic <- image_blank(target_width, target_height, color = "white")
#   
#   # Loop through the target image in tile-sized chunks
#   for (y in seq(1, target_height, by = tile_size)) {
#     for (x in seq(1, target_width, by = tile_size)) {
#       
#       # Extract the region of the target image
#       region <- image_crop(target_image, paste0(tile_size, "x", tile_size, "+", x-1, "+", y-1))
#       
#       # Find the best matching small image
#       best_match <- small_images[[1]]
#       best_diff <- Inf
#       for (img in small_images) {
#         
#         # Compare using Structural Similarity Index (SSIM)
#         comparison <- image_compare(region, img, metric = "ssim")
#         diff <- attr(comparison, "distortion")
#         if (diff < best_diff) {
#           best_diff <- diff
#           best_match <- img
#         }
#       }
#       
#       # Composite the best match onto the mosaic
#       mosaic <- image_composite(mosaic, best_match, offset = paste0("+", x-1, "+", y-1))
#     }
#   }
#   
#   # Save the mosaic to a file
#   image_write(mosaic, output_file)
#   
#   return(mosaic)
# }


# Function to compute average color of an image
average_color <- function(img) {
  
  img_resized <- image_resize(img, "1x1")
  img_info <- image_info(img_resized)
  avg_color <- as.numeric(image_data(img_resized))
  
  return(avg_color)
  
}


# Function to create a photomosaic and save tile information
result <- create_photomosaic4(target_image_path, image_folder, output_file, target_resolution, tile_size)
create_photomosaic4 <- function(target_image_path, image_folder, output_file, 
                                target_resolution = "800x800", tile_size = 32) {
  # Read the target image
  target_image <- image_read(target_image_path)
  
  # Scale target image to desired resolution
  target_image <- image_scale(target_image, target_resolution)
  
  # Call the function to resize all the images, however, if they have been resized 
  # before, these only need to be loaded, unless it is forced to resize everything.
  small_images <- get_resized_images(image_folder, "Thumbnails", tile_size)
  
  # Get the dimensions of the target image
  target_info <- image_info(target_image)
  target_width <- target_info$width
  target_height <- target_info$height
  
  # Create a blank canvas for the mosaic
  mosaic <- image_blank(target_width, target_height, color = "white")
  
  # Initialize a data frame to store the tile information
  tile_info <- data.frame(x = integer(), y = integer(), image_path = character(), stringsAsFactors = FALSE)
  
  # Loop through the target image in tile-sized chunks
  for (y in seq(1, target_height, by = tile_size)) {
    for (x in seq(1, target_width, by = tile_size)) {
      # Extract the region of the target image
      region <- image_crop(target_image, paste0(tile_size, "x", tile_size, "+", x-1, "+", y-1))
      
      # Compute the average color of the region
      region_color <- average_color(region)
      
      # Find the best matching small image based on average color
      best_match <- small_images[[1]]$image
      best_diff <- Inf
      best_image_path <- small_images[[1]]$path
      for (img in small_images) {
        diff <- sum((region_color - img$color)^2)
        if (diff < best_diff) {
          best_diff <- diff
          best_match <- img$image
          best_image_path <- img$path
        }
      }
      
      # Composite the best match onto the mosaic
      mosaic <- image_composite(mosaic, best_match, offset = paste0("+", x-1, "+", y-1))
      
      # Save the tile information
      tile_info <- tile_info %>% add_row(x = x-1, y = y-1, image_path = best_image_path)
    }
  }
  
  # Save the mosaic to a file
  image_write(mosaic, output_file)
  
  # Save the tile information to a CSV file
  write.csv(tile_info, sub("\\.png$", "_tile_info.csv", output_file), row.names = FALSE)
  
  return(list(mosaic = mosaic, tile_info = tile_info))
}


# Specify the paths to the target image and the folder containing the small images
image_folder <- "/Users/andrewyong/Documents/GitHub/Mosaic/Images"
data_folder <- "/Users/andrewyong/Documents/GitHub/Mosaic/Data"
target_image_path <- paste0(data_folder, "/Margaret.jpg")
output_folder <- "/Users/andrewyong/Documents/GitHub/Mosaic/Output"
output_file <- paste0(output_folder, "/Margaret_mosaic.png")
thumbnail_folder <- "/Users/andrewyong/Documents/GitHub/Mosaic/Thumbnails"

# Parameters
# multiplier <- 1
tile_size <- 32  # bigger tile size provides more detail
# Specify desired resolution and tile size
target_resolution <- "8192x5472"

# output_resolution <- c(1024*multiplier, 684*multiplier)  # Higher resolution for the output file

# Create the photomosaic
result <- create_photomosaic4(target_image_path, image_folder, output_file, target_resolution, tile_size)

# Display the mosaic
print(result$mosaic)

# The tile information is stored in result$tile_info
print(result$tile_info)

# Create the photomosaic
mosaic_image <- create_photomosaic2(target_image_path, image_folder, output_file, target_resolution, tile_size)


 # Create the photomosaic
mosaic_image <- create_photomosaic(target_image_path, image_folder, output_file, tile_size, output_resolution)

# Create the photomosaic2
mosaic_image <- create_photomosaic2(target_image_path, image_folder, output_file)

# Display the mosaic
print(mosaic_image)
