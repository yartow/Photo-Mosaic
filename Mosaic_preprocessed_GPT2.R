# Load the necessary packages
library(magick)
library(imager)
library(jpeg)
library(progress)
library(OpenImageR)
library(dplyr)
library(readr)

# Function to compute average color of an image
average_color <- function(img) {
  img_resized <- image_resize(img, "1x1")
  avg_color <- as.numeric(image_data(img_resized))
  return(avg_color)
}

# Function to create a photomosaic and save tile information
create_photomosaic_GPT <- function(target_image_path, small_images, output_file, 
                                   target_width = 800, tile_size = 32,
                                   intermediate_results_file = "intermediate_results.csv",
                                   force_square = FALSE) {
  
  # Read the target image
  target_image <- image_read(target_image_path)
  
  # Scale target image to desired width while maintaining the aspect ratio
  target_image <- image_scale(target_image, paste0(target_width, "x"))
  
  # Get the dimensions of the scaled target image
  target_info <- image_info(target_image)
  target_width <- target_info$width
  target_height <- target_info$height
  
  # Crop to square if required
  if (force_square) {
    crop_size <- floor(min(target_width, target_height) / tile_size) * tile_size
    target_image <- image_crop(target_image, paste0(crop_size, "x", crop_size, "+0+0"))
    
    # Update dimensions after cropping
    target_info <- image_info(target_image)
    target_width <- target_info$width
    target_height <- target_info$height
  }
  
  # Create a blank canvas for the mosaic
  mosaic <- image_blank(target_width, target_height, color = "white")
  
  # Initialize a data frame to store the tile information
  tile_info <- data.frame(x = integer(), y = integer(), image_path = character(), stringsAsFactors = FALSE)
  
  # Check if intermediate results file exists and load it if it does
  if (file.exists(intermediate_results_file)) {
    tile_info <- read_csv(intermediate_results_file, show_col_types = FALSE)
    processed_tiles <- paste(tile_info$x, tile_info$y, sep = "_")
  } else {
    processed_tiles <- character()
  }
  
  # Initialize a progress bar
  # total_tiles <- (target_width / tile_size) * (target_height / tile_size)
  # pb <- progress_bar$new(
  #   format = "  Creating photomosaic [:bar] :percent :elapsed/:eta",
  #   total = total_tiles,
  #   width = 60
  # )
  
  # Loop through the target image in tile-sized chunks
  for (y in seq(1, target_height, by = tile_size)) {
    for (x in seq(1, target_width, by = tile_size)) {
      tile_key <- paste(x-1, y-1, sep = "_")
      # if (tile_key %in% processed_tiles) {
      #   next
      # }
      
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
      
      # Diagnostic: Print information about the best match
      # cat("Placing tile at (", x-1, ", ", y-1, ") with image:", best_image_path, "\n")
    
      # Composite the best match onto the mosaic
      mosaic <- image_composite(mosaic, best_match, offset = paste0("+", x-1, "+", y-1))
      
      # Save the tile information
      tile_info <- tile_info %>% add_row(x = x-1, y = y-1, image_path = best_image_path)
      
      # Save intermediate results
      write_csv(tile_info, intermediate_results_file)
      
      # Update the progress bar
      # pb$tick()
    }
  }
  
  # Check the dimensions and contents of the mosaic
  # print(image_info(mosaic))
  # mosaic_preview <- image_resize(mosaic, "400x400")
  # print(mosaic_preview)
  
  # Save the mosaic to a file
  image_write(mosaic, output_file)
  
  # Save the tile information to a CSV file
  write_csv(tile_info, sub("\\.png$", "_tile_info.csv", output_file))
  
  return(list(mosaic = mosaic, tile_info = tile_info))
}


data_folder <- paste0(getwd(), "/Data")
intermediate_results <- paste0(data_folder, "/intermediate_results.csv")
output_csv <- paste0(data_folder, "/image_data.csv")
target_image_path <- paste0(data_folder, "/Family.jpg")
output_folder <- "/Users/andrewyong/Documents/GitHub/Mosaic/Output"
output_file <- paste0(output_folder, "/Margaret_mosaic.png")
thumbnail_folder <- "/Users/andrewyong/Documents/GitHub/Mosaic/Thumbnails"

# Run the preprocessing
image_folder <- paste0(getwd(), "/Images")
tile_size <- 64

# Load or preprocess small images
preprocessed <- FALSE
if (preprocessed) {
  small_images <- load_preprocessed_images(output_csv)
} else {
  preprocess_images(image_folder, tile_size, thumbnail_folder, output_csv)
  small_images <- load_preprocessed_images(output_csv)
}

# Create the photomosaic
result <- create_photomosaic_GPT(target_image_path, small_images, output_file, 
                                 target_width = 8192, tile_size = tile_size,
                                 intermediate_results_file = intermediate_results,
                                 force_square = FALSE)

# Display the mosaic
print(result$mosaic)

# The tile information is stored in result$tile_info
print(result$tile_info)
