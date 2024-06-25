# mosaic_script.R
library(magick)
library(imager)
library(jpeg)
library(progress)
library(OpenImageR)
library(dplyr)
library(readr)
library(tools)

# Read the command-line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("Please provide the path to the parameters file.")
}

params_file <- args[1]

# Source the parameters file
source(params_file)

# Ensure the paths are correct
if (!exists("image_folder") || !exists("file_path") || !exists("tile_size") || !exists("target_width")) {
  stop("Please provide image_folder, file_path, tile_size, and target_width in the parameters file.")
}

# Full file path
file_name_with_ext <- basename(file_path)
file_name <- tools::file_path_sans_ext(file_name_with_ext)

output_folder <- "Output"
data_folder <- "Data"

output_file <- paste0(output_folder, "/", file_name, "_", target_width, ".png")
intermediate_results <- paste0(data_folder, "/intermediate_results_", file_name, "_", target_width, ".csv")

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
  # TODO this forcing should occur in the cropping
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
  tile_info <- data.frame(x = integer(), y = integer(), 
                          image_path = character(), stringsAsFactors = FALSE)
  
  # Check if intermediate results file exists and load it if it does
  if (file.exists(intermediate_results_file)) {
    tile_info <- read_csv(intermediate_results_file, show_col_types = FALSE)
    processed_tiles <- paste(tile_info$x, tile_info$y, sep = "_")
  } else {
    processed_tiles <- character()
  }
  
  # Initialize a progress bar
  total_tiles <- (target_width / tile_size) * (target_height / tile_size)
  pb <- progress_bar$new(
    format = "  Creating photomosaic [:bar] :percent :elapsed/:eta",
    total = total_tiles,
    width = 60
  )
  
  # Loop through the target image in tile-sized chunks
  for (y in seq(1, target_height, by = tile_size)) {
    for (x in seq(1, target_width, by = tile_size)) {
      tile_key <- paste(x-1, y-1, sep = "_")
      if (tile_key %in% processed_tiles) {
        next
      }
      
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
      
      # Save intermediate results
      write_csv(tile_info, intermediate_results_file)
      
      # Update the progress bar
      pb$tick()
    }
  }
  
  # Save the mosaic to a file
  image_write(mosaic, output_file)
  
  # Save the tile information to a CSV file
  write_csv(tile_info, sub("\\.png$", "_tile_info.csv", output_file))
  
  return(list(mosaic = mosaic, tile_info = tile_info))
}

# Function to load preprocessed images and average colors
load_preprocessed_images <- function(image_data_csv) {
  image_data <- read_csv(image_data_csv, show_col_types = FALSE)
  
  # Create a list of images and their average colors
  small_images <- lapply(1:nrow(image_data), function(i) {
    img <- image_read(image_data$path[i])
    list(image = img, color = c(image_data$avg_r[i], image_data$avg_g[i], image_data$avg_b[i]), path = image_data$path[i])
  })
  
  return(small_images)
}

# Function to preprocess images
preprocess_images_3 <- function(image_folder, tile_size, thumbnail_folder, output_csv) {

  image_files <- list.files(image_folder, 
                            pattern = "\\.(jpg|jpeg|png|bmp|tiff)$", 
                            full.names = TRUE)
  
  image_data <- data.frame(path = character(), avg_r = numeric(), 
                           avg_g = numeric(), avg_b = numeric(), 
                           stringsAsFactors = FALSE)
  
  for (img_path in image_files) {
    
    # read the image
    img <- image_read(img_path)
    
    # # Crop to square if required
    # # TODO this forcing should occur in the cropping
    
    
    keep_aspect_ratio <- TRUE
    # To keep the aspect ratio when creating square thumbnails we crop the image
    # based on its shortest side
    if (keep_aspect_ratio) {
      
      img_resized 
      
      if (target_width > target_height){
        
        image_scale(img, paste0(tile_size, "x"))        
        
      } else {
        
        image_scale(img, paste0("x", tile_size))
        
      }

      # get the shortest side
      decrease_factor <- (target_height / tile_size)
      
      # rescale the image 
      img_resized <- image_scale(img, paste0(tile_size, "x", tile_size, "!"))
      
      crop_size <- floor(min(target_width, target_height) / tile_size) * tile_size
      
      target_image <- image_crop(target_image, 
                                 paste0(target_height / decrease_factor,
                                  "x", target_width / decrease_factor, "+0+0"))
      img <- image_crop(img, paste0(crop_size, "x", crop_size, "+0+0"))

      # Update dimensions after cropping
      target_info <- image_info(target_image)
      target_width <- target_info$width
      target_height <- target_info$height
    }
    
    # rescale the image 
    img_resized <- image_scale(img, paste0(tile_size, "x", tile_size, "!"))
    
    # get the average color
    img_avg_color <- average_color(img_resized)
    
    # create a new row in memory with the average image colors for each image
    image_data <- image_data %>% add_row(path = img_path, 
                                         avg_r = img_avg_color[1], 
                                         avg_g = img_avg_color[2], 
                                         avg_b = img_avg_color[3])
    
    # write the image to the thumbnail folder
    image_write(img_resized, file.path(thumbnail_folder, basename(img_path)))
    
  }
  
  # write the image data to the CSV file
  write_csv(image_data, output_csv)
  
}

# Main script execution
image_folder <- image_folder
tile_size <- tile_size
output_folder <- "Thumbnails"
output_csv <- paste0("Data", "/image_data.csv")

# Check if preprocessed data exists
if (!file.exists(output_csv) || force_process) {
  preprocess_images(image_folder, tile_size, output_folder, output_csv)
}

# Load preprocessed images
small_images <- load_preprocessed_images(output_csv)

# Create the photomosaic
result <- create_photomosaic_GPT(file_path, small_images, output_file, 
                                 target_width = target_width, tile_size = tile_size,
                                 intermediate_results_file = intermediate_results,
                                 force_square = FALSE)

print("Photomosaic created successfully.")
