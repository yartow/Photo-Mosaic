create_photomosaic <- function(target_image_path, small_images, output_file, target_width = 800, tile_size = 32) {
  # Read the target image
  target_image <- image_read(target_image_path)
  
  # Scale target image to desired width while maintaining the aspect ratio
  target_image <- image_scale(target_image, paste0(target_width, "x"))
  
  # Get the dimensions of the scaled target image
  target_info <- image_info(target_image)
  target_width <- target_info$width
  target_height <- target_info$height
  
  # Crop the image into a square based on the tile_size
  crop_size <- floor(min(target_width, target_height) / tile_size) * tile_size
  target_image <- image_crop(target_image, paste0(crop_size, "x", crop_size, "+0+0"))
  
  # Update target dimensions after cropping
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
  write_csv(tile_info, sub("\\.png$", "_tile_info.csv", output_file))
  
  return(list(mosaic = mosaic, tile_info = tile_info))
}

# Specify the paths to the target image and the preprocessed image data
target_image_path <- "path/to/your/target/image.jpg"
output_file <- "photomosaic_output.png"

# Create the photomosaic
result <- create_photomosaic(target_image_path, small_images, output_file, target_width = 1045, tile_size = 32)

# Display the mosaic
print(result$mosaic)

# The tile information is stored in result$tile_info
print(result$tile_info)
