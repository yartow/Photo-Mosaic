create_photomosaic_GPT <- function(target_image_path, small_images, output_file, 
                                   target_width = 800, tile_size = 32,
                                   intermediate_results_file = "intermediate_results.csv",
                                   force_square = FALSE, intermediate_save_interval = 100) {
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
  
  # Initialize a blank canvas for the mosaic
  mosaic <- image_blank(target_width, target_height, color = "white")
  
  # Initialize a data frame to store the tile information
  tile_info <- data.frame(x = integer(), y = integer(), image_path = character(), stringsAsFactors = FALSE)
  
  # Variables to track last processed tile
  last_processed_x <- 0
  last_processed_y <- 0
  
  # Check if intermediate results file exists and load it if it does
  if (file.exists(intermediate_results_file)) {
    tile_info <- read_csv(intermediate_results_file, show_col_types = FALSE)
    processed_tiles <- paste(tile_info$x, tile_info$y, sep = "_")
    
    if (nrow(tile_info) > 0) {
      last_processed_x <- max(tile_info$x)
      last_processed_y <- max(tile_info$y[tile_info$x == last_processed_x])
    }
    
    last_saved_mosaic <- paste0(output_file, "_last_saved.png")
    if (file.exists(last_saved_mosaic)) {
      mosaic <- image_read(last_saved_mosaic)
    }
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
  
  # Counter for saving intermediate results
  tile_counter <- 0
  
  # Loop through the target image in tile-sized chunks
  for (y in seq(1, target_height, by = tile_size)) {
    for (x in seq(1, target_width, by = tile_size)) {
      # Skip tiles already processed
      if (x < last_processed_x || (x == last_processed_x && y <= last_processed_y)) {
        next
      }
      
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
      
      # Increment tile counter
      tile_counter <- tile_counter + 1
      
      # Save the intermediate mosaic to disk periodically
      if (tile_counter %% intermediate_save_interval == 0) {
        intermediate_output_file <- paste0(output_file, "_last_saved.png")
        image_write(mosaic, intermediate_output_file)
      }
    }
  }
  
  # Save the final mosaic to a file
  image_write(mosaic, output_file)
  
  # Save the tile information to a CSV file
  write_csv(tile_info, sub("\\.png$", "_tile_info.csv", output_file))
  
  return(list(mosaic = mosaic, tile_info = tile_info))
}
