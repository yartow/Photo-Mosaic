# Function to compute average color of an image
average_color <- function(img) {
  img_resized <- image_resize(img, "1x1")
  avg_color <- as.numeric(image_data(img_resized))
  return(avg_color)
}

# Preprocess images and save results
preprocess_images <- function(image_folder, tile_size, output_folder, 
                              output_csv) {
  
  require(magick)
  require(dplyr)
  require(readr)
  
  image_files <- list.files(image_folder, full.names = TRUE, 
                            pattern = "\\.(jpg|jpeg|png|gif)$")
  
  # Ensure the output folder exists
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  # Data frame to store image paths and average colors
  image_data <- data.frame(path = character(), avg_r = numeric(), 
                           avg_g = numeric(), avg_b = numeric(), 
                           stringsAsFactors = FALSE)
  
  for (img_path in image_files) {
    
    # read the image
    img <- image_read(img_path)
    
    # scaled based on width or height, depending on which one is bigger 
    new_img <- image_scale(img, ifelse(imageInfo$width <= imageInfo$height, 
                                       paste0(tile_size, "x"), 
                                       paste0("x", tile_size)))
    
    # crop the image
    img_resized <- image_crop(new_img, paste0(tile_size, "x", tile_size, "+0+0"))
    
    # Save the resized image
    resized_path <- file.path(output_folder, basename(img_path))
    image_write(img_resized, resized_path)
    
    # Compute and save average color
    avg_color <- average_color(img_resized)
    image_data <- image_data %>% add_row(path = resized_path, 
                                         avg_r = avg_color[1], 
                                         avg_g = avg_color[2], 
                                         avg_b = avg_color[3])
  }
  
  # Save the data frame to CSV
  write_csv(image_data, output_csv)
  
}
