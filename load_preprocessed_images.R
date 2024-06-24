
# Load preprocessed image data
load_preprocessed_images <- function(image_data_csv) {
  
  require(magick)
  require(dplyr)
  require(readr)
  
  image_data <- read_csv(image_data_csv, show_col_types = FALSE)
  
  # Create a list of images and their average colors
  small_images <- lapply(1:nrow(image_data), function(i) {
    img <- image_read(image_data$path[i])
    list(image = img, color = c(image_data$avg_r[i], image_data$avg_g[i], 
                                image_data$avg_b[i]), path = image_data$path[i])
  })
  
  return(small_images)
}

