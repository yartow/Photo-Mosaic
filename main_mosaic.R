#description and stuff

# source files
source(paste0(getwd(), "/", "preprocess_images.R"))
source(paste0(getwd(), "/", "load_preprocessed_images.R"))
source(paste0(getwd(), "/", "Mosaic_preprocessed.R"))


# Specify the paths to the target image and the folder containing the small images
image_folder <- "/Users/andrewyong/Documents/GitHub/Mosaic/Images"
# to remove similar images from the image folder
source(paste0(getwd(), "/", "remove_similar_images.R"))

data_folder <- "/Users/andrewyong/Documents/GitHub/Mosaic/Data"

# uncomment the following line if you want to pick up where it left off last run
intermediate_results <- paste0(data_folder, "/intermediate_results.csv")
# target_image_path <- paste0(data_folder, "/Margaret.jpg")
target_image_path <- paste0(data_folder, "/Family.jpg")
output_folder <- "/Users/andrewyong/Documents/GitHub/Mosaic/Output"
output_file <- paste0(output_folder, "/Margaret_mosaic.png")
thumbnail_folder <- "/Users/andrewyong/Documents/GitHub/Mosaic/Thumbnails"

# Run the preprocessing
image_folder <- image_folder
tile_size <- 64
output_folder <- thumbnail_folder
output_csv <- paste0(data_folder, "/image_data.csv")

# Do a check here on whether pictures for this target image have already been preprocessed 
force_process <- TRUE
preprocessed <- FALSE
if(preprocessed && !force_process){
  
  # Load preprocessed images and average colors
  small_images <- load_preprocessed_images(output_csv)
  
} else {
  
  preprocess_images(image_folder, tile_size, thumbnail_folder, output_csv)
  small_images <- load_preprocessed_images(output_csv)
}

# create the actual mosaic
# # Specify the paths to the target image and the preprocessed image data
# data_folder <- "/Users/andrewyong/Documents/GitHub/Mosaic/Data"
# target_image_path <- paste0(data_folder, "/Margaret.jpg")
# intermediate_results <- paste0(data_folder, "/intermediate_results.csv")
# output_folder <- "/Users/andrewyong/Documents/GitHub/Mosaic/Output"
# output_file <- paste0(output_folder, "/Margaret_mosaic.png")

# Create the photomosaic
result <- create_photomosaic(target_image_path, small_images, output_file, 
                             target_width = 8192, tile_size = tile_size)

# Display the mosaic
print(result$mosaic)

# The tile information is stored in result$tile_info
print(result$tile_info)

