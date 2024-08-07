#description and stuff

# source files
source(paste0(getwd(), "/", "preprocess_images.R"))
source(paste0(getwd(), "/", "load_preprocessed_images.R"))
source(paste0(getwd(), "/", "Mosaic_preprocessed_GPT.R")) # <-- right version?


# Specify the paths to the target image and the folder containing the small images
image_folder <- paste0(getwd(), "/Images")

removeSimilarImages <- TRUE
# to remove similar images from the image folder
if (removeSimilarImages){
  
  source(paste0(getwd(), "/", "remove_similar_images.R"))
  
}

# uncomment the following line if you want to pick up where it left off last run
data_folder <- paste0(getwd(), "/Data")

output_csv <- paste0(data_folder, "/image_data.csv")
target_image_path <- paste0(data_folder, "/Margaret_portrait.jpg")
output_folder <- paste0(getwd(), "/Output")
thumbnail_folder <- paste0(getwd(), "/Thumbnails")

# Run the preprocessing
image_folder <- paste0(getwd(), "/Images")
tile_size <- 64
target_width <- 8192*2

# Extract the file name with extension
file_name_with_ext <- basename(target_image_path)

# Remove the extension to get the file name
file_name <- tools::file_path_sans_ext(file_name_with_ext)
output_file <- paste0(output_folder, "/", file_name, "_", target_width, ".png")
intermediate_results <- paste0(data_folder, "/intermediate_results_", file_name, "_", target_width, ".csv")
#   date()
force_process <- TRUE
preprocessed <- FALSE

# Load the preprocessed images unless `force_process` is TRUE
if(preprocessed && !force_process){
  
  # Load preprocessed images and average colors
  small_images <- load_preprocessed_images(output_csv)
  
} else {
  
  # TODO make it such that existing images are skipped
  # TODO there is still one error with images that are higher than they are wide, which leaves a white bar
  preprocess_images(image_folder, tile_size, thumbnail_folder, output_csv)
  small_images <- load_preprocessed_images(output_csv)
  
}

# Create the photomosaic
# TODO Check which version is latest
result <- create_photomosaic_GPT(target_image_path, small_images, output_file, 
                       target_width = target_width, tile_size = tile_size,
                         intermediate_results_file = intermediate_results,
                         force_square = FALSE)

# Display the mosaic
print(result$mosaic)
print(date())


