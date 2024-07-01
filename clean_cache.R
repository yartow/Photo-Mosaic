# Get the cache directory
cache_dir <- tempdir()
cat("R's Cache Directory:", cache_dir, "\n")

# List files in the cache directory
cache_files <- list.files(cache_dir, full.names = TRUE)
cat("Files in the Cache Directory:\n", paste(cache_files, collapse = "\n"), "\n")

# Optionally, remove files in the cache directory
unlink(cache_files, recursive = TRUE)
cat("Cache directory cleared.\n")
x <- 5+5
