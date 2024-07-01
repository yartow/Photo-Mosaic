# Function to log system stats to a file on macOS
log_system_stats_to_file <- function(log_file) {
  # Open the log file for writing
  sink(log_file)
  
  # Log the current date and time
  cat("Log Date and Time:\n", Sys.time(), "\n\n")
  
  # Hard Disk Space
  cat("Disk Space:\n")
  system("df -h", intern = FALSE)
  cat("\n")
  
  # CPU Usage
  cat("CPU Usage:\n")
  system("ps -A -o %cpu | awk '{s+=$1} END {print s \"% CPU Usage\"}'", intern = FALSE)
  cat("\n")
  
  # Memory Usage
  cat("Memory Usage:\n")
  system("vm_stat | perl -ne '/page size of (\\d+)/ and $size=$1; /Pages free: (\\d+)/ and printf(\"Free memory: %.2f GB\\n\", $1*$size/1024/1024/1024); /Pages active: (\\d+)/ and printf(\"Active memory: %.2f GB\\n\", $1*$size/1024/1024/1024); /Pages inactive: (\\d+)/ and printf(\"Inactive memory: %.2f GB\\n\", $1*$size/1024/1024/1024); /Pages speculative: (\\d+)/ and printf(\"Speculative memory: %.2f GB\\n\", $1*$size/1024/1024/1024); /Pages wired down: (\\d+)/ and printf(\"Wired memory: %.2f GB\\n\", $1*$size/1024/1024/1024); /Pages purgeable: (\\d+)/ and printf(\"Purgeable memory: %.2f GB\\n\", $1*$size/1024/1024/1024);'", intern = FALSE)
  cat("\n")
  
  # Close the log file
  sink()
}

# Specify the log file path
log_file <- "system_stats_log.txt"

# Call the function to log system stats to the file
log_system_stats_to_file(log_file)

# Inform the user that the log has been created
cat("System stats logged to:", log_file, "\n")
