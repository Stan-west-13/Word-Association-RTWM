# Function to load the most recent file based on most recent modification
load_most_recent_by_mtime <- function(directory, pattern = NULL, read_fun = read_rds, ...) {
  require(readr)
  # List files (optionally filter by pattern, e.g., "\\.csv$")
  files <- list.files(directory, pattern = pattern, full.names = TRUE)
  
  if (length(files) == 0) {
    stop("No matching files found in the specified directory.")
  }
  
  # Get file info including modification times
  file_info <- file.info(files)
  
  # Find the file with the latest modification time
  most_recent_file <- rownames(file_info)[which.max(file_info$mtime)]
  
  message("Loading most recently modified file: ", most_recent_file)
  
  # Load the file using the provided read function
  return(read_fun(most_recent_file, ...))
}
