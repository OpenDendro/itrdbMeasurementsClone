library(lubridate)
url <- "http://www1.ncdc.noaa.gov/pub/data/metadata/published/paleo/dif/xml/noaa-tree-2657.xml"
local_file <- "/Users/andybunn/Documents/research/openDendro/itrdbMeasurementsClone/dif_files/noaa-tree-2657.xml"

library(httr)

url <- "http://example.com/yourfile.txt"  # URL of the file you want to download
local_file <- "yourfile.txt"              # Local filename

# Function to get last modified timestamp of a file
get_file_last_modified <- function(file_path) {
  file.info(file_path)$mtime
}

# Check if file exists locally
if (file.exists(local_file)) {
  # Get last modified timestamp of local file
  local_last_modified <- file.info(local_file)$mtime

  # Send a HEAD request to get last modified timestamp of file from the server
  response <- HEAD(url)
  remote_lm_char <- headers(response)$`last-modified`
  # as POSIXct
  server_last_modified <- parse_date_time(remote_lm_char, "a, d b Y H:M:S",tz = "GMT")
  #server_last_modified <- as.POSIXct(headers(response)$`last-modified`, tz = "GMT")

  # Compare timestamps
  if (server_last_modified > local_last_modified) {
    # Download the file
    download.file(url, local_file, mode = "wb")
    cat("File downloaded.\n")
  } else {
    cat("Local file is already up to date.\n")
  }
} else {
  # File doesn't exist locally, so download it
  download.file(url, local_file, mode = "wb")
  cat("File downloaded.\n")
}
