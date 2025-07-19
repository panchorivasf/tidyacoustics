#' Extract Date Ranges from Audio Files in Directory Structure
#'
#' This function recursively searches through a directory structure to find audio
#' files and extracts temporal information from their filenames. It processes
#' multiple directories in parallel and returns a summary of date ranges, file
#' counts, and storage information for each folder containing audio files.
#'
#' @param parentFolder Character string specifying the path to the parent directory
#'   to search. The function will recursively search all subdirectories within
#'   this folder for audio files.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{folder}{Character. Full path to each subfolder containing audio files}
#'   \item{sensor}{Character. Unique sensor identifier extracted from filenames}
#'   \item{start}{POSIXct. Earliest datetime found in the folder's audio files}
#'   \item{end}{POSIXct. Latest datetime found in the folder's audio files}
#'   \item{n.files}{Numeric. Number of audio files in the folder}
#'   \item{total.size}{Numeric. Total size of all audio files in megabytes}
#' }
#'
#' @details
#' The function expects audio filenames to follow a specific naming convention:
#' \code{SENSOR_YYYYMMDD_HHMMSS.*} where:
#' \itemize{
#'   \item SENSOR is the sensor identifier
#'   \item YYYYMMDD is the date (year, month, day)
#'   \item HHMMSS is the time (hour, minute, second)
#' }
#'
#' Supported audio file extensions: .wav, .WAV, .wac, .flac
#'
#' The function uses parallel processing to improve performance when dealing with
#' large directory structures. It automatically detects available CPU cores and
#' uses all but one for processing.
#'
#' Folders without audio files are included in the output with NA values for
#' sensor, start, and end dates, and 0 for file count and total size.
#'
#' @examples
#' \dontrun{
#' # Process audio files in a directory structure
#' results <- date_range("/path/to/audio/files")
#'
#' # View summary of results
#' head(results)
#'
#' # Check date range across all files
#' range(c(results$start, results$end), na.rm = TRUE)
#'
#' # Total number of files processed
#' sum(results$n.files)
#' }
#'
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel stopImplicitCluster detectCores
#' @importFrom lubridate as.POSIXct
#'
#' @seealso
#' \code{\link[base]{list.dirs}} for directory listing,
#' \code{\link[base]{list.files}} for file listing,
#' \code{\link[foreach]{foreach}} for parallel iteration
#'
#' @export
#'
#' @author Francisco Rivas
#'
date_range <- function(parentFolder) {

  # Ensure the parentFolder ends with a slash
  if (!grepl("/$", parentFolder)) {
    parentFolder <- paste0(parentFolder, "/")
  }

  # Find all subdirectories within the parent folder
  subfolders <- list.dirs(path = parentFolder, full.names = TRUE,
                          recursive = TRUE)

  # Register parallel backend to use multiple cores
  no_cores <- detectCores() - 1
  registerDoParallel(cores=no_cores)

  # Initialize an empty list to store results from each subfolder
  results <- list()

  # Use foreach to iterate over subfolders in parallel
  results <- foreach(folder = subfolders, .combine='rbind') %dopar% {
    # List files matching the pattern for '.wav', '.WAV', '.wac', or '.flac'
    wavList <- list.files(folder, pattern = "\\.(wav|WAV|wac|flac)$",
                          full.names = FALSE, recursive = FALSE)

    if(length(wavList) == 0) {
      return(data.frame(folder = folder, sensor = NA, start = NA, end = NA,
                        n.files = 0, total.size = 0))
    }

    fileSizes <- file.info(file.path(folder, wavList))$size
    totalSize <- sum(fileSizes) / (1024 * 1024) # Convert to megabytes

    df <- data.frame(filename = wavList)
    df$datetime <- as.POSIXct(sub(".*_([0-9]{8})_([0-9]{6}).*", "\\1 \\2",
                                  df$filename),
                              format = "%Y%m%d %H%M%S")
    df$sensor_id <- sub("([^_]+)_.*", "\\1", df$filename)

    start <- min(df$datetime, na.rm = TRUE)
    end <- max(df$datetime, na.rm = TRUE)

    data.frame(folder = folder, sensor = unique(df$sensor_id),
               start = start, end = end,
               n.files = length(wavList),
               total.size = totalSize)
  }

  # Stop the parallel backend
  stopImplicitCluster()

  return(results)
}

