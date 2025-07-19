#' Calculate summary statistics for acoustic indices
#'
#' Processes a data frame of acoustic index values, reformats the data, and calculates
#' comprehensive summary statistics for each index by recording unit and channel.
#'
#' @param df A data frame containing acoustic index values. Expected to contain columns
#'           including 'offset', 'channel', and various index columns (NDSI to CENT).
#'           The input file names should contain embedded datetime information in the
#'           format: "UNITID_YYYYMMDD_HHMMSS.wav".
#'
#' @return A data frame with summary statistics for each acoustic index, including:
#' \itemize{
#'   \item{mean_value: Mean value of the index}
#'   \item{closest_to_mean: Recording file closest to the mean value}
#'   \item{max_value: Maximum index value}
#'   \item{closest_to_max: Recording file closest to the maximum value}
#'   \item{min_value: Minimum index value}
#'   \item{closest_to_min: Recording file closest to the minimum value}
#'   \item{median_value: Median index value}
#'   \item{closest_to_median: Recording file closest to the median value}
#'   \item{mode_value: Mode of index values}
#'   \item{closest_to_mode: Recording file closest to the mode value}
#' }
#'
#' @details The function performs the following operations:
#' \itemize{
#'   \item Converts column names to lowercase
#'   \item Removes the 'offset' column
#'   \item Converts channel numbers (0,1) to labels ('left','right')
#'   \item Reshapes data from wide to long format for index values
#'   \item Extracts datetime, date, hour, and unit ID from filenames
#'   \item Calculates comprehensive summary statistics grouped by unit, channel, and index
#' }
#'
#' @note Important assumptions about input data:
#' \itemize{
#'   \item Filenames must follow the pattern "UNITID_YYYYMMDD_HHMMSS.wav"
#'   \item Channel values must be 0 (left) or 1 (right)
#'   \index columns should be named from 'ndsi' to 'cent'
#'   \item Requires DescTools package for mode calculation
#' }
#'
#' @examples
#' \dontrun{
#' # Process acoustic index data
#' summary_stats <- indices_summary(acoustic_data)
#'
#' # View summary for a specific unit and index
#' filter(summary_stats, unit_id == "ARU1", index == "ndsi")
#' }
#'
#' @importFrom dplyr select mutate case_when group_by reframe everything
#' @importFrom tidyr pivot_longer
#' @importFrom lubridate as_datetime date hour
#' @importFrom DescTools Mode
#' @export
indices_summary<-function(df){
  # Reformat data frame
  colnames(df) <- tolower(colnames(df))

  df <- df |> select(-offset)

  df <- df |>
    mutate(channel = case_when(
      channel == 0 ~ 'left',
      channel == 1 ~ 'right'
    )) |>
    pivot_longer(cols = c(ndsi:cent), names_to = "index", values_to = "value")

  df$datetime <- sapply(strsplit(df$in.file, "[_]"), function(x) paste(x[2], gsub(".wav", "", x[3]), sep = " "))
  df$datetime <- as_datetime(df$datetime, format = "%Y%m%d %H%M%S")
  df$date <- lubridate::date(df$datetime)
  df$hour <- lubridate::hour(df$datetime)
  df$unit_id <- sapply(strsplit(df$in.file,"[_]"), function(x){x[1]})

  df <- df |> select(c(in.file, unit_id, datetime, date, hour, everything()))


  # Get summary statistics and figure out which recordings are closest to those:
  summary <- df |>
    group_by(unit_id, channel, index) |>
    reframe(
      mean_value = mean(value, na.rm = TRUE),
      closest_to_mean = in.file[which.min(abs(value - mean(value, na.rm = TRUE)))],

      max_value = max(value, na.rm = TRUE),
      closest_to_max = in.file[which.min(abs(value - max(value, na.rm = TRUE)))],

      min_value = min(value, na.rm = TRUE),
      closest_to_min = in.file[which.min(abs(value - min(value, na.rm = TRUE)))],

      median_value = median(value, na.rm = TRUE),
      closest_to_median = in.file[which.min(abs(value - median(value, na.rm = TRUE)))],

      mode_value = max(DescTools::Mode(value)),
      closest_to_mode = in.file[which.min(abs(value - max(DescTools::Mode(value))))]
    )

  return(summary)

}
