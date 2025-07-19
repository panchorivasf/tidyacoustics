#' Create an annotation spreadsheet for audio files
#'
#' Generates a spreadsheet template for annotating biological sounds (insects, birds,
#' mammals, amphibians) and environmental noise (wind, fire) in audio recordings.
#'
#' @param folder Character string specifying the path to the directory containing
#'               audio files (must be .wav format).
#' @param date Optional character string or date to filter files by date. Uses
#'             `waiver()` by default to include all files. The function looks for
#'             this date pattern in filenames.
#' @param filename Character string for the output Excel filename (without extension).
#'
#' @return Invisibly returns the created data frame and writes an Excel spreadsheet
#'         to the specified folder. The spreadsheet contains columns for each
#'         annotation category with empty strings ready for manual annotation.
#'
#' @details The function:
#' \itemize{
#'   \item Changes the working directory to the specified folder (use with caution)
#'   \item Finds all .wav files in the directory
#'   \item Filters files by date pattern if provided
#'   \item Creates a data frame with annotation columns
#'   \item Saves as an Excel file with the specified name
#' }
#'
#' @note
#' \itemize{
#'   \item Requires the `xlsx` package for Excel output
#'   \item Changes the working directory during execution (may affect subsequent operations)
#'   \item All annotation columns are initialized with empty strings
#' }
#'
#' @examples
#' \dontrun{
#' # Create annotation spreadsheet for all WAV files in "recordings" folder
#' annotation_spreadsheet("path/to/recordings", filename = "annotations")
#'
#' # Create spreadsheet only for files containing "2023-05-15" in their names
#' annotation_spreadsheet("path/to/recordings",
#'                       date = "2023-05-15",
#'                       filename = "may_annotations")
#' }
#'
#' @importFrom dplyr mutate data_frame
#' @importFrom xlsx write.xlsx
#' @export
annotation_spreadsheet <- function(folder, date = waiver(), filename) {
  require(dplyr)
  require(xlsx)

  setwd(folder)
  audioFiles <- list.files(pattern = ".wav")
  fireDateFiles <- grep(date, audioFiles, value = TRUE)
  df <- data_frame(file_name = fireDateFiles)
  df <- df |>
    mutate(insects = "",
           birds = "",
           mammals = "",
           amphibians = "",
           wind = "",
           fire = "")

  write.xlsx(df, file = paste0(filename, ".xlsx"))
}
annotation_spreadsheet <- function(folder, date = waiver(), filename){

  require(dplyr)
  require(xlsx)

  setwd(folder)
  audioFiles <- list.files(pattern = ".wav")
  fireDateFiles <- grep(date, audioFiles, value = TRUE)
  df <- data_frame(file_name = fireDateFiles)
  df <- df  |>
    mutate(insects = "",
           birds = "",
           mammals = "",
           amphibians = "",
           wind = "",
           fire = "")


  write.xlsx(df, file = paste0(filename, ".xlsx"))

}
