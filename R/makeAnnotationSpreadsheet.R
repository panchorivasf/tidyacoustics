
makeAnnotationSpreadsheet <- function(folder, date = waiver(), filename){

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
