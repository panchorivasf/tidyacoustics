library(tidyverse)
indices_summary<-function(df){
  # Reformat data frame
  colnames(df) <- tolower(colnames(df))
  
  df <- df %>% select(-offset)
  
  df <- df %>%
    mutate(channel = case_when(
      channel == 0 ~ 'left',
      channel == 1 ~ 'right'
    )) %>%
    pivot_longer(cols = c(ndsi:cent), names_to = "index", values_to = "value")
  
  df$datetime <- sapply(strsplit(df$in.file, "[_]"), function(x) paste(x[2], gsub(".wav", "", x[3]), sep = " "))
  df$datetime <- as_datetime(df$datetime, format = "%Y%m%d %H%M%S")
  df$date <- lubridate::date(df$datetime)
  df$hour <- lubridate::hour(df$datetime)
  df$unit_id <- sapply(strsplit(df$in.file,"[_]"), function(x){x[1]})
  
  df <- df %>% select(c(in.file, unit_id, datetime, date, hour, everything()))
  
  
  # Get summary statistics and figure out which recordings are closest to those:
  summary <- df %>%
    group_by(unit_id, channel, index) %>%
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

# Example
summary <- indices_summary(indices)

