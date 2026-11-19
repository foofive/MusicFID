library(caret)
library(dplyr)

remove_noise_columns = function(data) {
  data$Timestamp = NULL
  data$Permissions = NULL
  data$Primary.streaming.service = NULL
  return(data)
}

convert_columns_to_factor = function(data) {
  freq_levels = c("Never", "Rarely", "Sometimes", "Very frequently")
  freq_cols = grep("^Frequency", names(data), value = TRUE)
  
  for (col in freq_cols) {
    data[[col]] = factor(data[[col]], levels = freq_levels, ordered = TRUE)
  }
  
  yn_cols = c("While.working", "Instrumentalist", "Composer",
              "Exploratory", "Foreign.languages")
  
  for (col in yn_cols) {
    data[[col]] = factor(data[[col]], levels = c("No", "Yes"))
  }

  data$Music.effects = factor(data$Music.effects,
                              levels = c("Worsen", "No effect", "Improve"),
                              ordered = TRUE)
  
  return(data)
}


remove_na_target_rows_classification = function(data) {
  data = data[!is.na(data$Music.effects),]
  return(data)
}

ordinal_cols_encoder = function(data) {
  freq_cols = c(grep("^Frequency", names(data), value = TRUE))
  yn_cols = c("While.working", "Instrumentalist", "Composer",
              "Exploratory", "Foreign.languages")
  
  cols = c(freq_cols, "Music.effects", yn_cols)
  
  for (col in cols) {
    data[[col]] = as.integer(data[[col]])-1
  }
  
  return(data)
}

nominal_cols_encoder = function (data) {
  
}


return_sorted_na_per_row_if_present = function (data){
  na_counts = data %>%
    mutate(na_count = rowSums(is.na(.))) %>%
    filter(na_count > 0) %>%
    arrange(desc(na_count))
  return(na_counts)
} 

#impute_all_na

