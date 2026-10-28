remove_noise_columns = function(data) {
  data$Timestamp = NULL
  data$Permissions = NULL
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