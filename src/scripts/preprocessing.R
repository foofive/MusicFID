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
  
  encoder = dummyVars(~ Fav.genre, data = data, levelsOnly = TRUE)
  favgenre_dummies = as.data.frame(predict(encoder, newdata = data))
  
  colnames(favgenre_dummies) = colnames(favgenre_dummies) %>%
    gsub("[^A-Za-z0-9]+", "_", .) %>%
    gsub("^_|_$", "", .)
  
  res = data %>%
    mutate(favgenre_dummies, .before = "Exploratory") %>%
    select(-Fav.genre)

  return(res)
}


return_sorted_na_per_row_if_present = function (data){
  na_counts = data %>%
    mutate(na_count = rowSums(is.na(.))) %>%
    filter(na_count > 0) %>%
    arrange(desc(na_count))
  return(na_counts)
} 


remove_outlier_bpm_values = function (data) {
  bpm_to_remove = data$BPM < 20 | data$BPM > 240
  data$BPM[bpm_to_remove] = NA
  return(data)
}


impute_all_na = function(data,cols = NULL, method = "bagImpute", impute_targets = FALSE) {
  
  col_names = if (is.null(cols)) colnames(data) else cols
  
  targets = tail(colnames(data), n = 5)

  targets_with_na = targets[colSums(is.na(data[, targets, drop = FALSE])) > 0]
  if (!impute_targets) {
    col_names = setdiff(col_names, targets_with_na)
  }
  
  # Objetivos SIN NA: se usan como predictoras porque no van a ser imputadas
  targets_complete = setdiff(targets, targets_with_na)
  model_cols = union(col_names, targets_complete)
  
  # Columnas a imputar
  imp_cols = names(which(colSums(is.na(data[, col_names, drop = FALSE])) > 0))
  
  if (length(imp_cols) == 0) return(data)
  
  model_imp = preProcess(data[, model_cols, drop = FALSE], method = method)
  imputed = predict(model_imp, newdata = data[, model_cols, drop = FALSE])
  
  data[, imp_cols] = imputed[, imp_cols, drop = FALSE]
  
  data[, imp_cols] = data[, imp_cols, drop = FALSE] %>%
    mutate(across(where(is.numeric), ~ round(.x, 0)))
  
  return(data)
}


