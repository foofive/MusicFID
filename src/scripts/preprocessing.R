remove_noise_columns = function(data) {
  data$Timestamp = NULL
  data$Permissions = NULL
  return(data)
}
