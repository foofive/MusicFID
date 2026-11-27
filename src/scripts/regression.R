regression_round_levels = function(data) {
  return (
    data |>
      mutate(Anxiety = round(Anxiety)) |>
      mutate(Depression = round(Depression)) |>
      mutate(Insomnia = round(Insomnia)) |>
      mutate(OCD = round(OCD))
  )
}

regression_count_levels = function(data) {
  return (
    list(
      data |> group_by(Anxiety) |> count(),
      data |> group_by(Depression) |> count(),
      data |> group_by(Insomnia) |> count(),
      data |> group_by(OCD) |> count()
    )
  )
}

regression_get_cor = function(data, threshold = 0.25) {
  cor_anxiety = musicmh_regr |> cor(musicmh_regr$Anxiety)
  cor_depression = musicmh_regr |> cor(musicmh_regr$Depression)
  cor_insomnia = musicmh_regr |> cor(musicmh_regr$Insomnia)
  cor_ocd = musicmh_regr |> cor(musicmh_regr$OCD)
  
  return (
    list(
      as.data.frame(cor_anxiety) |> filter(abs(V1) > threshold),
      as.data.frame(cor_depression) |> filter(abs(V1) > threshold),
      as.data.frame(cor_insomnia) |> filter(abs(V1) > threshold),
      as.data.frame(cor_ocd) |> filter(abs(V1) > threshold)
    )
  )
}

regression_standardize = function(data) {
  standardized = preProcess(data, method = c("center", "scale"))
  return (predict(standardized, data))
}

regression_train_models = function(data, k) {
  train_control = trainControl(
    method = "cv",
    number = k
  )
  return (
    list(
      train(Anxiety~., data = data, method = "lm", trControl = train_control),
      train(Depression~., data = data, method = "lm", trControl = train_control),
      train(Insomnia~., data = data, method = "lm", trControl = train_control),
      train(OCD~., data = data, method = "lm", trControl = train_control)
    )
  )
}