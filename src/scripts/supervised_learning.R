# baselines
# variable: Music.effects

# Preprocesamiento previo
supervised_music_effects_preprocessing = function(data){
  data = remove_na_target_rows_classification(data)
  data = impute_all_na(data)
  return (data)
}

# Agrupa Music.effects en dos clases: "Improve" y "Not.improve"
# (Music.effects codificada: 0 = Worsen, 1 = No effect, 2 = Improve)
binarize_music_effects = function(data){
   data$Music.effects = factor(ifelse(data$Music.effects == 2, "Improve", "Not.improve"),
                              levels = c("Improve", "Not.improve"), ordered = TRUE)
  return (data)
}

# Particion estratificada
split_train_test = function(data, target = "Music.effects", p = 0.8, seed = 42){
  set.seed(seed)
  idx = createDataPartition(data[[target]], p = p, list = FALSE)
  return (list(train = data[idx, ], test = data[-idx, ]))
}

# Up-sampling
upsample_train = function(train, target = "Music.effects", seed = 42){
  set.seed(seed)
  predictors = train[, setdiff(names(train), target)]
  res = upSample(x = predictors, y = train[[target]], yname = target)
  return (res)
}

# ------------------------------------------------------------------
# Evaluacion
# ------------------------------------------------------------------

music_effects_summary = function(data, lev = NULL, model = NULL){
  cm = confusionMatrix(data$pred, data$obs, positive = "Not.improve")
  f1 = cm$byClass[["F1"]]
  c(Accuracy = cm$overall[["Accuracy"]],
    Kappa = cm$overall[["Kappa"]],
    Sens = cm$byClass[["Sensitivity"]],
    Spec = cm$byClass[["Specificity"]],
    BalancedAccuracy = cm$byClass[["Balanced Accuracy"]],
    F1 = ifelse(is.na(f1), 0, f1))
}

music_effects_control = function(index, sampling = "up"){
  trainControl(method = "repeatedcv",
               index = index,
               sampling = sampling,
               summaryFunction = music_effects_summary,
               savePredictions = "final")
}

evaluate_on_test = function(models, x_test, y_test){
  res = lapply(names(models), function(name){
    pred = predict(models[[name]], newdata = x_test)
    metrics = music_effects_summary(data.frame(pred = pred, obs = y_test))
    data.frame(Modelo = name, t(round(metrics, 3)), check.names = FALSE)
  })
  return (do.call(rbind, res))
}

# ------------------------------------------------------------------
# OneR 
# ------------------------------------------------------------------

oner_fit = function(x, y){
  x = as.data.frame(x)
  rules = lapply(names(x), function(col){
    v = x[[col]]
    breaks = NULL
    if (is.numeric(v) && length(unique(v)) > 10) {
      breaks = unique(quantile(v, probs = seq(0, 1, length.out = 6)))
      breaks[1] = -Inf
      breaks[length(breaks)] = Inf
      v = cut(v, breaks)
    }
    tab = table(as.character(v), y)
    pred = colnames(tab)[apply(tab, 1, which.max)]
    names(pred) = rownames(tab)
    list(col = col, breaks = breaks, pred = pred,
         err = sum(tab) - sum(apply(tab, 1, max)))
  })
  errors = sapply(rules, function(r) r$err)
  best = rules[[which.min(errors)]]
  best$errors = sort(setNames(errors / length(y), names(x)))
  best$default = names(which.max(table(y)))
  best$levels = levels(y)
  return (best)
}

oner_predict = function(model, newdata){
  v = as.data.frame(newdata)[[model$col]]
  if (!is.null(model$breaks)) v = cut(v, model$breaks)
  pred = unname(model$pred[as.character(v)])
  pred[is.na(pred)] = model$default
  return (factor(pred, levels = model$levels))
}

oner_model = list(
  label = "OneR",
  library = NULL,
  type = "Classification",
  parameters = data.frame(parameter = "none", class = "character", label = "none"),
  grid = function(x, y, len = NULL, search = "grid") data.frame(none = "none"),
  fit = function(x, y, wts, param, lev, last, weights, classProbs, ...) oner_fit(x, y),
  predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) oner_predict(modelFit, newdata),
  prob = NULL,
  sort = function(x) x
)

# ------------------------------------------------------------------
# Naive Bayes
# ------------------------------------------------------------------

nb_model = list(
  label = "Naive Bayes",
  library = "e1071",
  type = "Classification",
  parameters = data.frame(parameter = "none", class = "character", label = "none"),
  grid = function(x, y, len = NULL, search = "grid") data.frame(none = "none"),
  fit = function(x, y, wts, param, lev, last, weights, classProbs, ...)
    e1071::naiveBayes(x = as.data.frame(x), y = y),
  predict = function(modelFit, newdata, preProc = NULL, submodels = NULL)
    predict(modelFit, as.data.frame(newdata), type = "class"),
  prob = function(modelFit, newdata, preProc = NULL, submodels = NULL)
    as.data.frame(predict(modelFit, as.data.frame(newdata), type = "raw")),
  sort = function(x) x
)
