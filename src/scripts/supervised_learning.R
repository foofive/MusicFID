# baselines
# variable: Music.effects

# Preprocesamiento previo
supervised_music_effects_preprocessing = function(data){
  data = remove_na_target_rows_classification(data)
  data = impute_all_na(data)
  return (data)
}
