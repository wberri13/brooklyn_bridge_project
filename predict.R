predict_pedestrians <- function(weather_summary, temperature, precipitation, has_event,
                                hour, weekday, month) {
  
  model <- readRDS("final_model.rds")
  lvls  <- readRDS("factor_levels.rds")
  
  # replicate transformation of sqrt of precipitation
  precipitation <- sqrt(as.numeric(precipitation))
  
  # build a one-row dataframe with exact column names model expects
  new <- data.frame(
    weather_summary = factor(weather_summary, levels = lvls$weather_summary),
    temperature = as.numeric(temperature),
    precipitation = precipitation,
    has_event = as.numeric(has_event),
    hour = factor(as.integer(hour), levels = lvls$hour),
    weekday = factor(weekday, levels = lvls$weekday),
    month = factor(month, levels = lvls$month)
  )
  
  # predict on log scale (because your model was fit on log(Pedestrians + 1))
  pred_log <- predict(model, newdata = new)
  
  # convert back to pedestrian count
  pred_count <- exp(pred_log) - 1
  
  return(as.numeric(pred_count))
}
