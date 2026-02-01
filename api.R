library(plumber)
source("predict.R")

#* @post /predict
function(req){
  body <- jsonlite::fromJSON(req$postBody)
  
  pred <- predict_pedestrians(
    weather_summary = body$weather_summary,
    temperature = body$temperature,
    precipitation = body$precipitation,
    has_event = body$has_event,
    hour = body$hour,
    weekday = body$weekday,
    month = body$month
  )
  
  list(predicted_pedestrians = pred)
}

#* @get /levels
function(){
  lvls <- readRDS("factor_levels.rds")
  lvls
}