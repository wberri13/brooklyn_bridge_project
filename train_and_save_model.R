#i imported the file as pedestrians, include headeres, do NOT select as factor
df = pedestrians

#dropping useless cols
df$Towards.Brooklyn = NULL
df$Towards.Manhattan = NULL
df$lat = NULL
df$long = NULL
df$location = NULL
df$Location1 = NULL

#-------------------------------------------------------------------------------
#changing event column to be boolean
df$has_event <- ifelse(df$events == "" | is.na(df$events), 0, 1)
df$events <- NULL

#dropping NAs for specified cols.
#this is every column except events because events
df_clean <- df[complete.cases(df$hour_beginning, df$Pedestrians, df$weather_summary, df$temperature), ]
unique_valuestemp <- unique(df_clean$temperature)
df <- df[df$weather_summary != "", ]

#setting our working dataset to df_clean now
df = df_clean

#-------------------------------------------------------------------------------

#Converting Ped. Count to numeric had to remove commas because they were being treated as chars
df$Pedestrians <- as.numeric(gsub(",", "", df$Pedestrians))

#weather needs to be factor
df$weather_summary <- as.factor(df$weather_summary)
#-------------------------------------------------------------------------------

df$hour_beginning <- as.POSIXct(df$hour_beginning,format = "%Y %b %d %I:%M:%S %p")
df$hour <- lubridate::hour(df$hour_beginning)
df$weekday <- lubridate::wday(df$hour_beginning, label = TRUE)
df$month <- lubridate::month(df$hour_beginning, label = TRUE)
df$hour_beginning <- NULL
#-------------------------------------------------------------------------------

#apply the precipitation transformation from graph and add a higher order term for precipitation
df$precipitation = sqrt(df$precipitation)
#at this point we have tested all interaction terms we think may exist. These are the ones that we kept
final_model <- lm(Pedestrians ~ . + I(precipitation^2) + I(hour^2) + I(hour^3) + temperature:hour + weather_summary:hour, data=df)
summary(final_model) #final R^2 adjust here is 0.7437

#----------------------------------------------
install.packages("lmtest")
library(lmtest)

#running the Durbin-Watson d-Test for correlation since this deals with time. 
#d ~ 2, residuals are uncorrelated
dwtest(final_model)

#-------------------------------------------------------------------------------
dfHour_asFactor = df
dfHour_asFactor$hour <- as.factor(df$hour)

#lets keep moving with seventh_model and hour being treated as a factor
df = dfHour_asFactor
final_model <- lm(Pedestrians ~ . + I(precipitation^2) + temperature:hour, data=dfHour_asFactor)
summary(final_model)

dfcopy = df
dfcopy$Pedestrians = log(dfcopy$Pedestrians +1 ) #have to add 1 because log doesnt take in 0
final_model <- lm(Pedestrians ~ . + I(precipitation^2) + temperature:hour, data=dfcopy)
summary(final_model) #adjusted R-squared is now 0.853 because of transformation


# after final_model is fit on dfcopy
saveRDS(final_model, "final_model.rds")

# save factor levels so Streamlit predictions won’t break
levels_list <- list(
  weather_summary = levels(dfcopy$weather_summary),
  hour = levels(dfcopy$hour),
  weekday = levels(dfcopy$weekday),
  month = levels(dfcopy$month)
)
saveRDS(levels_list, "factor_levels.rds")

