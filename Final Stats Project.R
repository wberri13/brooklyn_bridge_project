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
#this is checking the unique values for each col so i can see the NAs. I ran these lines after to see if there were still NAs
unique_values <- unique(df$weather_summary)
unique_values
unique_valuestemp <- unique(df$temperature)

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

#quick check
uniques <- unique(df$Pedestrians)
uniques

#weather needs to be factor
df$weather_summary <- as.factor(df$weather_summary)
#-------------------------------------------------------------------------------

#checking what type of events there are
uniques <- unique(df$events)

#df$events<-as.factor(df$events)
df$hour_beginning <- as.POSIXct(df$hour_beginning,format = "%Y %b %d %I:%M:%S %p")
df$hour <- lubridate::hour(df$hour_beginning)
df$weekday <- lubridate::wday(df$hour_beginning, label = TRUE)
df$month <- lubridate::month(df$hour_beginning, label = TRUE)
df$hour_beginning <- NULL

#-------------------------------------------------------------------------------
#make base model to see what the R2 is and run variable selection. 
base_model <- lm(Pedestrians ~ ., data=df)
summary(base_model)
#our F score shows our model is significant enough to use! p-val < 2.2e-16

#variable selection:
library(olsrr) #variable screening package
ols_step_both_p(base_model, penter=0.05, prem=0.1, details=TRUE) 
#variable selection confirms keeping all variables. R^2 adjusted is .59 now

#-------------------------------------------------------------------------------
#PLOTTING to determine transformations on x and higher order terms
library(ggplot2)
ggplot(df, aes(x = temperature, y = Pedestrians)) +
geom_point(color = "blue") +
geom_smooth(method = "lm", se = FALSE, color = "red") +
labs(title = "Temperature vs Pedestrians", x = "Temperature", y = "Pedestrians")
#shows a linear trend

ggplot(df, aes(x = weather_summary, y = Pedestrians)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Weather Summary vs Pedestrians", x = "Weather Summary", y = "Pedestrians")

ggplot(df, aes(x = precipitation, y = Pedestrians)) +
  geom_point(color = "blue") +
  labs(title = "Precipitation vs Pedestrians", x = "Precipitation", y = "Pedestrians")

#applying this transformation to make it a more linear trend
#tries to linearize relationship a bit. does spread it out more. Not fully.
#In the model there should be a higher order term on precipitation.
ggplot(df, aes(x = sqrt(precipitation), y = Pedestrians)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Square Root of Precipitation vs Pedestrians", x = "Sqrt(Precipitation", y = "Pedestrians")

#Conclusion here: Looks like a higher order term would help fit model better. Hour^3/ Hour^2
ggplot(df, aes(x = hour, y =Pedestrians)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Hour vs Pedestrians", x = "Hour", y = "Pedestrians")

ggplot(df, aes(x = month, y =Pedestrians)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Month vs Pedestrians", x = "Hour", y = "Pedestrians")

ggplot(df, aes(x = weekday, y =Pedestrians)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Weekday vs Pedestrians", x = "Hour", y = "Pedestrians")

ggplot(df, aes(x = has_event, y =Pedestrians)) +
  geom_point(color = "blue") +
  labs(title = "Has event vs pedestrians", x = "Has Event", y = "Pedestrians")


#_______________________________________________________________
#Now we have to test correlation between all our variables. Using Variable Inflation Scores
#i have to use the base model here because higher order terms were in my other models
library(car)
vif(base_model) 
#No signification VIF scores. all less than 10.
#_______________________________________________________________

#lets rerun base model again
base_model <- lm(Pedestrians ~ weather_summary + temperature + precipitation + has_event +hour + weekday + month, data=df)
summary(base_model) #R^2 adjusted is 0.5878

#apply the precipitation transformation from graph and add a higher order term for precipitation
df$precipitation = sqrt(df$precipitation)
second_model <- lm(Pedestrians ~ . + I(precipitation^2), data=df)
summary(second_model) #R^2 adjusted is 0.5901, not much of a change

#apply the higher order terms on hour
third_model <-lm(Pedestrians ~ . + I(precipitation^2) + I(hour^2) + I(hour^3), data=df)
summary(third_model) #much better R^2 adjusted. R^2 adjusted is 0.7116

#going to try adding some interaction terms
#temperature and hour probably interact....
#does the effect of temperature depends on the time of day?
fourth_model <- lm(Pedestrians ~ . + I(precipitation^2) + I(hour^2) + I(hour^3) + temperature:hour, data=df)
summary(fourth_model)  #adjusted R^2 only went up a bit. Now it's 0.7191

#interaction of temperature and added event. R^2 adjusted is more or less the same. 
bad_model <- lm(Pedestrians ~ . + I(precipitation^2) + I(hour^2) + I(hour^3) + temperature:hour + temperature:has_event, data=df)
summary(bad_model)  #adjusted R^2 is 0.7195

#adding an interaction term for weather summary and hour. 
#rain does not have the same effect at all hours. same for every other weather summary possibility
fifth_model <- lm(Pedestrians ~ . + I(precipitation^2) + I(hour^2) + I(hour^3) + temperature:hour + weather_summary:hour, data=df)
summary(fifth_model) #R^2 adjusted here is 0.7437

#adding an interaction term for temperature and has event. 
#temperature doesnt necessarily have the same effect on an event vs no event.
sixth_model <- lm(Pedestrians ~ . + I(precipitation^2) + I(hour^2) + I(hour^3) + temperature:hour + weather_summary:hour + temperature:has_event, data=df)
summary(sixth_model) #R^2 adjusted here is 0.744. Not worth adding the interaction term

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
#RESIDUAL ANALYSIS
# plotting the residuals vs the variable "temperature":
plot(df$temperature, resid(final_model),
     xlab = "Temperature", 
     ylab = "Regression Residuals", 
     main = "Regression Residuals vs Temperature")
# adding a horizontal reference line at residual = 0 to the plot
abline(h = 0, col = "red")
#Residuals seem to be more or less randomly distributed. This is good.

# plotting the residuals vs the variable "hour" for the second model:
plot(df$hour, resid(final_model), 
     xlab = "hour", 
     ylab = "Regression Residuals", 
     main = "Regression Residuals vs Hour")
# adding a horizontal reference line at residual = 0 to the plot
abline(h = 0, col = "red")
#This shows an issue with our model. Residuals are not random around hour. 
#This may be because its not treating the hour as a cycle. hour 23 is only 1 hour away from hour 0
#Instead of applying a transformation, we will make the hour a factor that way they are treated separately.
dfHour_asFactor = df
dfHour_asFactor$hour <- as.factor(df$hour)
seventh_model  <- lm(Pedestrians ~ . + I(precipitation^2) + temperature:hour, data=dfHour_asFactor) #note: got rid of the last interaction term because it was giving NAs
summary(seventh_model) #this model performs the best. It has an R^2 adjusted of 0.8181
#we got rid of the higher order terms on hour in this situation
#Now, every hour is technically allowed to have a different residual behavior

#lets keep moving with seventh_model and hour being treated as a factor
df = dfHour_asFactor
final_model <- lm(Pedestrians ~ . + I(precipitation^2) + temperature:hour, data=dfHour_asFactor)
summary(final_model)

#not fair to judge based off of just precipitation, there are higher order terms
# plotting the residuals vs the precipitation:
plot(df$precipitation, resid(final_model), 
     xlab = "1/Precipitation", 
     ylab = "Regression Residuals", 
     main = "Regression Residuals vs 1 over precipitation")
# adding a horizontal reference line at residual = 0 to the plot
abline(h = 0, col = "red")

#code from notes
plot(df$Pedestrians, resid(final_model),
     xlab = "Pedestrians",
     ylab = "Residuals",
     main = "Residuals vs Pedestrians")
abline(h = 0, col = "red", lwd = 2)
#very skewed increasing residuals as pedestrians increase
#multiplicative error?
#applying log transformation

plot(log(df$Pedestrians), resid(final_model),
     xlab = "Pedestrians",
     ylab = "Residuals",
     main = "Pedestrians vs Residuals")
abline(h = 0, col = "red", lwd = 2)
#log transformation definitely helped randomize our residuals.

dfcopy = df
dfcopy$Pedestrians = log(dfcopy$Pedestrians +1 ) #have to add 1 because log doesnt take in 0
final_model <- lm(Pedestrians ~ . + I(precipitation^2) + temperature:hour, data=dfcopy)
summary(final_model) #adjusted R-squared is now 0.853 because of transformation
