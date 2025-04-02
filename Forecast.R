library(stargazer)
library(lubridate)
setwd("/Users/benburton/Documents/Econ 345/Forecastingcomp")
data <- read.csv("goose_competition_fall_2024.csv")
getwd()
str(data)

model1 <- lm(cyclists ~ April + August + December + February + January + July + June + March + May + November + October + September + Weekday + Weekend + trend + mean_temp + percip.mm. + rain_binary, data=data)

summary(model1)
stargazer(model1, type = "text")


future_dates <- seq.Date(as.Date("2024-11-07"), as.Date("2024-11-29"), by = "day")

forecast_start_date <- as.Date("2024-11-16")

  future_data <- data.frame(
    date = future_dates,
    November = 1,  # All rows are in November
    Weekday = ifelse(wday(future_dates) %in% 2:6, 1, 0),  
    Weekend = ifelse(wday(future_dates) %in% c(1, 7), 1, 0),  
    trend = seq(max(data$trend) + 1, by = 1, length.out = length(future_dates)), 
    mean_temp = ifelse(future_dates >= as.Date("2024-11-16"), 
                       data$forecast_temp[match(future_dates, data$date)], 0),  
    percip.mm. = ifelse(future_dates >= as.Date("2024-11-16"), 
                        data$forecast_percip[match(future_dates, data$date)], 0),  
    rain_binary = ifelse(future_dates >= as.Date("2024-11-16"), 
                         ifelse(data$forecast_rain[match(future_dates, data$date)] > 0, 1, 0), 0)
  )


future_data <- cbind(
  future_data,
  April = 0, August = 0, December = 0, February = 0, January = 0,
  July = 0, June = 0, March = 0, May = 0, October = 0, September = 0
)

predictions <- predict(model1, newdata = future_data)

# Add predictions to future_data
future_data$predicted_cyclists <- predictions

# View the results
print(future_data)


plot(future_data$date, future_data$predicted_cyclists, type = "b", col = "blue",
     xlab = "Date", ylab = "Predicted Cyclists",
     main = "Cyclist Predictions for November 2024")

# Save model summary as a text file
stargazer(model1, type = "text", out = "model_summary.txt")


