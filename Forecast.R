# to run must first download goose_competition_fall_2024.csv and set working directory to your own file 
# none of the libraries have to be downloaded to run basic model they are used to test goodness of fit and account for possible heteroskedasticity 

library(lubridate)
library(car)  # For Breusch-Pagan test
library(lmtest)  # For White test
library(sandwich)  # For robust standard errors
library(stargazer)

setwd("")
data <- read.csv("goose_competition_fall_2024.csv")
getwd()
str(data)

# Fit the model
model1 <- lm(cyclists ~ April + August + December + February + January + July + June + 
               March + May + November + October + September + Weekday + Weekend + 
               trend + mean_temp + percip.mm. + rain_binary, data=data)

# Model summary
summary(model1)
stargazer(model1, type = "text")

# Test for heteroskedasticity
bp_test <- bptest(model1)  # Breusch-Pagan test
white_test <- bptest(model1, ~ fitted(model1) + I(fitted(model1)^2))  # White test

# Print test results
print("Breusch-Pagan Test:")
print(bp_test)
print("White Test:")
print(white_test)

# Robust standard errors
robust_se <- coeftest(model1, vcov = vcovHC(model1, type = "HC0"))
print("Robust Standard Errors:")
print(robust_se)

# Forecasting
future_dates <- seq.Date(as.Date("2024-11-07"), as.Date("2024-11-29"), by = "day")
forecast_start_date <- as.Date("2024-11-16")

future_data <- data.frame(
  date = future_dates,
  November = 1,  # All rows are in November
  Weekday = ifelse(wday(future_dates) %in% 2:6, 1, 0),  
  Weekend = ifelse(wday(future_dates) %in% c(1, 7), 1, 0),  
  trend = seq(max(data$trend) + 1, by = 1, length.out = length(future_dates)), 
  mean_temp = ifelse(future_dates >= forecast_start_date, 
                     data$forecast_temp[match(future_dates, data$date)], 0),  
  percip.mm. = ifelse(future_dates >= forecast_start_date, 
                      data$forecast_percip[match(future_dates, data$date)], 0),  
  rain_binary = ifelse(future_dates >= forecast_start_date, 
                       ifelse(data$forecast_rain[match(future_dates, data$date)] > 0, 1, 0), 0)
)

future_data <- cbind(
  future_data,
  April = 0, August = 0, December = 0, February = 0, January = 0,
  July = 0, June = 0, March = 0, May = 0, October = 0, September = 0
)

# Make predictions
predictions <- predict(model1, newdata = future_data)
future_data$predicted_cyclists <- predictions

# View results
print(future_data)

# Plot predictions
plot(future_data$date, future_data$predicted_cyclists, type = "b", col = "blue",
     xlab = "Date", ylab = "Predicted Cyclists",
     main = "Cyclist Predictions for November 2024")

# Save model summary and test results
stargazer(model1, type = "text", out = "model_summary.txt")
writeLines(c("Breusch-Pagan Test:", capture.output(bp_test), "", "White Test:", capture.output(white_test)),
           "heteroskedasticity_tests.txt")
