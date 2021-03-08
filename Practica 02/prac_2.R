install.packages("ggfortify")
install.packages("tseries")
install.packages("forecast")
install.packages("dplyr")
library(ggfortify)
library(dplyr)
library(tseries)
library(forecast)
data(AirPassengers)
AP <- AirPassengers
# Take a look at the class of the dataset AirPassengers
class(AP)
## [1] "ts"

# Analisis exploratorio de datos
AP
# Check for missing values
sum(is.na(AP))
## [1] 0
# Check the frequency of the time series
frequency(AP)
## [1] 12
# Check the cycle of the time series
cycle(AP)
# Review the table summary
summary(AP)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   104.0   180.0   265.5   280.3   360.5   622.0
# Plot the raw data using the base plot function
plot(AP,xlab="Date", ylab = "Passenger numbers (1000's)",main="Air Passenger numbers from 1949 to 1961")
autoplot(AP) + labs(x ="Date", y = "Passenger numbers (1000's)", title="Air Passengers from 1949 to 1961") 
boxplot(AP~cycle(AP),xlab="Date", ylab = "Passenger Numbers (1000's)" ,main ="Monthly Air Passengers Boxplot from 1949 to 1961")

# Descomposicion de series de tiempo
decomposeAP <- decompose(AP,"multiplicative")
autoplot(decomposeAP)

# Modelo Arima
arimaAP <- auto.arima(AP)
arimaAP
ggtsdiag(arimaAP)
forecastAP <- forecast(arimaAP, level = c(95), h = 36)
autoplot(forecastAP)
