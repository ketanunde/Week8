

# In this example, 1 value = 1 month
# data starts from January 2003
monthly_sales <- c(18, 54, 56, 24, 87, 65, 45, 23, 19, 65, 35, 37,
                   24, 65, 37, 86, 34, 44, 96, 24, 42, 16, 25, 39)

# Sales data starts from Jan2003 and is for 24 months
# Frequency = no of observation per unit in time
# In this  example, we have 2 x 12 months of data

# Yearly data -> frequency = 1
# Monthly -> frequency = 12
# daily -> depends on data content, typically either 7
# Yearly -> 1
# Quaterly -> 4

quarterly_sales <- c(2, 4, 6, 2, 7, 4, 7, 8)

# change month_sales to a time  serie object
ts_monthly_sales <- ts(monthly_sales, start = c(2003, 1), frequency = 12)
ts_monthly_sales

plot(ts_monthly_sales)
start(ts_monthly_sales)
end(ts_monthly_sales)
frequency(ts_monthly_sales)

library(forecast)
opar <- par(no.readonly = TRUE)
par(mfrow = c(2,2))

ylim <- c(min(Nile), max(Nile))
# ma() allows us to smooth the nile time series
plot(Nile, main = "Raw time series")
plot (ma(Nile, 3), main = "Simple Moving Average (k=3)", ylim = ylimit)
plot (ma(Nile, 7), main = "Simple Moving Average (k=7)")#, ylim = ylimit)
plot (ma(Nile, 15), main = "Simple Moving Average (k=15)")#, ylim = ylimit)
par(opar)

library(tseries)
adf.test(Nile)

?AirPassengers
air_passengers <- ts(AirPassengers, start = c(1949, 1), frequency = 12)
class(air_passengers)
start(air_passengers)
end(air_passengers)
frequency(air_passengers)
summary(air_passengers)

cycle(air_passengers)

# Check the missing values
na_records <- air_passengers[!complete.cases(air_passengers)]
sum(na_records)

# Examining whether ts is additive or multiplicative
plot(air_passengers, xlab = "Date", 
     ylab = "Passenger number (1000s)",
     main = "Air passenger numbers from 1949 to 1960")
# add a straight line to show linerarity of the data
abline(reg =  lm(air_passengers~ time(air_passengers)))

# Examine trend in the data
plot(aggregate(air_passengers, FUN = mean))

# Seansonality 
boxplot(air_passengers ~ cycle(air_passengers), 
        xlab = "Date", ylab = "Passenger number(1000's)",
        main = "Monthly air passenger bob plot from 1949 to 1960")


seasonal_decomposition <- stl(air_passengers, s.window = "period")
plot(seasonal_decomposition)

# Test the stationarity of the time series object

# adf test
# H0 = time series is non-stationary (Null Hypothesis)
# H1 = time series is stationary

library(tseries)
adf.test(air_passengers)

# METHOD 2
Acf(air_passengers)
Pacf(air_passengers)

log_air_passengers <- log(air_passengers)
plot(log_air_passengers)

opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2))
plot(air_passengers)
plot(log_air_passengers)
par(opar)

adf.test(log_air_passengers, alternative = "Stationary")

acf(log_air_passengers)

pcfa(diff_air_passangers)

