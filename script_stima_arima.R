if (!require("forecast")) install.packages("forecast")
library(forecast)

data<-data_store6
ogg<-"traffic"


time_series<-ts(data[[ogg]],start = 1, frequency = 1)
time_ser<-ts(data[["qty"]],start =1, frequency = 1)
plot(time_series,xlab = "week",ylab = col)
fit <- tslm(time_series ~ trend)
lines(fitted(fit),col="blue")


time_series<-ts(data[[ogg]],start = 1, frequency = 1)
auto.arima(time_series)

fit <- Arima(time_series, order=c(1,0,0))
fit
tsdisplay(residuals(fit))
Box.test(residuals(fit), lag=36, fitdf=6, type="Ljung")


plot(forecast(fit))

ccf(time_series,time_ser)
