setwd("D:\\Work\\Training\\Ivy Training\\Time Series Forecasting")
data<-read.csv("TractorSales.csv")

data<-ts(data[,2],start = c(2003,1),frequency = 12)

plot(data, xlab="Years", ylab = "Tractor Sales")
data

#Step 1: lag analysis
lag.plot(data, lags=9, do.lines=FALSE)
acf(data)
pacf(data)

#Step 2: Difference data to make data stationary on mean (remove trend)
plot(diff(data),ylab="Differenced Tractor Sales")



#Step 3: log transform data to make data stationary on variance
plot(log10(data),ylab="Log (Tractor Sales)")


#first check if the series staionar or not. KPSS is an improved version of ADF. 
#KPSS test if significant then it means the series is non stationary
#KPSS pvalue is small or close to 0 then it means the series is non stationary
#KPSS pvalue if large then it means the series is stationary
#We want the series to be staionary, so higher p values of KPSS is always preferred
#Here we see that the p value is almost equal to 0 which means the series is  non staionary and it needs DIFFRENCING
#inADF it is other way round so if p value in ADF test is small the series is STAIONARY

kpss.test(log10(data))
#find the diffrence order
ns <- nsdiffs(log10(data))
if(ns > 0) {
  data_star <- diff(log10(data),lag=frequency(log10(data)),differences=ns)
} else {
  data_star <- log10(data)
}
nd <- ndiffs(log10(data))
if(nd > 0) {
  data_star <- diff(data_star,differences=nd)
}



#Step 4: Difference log transform data to make data stationary on both mean and variance
plot(diff(log10(data)),ylab="Differenced Log (Tractor Sales)")


#Step 5: Plot ACF and PACF to identify potential AR and MA model
acf(ts(diff(log10(data))),main="ACF Tractor Sales")
pacf(ts(diff(log10(data))),main="PACF Tractor Sales")


#Step 6: Identification of best fit ARIMA model
ARIMAfit <- auto.arima(log10(data), approximation=FALSE,trace=FALSE)
summary(ARIMAfit)

#The best fit model is selected based on Akaike Information Criterion (AIC) , and 
#Bayesian Information Criterion (BIC) values. The idea is to choose a model with minimum
#AIC and BIC values.

#Step 6: Forecast sales using the best fit ARIMA model
pred <- predict(ARIMAfit, n.ahead = 36)
pred
plot(data,type="l",xlim=c(2004,2018),ylim=c(1,1600),xlab = "Year",ylab = "Tractor Sales")
lines(10^(pred$pred),col="blue")
lines(10^(pred$pred+2*pred$se),col="orange")
lines(10^(pred$pred-2*pred$se),col="orange")

#Step 7: Plot ACF and PACF for residuals of ARIMA model to ensure no more information is left for extraction
acf(ts(fit$residuals),main="ACF Residual")
pacf(ts(fit$residuals),main="PACF Residual")
