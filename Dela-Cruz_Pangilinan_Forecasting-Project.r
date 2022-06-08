#AMAT132-Introductory to Forecasting

#Name: Bhea M. Dela Cruz & Myrene Pangilinan

#Objective : Forecasting 30 days sales for JRL Water




#calling necessary library for forecast
library(forecast)
library(tseries)
library(zoo)
library(ggplot2)


# Importing the data file
library(readxl)
sales <- read_excel("C:/Users/Gladys/Desktop/FORECASTING PROJECT- FILES/JRL DAILY SALES_data.xlsx")
View(sales)
head(sales)


#Converting data into time series
sales.ts<-ts(sales$Sales, start=1)


#Plotting the data
plot(sales.ts, xlab="Day", ylab="Sales", main = "Sales of JRL Water for 182 consecutive days")


#Using linear interpolation for missing values
sales2<-na.interp(sales.ts)

#Plotting the interpolated with the original time series
autoplot(sales2, series="Interpolated") + autolayer(sales.ts, series="Original") + scale_colour_manual(values=c(`Interpolated`="red",`Original`="gray")) + xlab("Day")+ylab("Sales")+ ggtitle("Interpolated Time Series")


#We now plot the ACF and PACF
plot(acf(sales2),main = "ACF plot")
plot(pacf(sales2),main="PACF plot")


#Test the Stationarity
adf.test(sales2)

#We can see that the p-value is not greater than 0.01 
#Time series can be stationary

#We can still check the number of differencing required
ndiffs(sales2)

#ndiff returns 1

#differencing
sales_d1<-diff(sales2,differences = 1)
adf.test(sales_d1)

plot(acf(sales_d1), main="Differenced ACF")
plot(pacf(sales_d1), main="Differenced PACF")

#The difference time series shows more outlier

#ARIMA is one of the best model for stationary time series

#We fit an ARIMA(2,0,0) as observed from the ACF plots
fit_mod1<-Arima(sales2,order=c(2,0,0))
fit_mod1


#We now fit an ARIMA model using auto.arima
fit_mod2<-auto.arima(sales2,ic="aic",trace=TRUE)

#Model fit by auto.arima() is ARIMA (3,1,3)
#Check AICc
fit_mod2


#ARIMA(3,1,3) has less AICc than ARIMA(2,0,0) 

#Check for residuals
checkresiduals(fit_mod2)

#Ljung Box Test
mod_resid=residuals(fit_mod2)
Box.test(mod_resid,lag=10, type="Ljung-Box")


#We choose ARIMA(3,1,3) as our model

#We will now use the model to forecast

JRLsales=forecast(fit_mod2, level=95, h=30)

JRLsales

#Plot forecast
plot(JRLsales, xlab="Day",ylab="Sales", main="30 days forecast for June 1 to 20,2022")

#[END CODE]












