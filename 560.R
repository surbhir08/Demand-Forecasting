#rm(list=ls())
library(TSA)
library(tseries)
library(tidyverse)
#library(MASS)
#library(forecast)
library(corrplot)

walmart_df <-read.csv( "Walmart.csv")
attach(walmart_df)
str(walmart_df)
colnames(walmart_df)
head(walmart_df)

par(mfrow=c(3,2))
plot(Temperature, Weekly_Sales)
plot(Fuel_Price, Weekly_Sales)
plot(CPI, Weekly_Sales)
plot(Unemployment, Weekly_Sales)
plot(Holiday_Flag, Weekly_Sales)
plot(Store, Weekly_Sales)

# correlation score
cor(Temperature, Weekly_Sales)
# [1] -0.06381001 very weak negative correlation between temperature and weekly sales
# not a strong linear relationship between these two variables
# more sales when temp is lower

cor(Fuel_Price, Weekly_Sales)
# [1] 0.009463786 
# lower fuel price leading to more sales

cor(CPI,Weekly_Sales)
# [1] -0.07263416
# ranges of CPI have similar sales distributions

cor(Unemployment, Weekly_Sales)
# [1] -0.1061761
# lower unemployment rate has higher weekly sales.

cor(Holiday_Flag, Weekly_Sales)
#  0.03689097

cor(Store, Weekly_Sales)

par(mfrow=c(3,3))
hist(Weekly_Sales)
hist(Temperature)
hist(Fuel_Price)
hist(CPI)
hist(Unemployment)
hist(Holiday_Flag)
hist(Store)
# left tail - exp
# right tail - log

# Log transformation on required variables for normality
par(mfrow=c(3,2))
hist(log(Weekly_Sales))
hist(log(Temperature))
hist(log(Fuel_Price))
hist(log(CPI))
hist(log(Unemployment))

dev.off()
walmart_df$as.date <- as.Date(Date, format = "%d-%m-%Y")

# plotting weekly sales over time
sales_by_date <- walmart_df %>%
  group_by(as.date) %>%
  summarize(total_sales = sum(log(Weekly_Sales)))
plot(sales_by_date, type='l')
par(mfrow=c(2,1))
acf(log(Weekly_Sales), 100)
pacf(log(Weekly_Sales), 100)
#Hints of yearly and monthly seasonality are observed in the Time-Series.

# plotting weekly fuel_procing over time
fuel_by_date <- walmart_df %>%
  group_by(as.date) %>%
  summarize(fuel_price = mean(Fuel_Price))
plot(fuel_by_date, type='l')


# plotting weekly fuel_procing over time
unemp_by_date <- walmart_df %>%
  group_by(as.date) %>%
  summarize(Unemployment = mean(Unemployment))
plot(unemp_by_date, type='l')


y = log(Weekly_Sales)
unem = log(Unemployment)
fuel = log(Fuel_Price)

# linear regression model with predictors
lm_mod = lm(y~ unem+ Holiday_Flag+
              CPI+ fuel+ Temperature)
summary(lm_mod)

# resid should be normally distributed
par(mfrow=c(2,2))
plot(lm_mod$residuals, col="blue")
hist(lm_mod$residuals)
acf(lm_mod$residuals, 100)
pacf(lm_mod$residuals, 100)
plot(lm_mod$fitted.values, color = "red")
# reason: seasonality 
# significant predictors: Unemployment, CPI, Temperature
# not significant : Holiday_Flag, Fuel_Price

# MODEL-BUILDING STRATEGY
# a. model specification (or identification)
walmart_df = walmart_df[order(walmart_df$as.date),] # ascending order

# conversion to time series object
min(walmart_df$as.date)
max(walmart_df$as.date)

sales.ts = ts(log(Weekly_Sales), start = c(2010, 2), end = c(2012, 10), 
              frequency = 52)
temp.ts = ts(Temperature, start = c(2010, 2), end = c(2012, 10), 
             frequency = 52)
cpi.ts = ts(CPI, start = c(2010, 2), end = c(2012, 10), 
            frequency = 52)
unemp.ts = ts(log(Unemployment), start = c(2010, 2), end = c(2012, 10), 
              frequency = 52)
fuel.ts = ts(log(Fuel_Price), start = c(2010, 2), end = c(2012, 10), 
             frequency = 52)

par(mfrow=c(3,1))
plot(sales.ts)
plot(temp.ts)
plot(cpi.ts)
plot(unemp.ts)
plot(fuel.ts)

ts.df <- data.frame(w_sales = sales.ts, 
                    temp = temp.ts, 
                    fuel_p = fuel.ts, 
                    unemployment = unemp.ts,
                    cpi = cpi.ts)

head(ts.df)
str(ts.df)
cor(ts.df)

# train and test data
set.seed(123)  
train_index <- 90
n_total <- nrow(ts.df) #113
train_data <- ts.df[1:(train_index), ]
test_data <- ts.df[(train_index+1):n_total, ]

cor_matrix <- cor(train_data[, -1])
dev.off()
corrplot(cor_matrix, method = "color")

#ts.df$cpi <- NULL
#ts.df$fuel_p <- NULL

plot(ts.df$w_sales)

adf.test(ts.df$w_sales)
kpss.test(ts.df$w_sales)

# is stationary
# The basic idea of stationarity is that the probability laws that govern the behavior of the process do not change over time. In a sense, the process is in statistical equilibrium.
# constant mean and variance over time
# covariance independent of time
par(mfrow=c(2,2))
acf(ts.df$w_sales, 100)
pacf(ts.df$w_sales, 100)
hist(ts.df$w_sales)
qqnorm(ts.df$w_sales)
qqline(ts.df$w_sales, col = 2)
shapiro.test(ts.df$w_sales)
jarque.bera.test(ts.df$w_sales)

xreg_matrix <- as.matrix(train_data[, -1])
head(xreg_matrix)
###Autoarima analysis
?auto.arima
a_arima = auto.arima(train_data$w_sales, max.p = 5,
                     max.q = 5, max.P = 4,
                     max.Q = 4, stationary = TRUE,
                     seasonal = TRUE, test = c("kpss", "adf", "pp"),
                     xreg = xreg_matrix)
summary(a_arima) # best 1,0,2
plot(residuals(a_arima))

# custom fun for best seasonal arima
fit.best.arima = function(y, maxord=c(1,1,1,1,1,1)){ 
  best.aic = Inf
  n        = length(y)
  for(p in 0:maxord[1]){  # loop for different AR orders up to max
    for(d in 0:maxord[2]){ # loop for different I orders up to max
      for(q in 0:maxord[3]){ # loop for different MA orders up to max
        for(P in 0:maxord[4]){ # loop for different seasonal AR orders up to max
          for(D in 0:maxord[5]){ # loop for different seasona I orders up to max
            for(Q in 0:maxord[6]){ # loop for different seasonal MA orders up to max
              # fit a model for each specification
              mod = arima(y, order=c(p,d,q),seas=list(order=c(P,D,Q),frequency(y)),method="CSS")
              # compute the model selection criterion, in this case the "consistent AIC"
              fit.aic = -2*mod$loglik + (log(n)+1)*length(mod$coef)
              
              if(fit.aic<best.aic){ # save the current best model
                best.aic=fit.aic 
                best.fit = mod
                best.model= c(p,d,q,P,D,Q)}
            }
          }
        }}}}
  list(best.aic,best.fit,best.model) # return the best model after evaluating them all
}

fit.best.arima(train_data$w_sales, c(2,1,2,2,1,2))
# best 1,0,1+ 2,0,2


# mod1
best.fit.sales = arima(train_data$w_sales, order=c(1,0,1), 
                       seasonal=list(order=c(2,0,2),
                                     frequency(train_data$w_sales)),
                       xreg = xreg_matrix, method="CSS")

tsdiag(best.fit.sales,tol=.15,gof.lag=24)
plot(residuals(best.fit.sales))
hist(residuals(best.fit.sales))
acf(residuals(best.fit.sales),120)
pacf(residuals(best.fit.sales),120)
Box.test(resid,type='Ljung-Box',lag=1)
?arima()

#mod2
best.fit.sales1 = arima(train_data$w_sales, order=c(1,0,2), 
                        seasonal=list(order=c(2,0,2),
                                      frequency(train_data$w_sales)),
                        xreg = xreg_matrix, method="CSS")

tsdiag(best.fit.sales1,tol=.15,gof.lag=24)
plot(residuals(best.fit.sales1))
hist(residuals(best.fit.sales1))
acf(residuals(best.fit.sales1),120)
pacf(residuals(best.fit.sales1),120)
Box.test(resid,type='Ljung-Box',lag=1)
dev.off()
plot(residuals(best.fit.sales), col="green")
lines(residuals(best.fit.sales1), col="red")

# detect outliers
detectAO(best.fit.sales)
detectIO(best.fit.sales)

detectAO(best.fit.sales1)
detectIO(best.fit.sales1)
?detectIO
xreg_matrix_test <- as.matrix(test_data[, -1])

#prediction = predict(best.fit.sales, newxreg = xreg_matrix_test)
#plot(prediction$pred,col="red")

###########################################################################################

obj = plot(best.fit.sales1,col="red", type='b',n.ahead=23, newxreg = xreg_matrix_test)
names(obj)

#obj=plot(best.fit.elec,col="red", type='b',n.ahead=24)
par(mfrow=c(1,2))
# We now compare the data we left out with our forecasted values
plot(seq(91,113),col="blue", test_data$w_sales, type="l",xlab="Time", ylab="log(weekly_sales)")
lines(seq(91,113),as.numeric(obj$pred),col="red")
lines(seq(91,113),as.numeric(obj$lpi),col="red",lty=2)
lines(seq(91,113),as.numeric(obj$upi),col="red",lty=2)
legend("topleft", c("data", "forecast", "Forecast Interval"), col=c("blue","red","red"), lty=c(1,1,2))

RMSFE = sqrt(mean(test_data$w_sales-as.numeric(obj$pred))^2)
exp(RMSFE)


obj2 = plot(best.fit.sales,col="red", type='b',n.ahead=23, newxreg = xreg_matrix_test)
names(obj)

#obj=plot(best.fit.elec,col="red", type='b',n.ahead=24)
par(mfrow=c(1,2))
# We now compare the data we left out with our forecasted values
plot(seq(91,113),col="blue", test_data$w_sales, type="l",xlab="Time", ylab="log(weekly_sales)")
lines(seq(91,113),as.numeric(obj2$pred),col="red")
lines(seq(91,113),as.numeric(obj2$lpi),col="red",lty=2)
lines(seq(91,113),as.numeric(obj2$upi),col="red",lty=2)
legend("topleft", c("data", "forecast", "Forecast Interval"), col=c("blue","red","red"), lty=c(1,1,2))

RMSFE = sqrt(mean(test_data$w_sales-as.numeric(obj2$pred))^2)
exp(RMSFE)
####################################################################################
# ccf trial

ts.df <- data.frame(w_sales = sales.ts, 
                    unemployment = unemp.ts)

set.seed(123)  
train_index <- 90
n_total <- nrow(ts.df) #113
train_data <- ts.df[1:(train_index), ]
test_data <- ts.df[(train_index+1):n_total, ]
xreg_matrix <- as.matrix(train_data[, -1])
head(xreg_matrix)

xreg_matrix_test <- as.matrix(test_data[, -1])

mod_sales_uemp = arima(train_data$w_sales, order=c(1,0,1), 
                        seasonal=list(order=c(2,0,2),
                                      frequency(train_data$w_sales)),
                        xreg = xreg_matrix, method="CSS")

sales = sales.ts
sales_test = sales.ts
unemployment = unemp.ts
unemployment_test = unemp.ts
typeof(sales.ts)
sales_uemployment= ts.intersect(sales,unemployment)
par(mfrow=c(1,2))
plot(sales_uemployment,yax.flip=TRUE,main='')
ccf(as.numeric(sales_uemployment[,1]),as.numeric(sales_uemployment[,2]),
    main='sales & unemployment',ylab='CCF')
# pre whitening
prewhitened_sales = prewhiten(as.numeric(sales_uemployment[,1]),
                              as.numeric(sales_uemployment[,2]),
          ylab='CCF' )

# Extract the residuals (prewhitened series)
residuals_sales <- prewhitened_sales$ccf
# Example: Fit an ARIMA model to the prewhitened series

ccf = as.numeric(unlist(residuals_sales))
mod_sales_uemp_prewhiten = arima(as.numeric(unlist(residuals_sales)), order=c(1,0,1), 
                       seasonal=list(order=c(2,0,2), frequency= sales.ts,
                       method="CSS"))
# View ARIMA model summary
summary(mod_sales_uemp_prewhiten)
as.matrix
dev.off()
obj_prewhiten = plot(mod_sales_uemp_prewhiten,col="red", type='b',n.ahead=23)
names(obj_prewhiten)




