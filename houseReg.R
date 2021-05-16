
df = read.csv("C:\\Users\\sachi\\Downloads\\NCI\\Statics\\CA2\\NewHouseRegistrations_Ireland.csv", header = TRUE)

library(fpp2)



summary(df)



library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('forecast')
library('TSA')
library('tseries')
library('caret')
new_hs_reg <- ts(df$NewHouseRegistrations, frequency = 1, start = c(1978))
new_hs_reg
summary(new_hs_reg)
plot.ts(new_hs_reg)

#SES model - It has flat forecast funciton
ses_house_reg <- ses(new_hs_reg, h = 4)
round(accuracy(ses_house_reg),2)
autoplot(ses_house_reg)
autoplot(ses_house_reg)+autolayer(fitted(ses_house_reg), series = "Fitted")
summary(ses_house_reg)


#Holt
new_hs_reg_holt <- window(new_hs_reg, start = 1978 )
new_hs_reg_holt
plot(new_hs_reg_holt)
reg_house_holt <- holt(new_hs_reg_holt, h = 5)
reg_house_holt
summary(reg_house_holt)
with_damo <- holt(new_hs_reg_holt,damped = FALSE, PI=FALSE, h = 10)
without_damo <- holt(new_hs_reg_holt,damped = TRUE, PI=FALSE, h = 10)
autoplot(new_hs_reg_holt)+autolayer(with_damo, series = "With damp")+autolayer(without_damo, series = "Without damp")
#Holt with ets
ets_holt_damp <- ets(new_hs_reg, model="ZZN", damped = TRUE)
summary(ets_holt_damp)
ets_holt_no_damp <- ets(new_hs_reg_holt, model="ZZN", damped = FALSE)
autoplot(new_hs_reg_holt)+autolayer(ets_holt_damp$fitted, series = "With damp")+autolayer(ets_holt_no_damp$fitted, series = "Without damp ets")+autolayer(forecast(ets_holt_damp, h = 10))+autolayer(forecast(ets_holt_no_damp, h = 10))
forecast(ets_holt_damp, h = 10)
forecast(ets_holt_no_damp, h = 10)
summary(ets_holt_no_damp)
?ets

#Arima
ggtsdisplay(new_hs_reg)
#The graphs suggest differencing of the data before applying ARMA models.
ndiffs(new_hs_reg)
kpss.test(new_hs_reg, null = 'Trend')
#adf.test(new_hs_reg)
#?adf.test
diff_ts <- diff(diff(diff(new_hs_reg)))
ggtsdisplay(diff_ts)
acf(new_hs_reg)
pacf(new_hs_reg)
#AR I MA - p d q
fit <- Arima(new_hs_reg, c(2,0,4))
fit
qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals, type = 'Ljung-Box')
checkresiduals(fit)
summary(fit)

fcast <- forecast(fit, h=8)
fcast
plot(fcast)
fcast


#Auto Arima
auto_arima_fit <- auto.arima(new_hs_reg)
qqnorm(auto_arima_fit$residuals)
qqline(auto_arima_fit$residuals)
Box.test(auto_arima_fit$residuals, type = 'Ljung-Box')
checkresiduals(auto_arima_fit)
summary(auto_arima_fit)
fcast_auto <- forecast(auto_arima_fit, h=8)
fcast_auto
plot(fcast_auto)
accuracy(auto_arima_fit)
fcast_auto
acf(new_hs_reg)
pacf(new_hs_reg)
?auto.arima

