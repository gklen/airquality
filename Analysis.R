#Greg Kelly
#Graphing and analysing the dublin air quality data and the HIPE data

library(readr)
library(tidyverse)
library(caret)
library(lubridate)
library(reshape2)
library(data.table)
library(forecast) #time series
library(mgcv) #geralised additive models with integrated smoothness estimation
library(gam)
library(car)

data = read_csv("Analysis/Dublin_all_data_MMM_SuitableForAnalysis.csv")

#quick look at all the data
#meltdf <- melt(data,id="Date")
#ggplot(meltdf,aes(x= Date,y=value,colour=variable,group=variable)) + geom_line()

#no data for the years 1999 and 2000 going to use data from 2001 on
data = data[data[["Date"]] >= "2001-01-01" , ]

#remove the minimum value columns
data$CO_min = NULL
data$NO_min = NULL
data$NO2_min = NULL
data$NOx_min = NULL
data$SO2_min = NULL
data$ozone_min = NULL

#data valid for the dates PM25 data became available
data_PM25 = data[data[["Date"]] >= "2009-01-01" , ]

#data for time series
data_ts = data

#boxplot and summary of pollutant data
data2 = subset(data_PM25, select = c(PM10, PM25, NO_mean, NO2_mean, SO2_mean, ozone_mean, CO_mean))
boxplot(data2, main = "Pollutant Boxplot", ylab = "Concentration ug/m3", xlab = "Pollutant")
summary(data2)

boxplot(data$CO_mean, main = "Carbon Monoxide Boxplot", ylab = "Concentration mg/m3")
summary(data$CO_mean)



#Time series -----
#aggregate data into monthly data for time series to decrease the processing time
monthly = aggregate(data_ts[names(data_ts)!='Date'], list(hour=cut(data_ts$Date,'month')), mean, na.rm=T)
data_ts = monthly

monthly = aggregate(data_PM25[names(data_PM25)!='Date'], list(hour=cut(data_PM25$Date,'month')), mean, na.rm=T)
monthly_PM25 = monthly


#Pollutant Time series-the means
#PM10
summary(data_ts$PM10)
boxplot(data_ts)
plot(decompose(ts(data_ts$PM10, frequency = 12, start=c(2001,1))))

PM10_arima = auto.arima(data_ts$PM10, stepwise=FALSE, approximation=FALSE, seasonal = TRUE, D=1)
PM10_arima

#https://otexts.org/fpp2/arima-r.html check model is is stationary and invertible by checking the rooks of the polynomials.
#dots in the circles show that the model is good for forecasting
autoplot(PM10_arima)

#95% confidence interval forecast time series. D=1 sets it to seasonal ARIMA, h is how many periods of forecasting
PM10_arima = forecast(auto.arima(ts(data_ts$PM10, frequency=12, start=c(2001,1)), stepwise=FALSE, approximation=FALSE), h=36, level=c(95))
plot(PM10_arima, main = "PM10 Time Series Forecast", ylab = "PM10 ug/m3", xlab = "Year")

#check residuals
checkresiduals(PM10_arima)

#Ljung-box test
Box.test(PM10_arima$residuals, lag=20, type="Ljung-Box")

?auto.arima

#check graph accuracy, mean error (ME), root mean squared error (RMSE), mean absolute error (MAE), 
#mean percentage error (MPE), mean absolute percentage error (MAPE), mean absolute scaled error (MASE) 
#first-order autocorrelation coefficient (ACF1)
#accuracy(PM10_arima)
summary(PM10_arima)

PM10_arima = as.ts(PM10_arima)
plot(decompose(PM10_arima))
?decompose






#PM25
PM25_arima = ts(monthly_PM25$PM25, frequency = 12,start=c(2009,1))
plot(decompose(PM25_arima))

PM25_arima = auto.arima(ts(monthly_PM25$PM25), stepwise=FALSE, approximation=FALSE)
PM25_arima

#https://otexts.org/fpp2/arima-r.html check model is is stationary and invertible by checking the rooks of the polynomials.
#dots in the circles show that the model is good for forecasting
autoplot(PM25_arima)

#95% confidence interval forecast time series. D=1 sets it to seasonal ARIMA, h is how many periods of forecasting
PM25_arima = forecast(auto.arima(ts(monthly_PM25$PM25, frequency=12, start=c(2009,1)), stepwise=FALSE, approximation=FALSE), h=36, level=c(95))
plot(PM25_arima, main = "PM2.5 Time Series Forecast", ylab = "PM2.5 ug/m3", xlab = "Year")

#Ljung-box test
Box.test(PM25_arima$residuals, lag=20, type="Ljung-Box")

#check residuals
checkresiduals(PM25_arima)

summary(PM25_arima)

PM25_arima = as.ts(PM25_arima)
plot(decompose(PM25_arima))




#NO2_mean
NO2_arima = ts(data_ts$NO2_mean, frequency = 12, start=c(2001,1))
plot(decompose(NO2_arima))

NO2_arima = auto.arima(ts(data_ts$NO2_mean), stepwise=FALSE, approximation=FALSE)
NO2_arima

#https://otexts.org/fpp2/arima-r.html check model is is stationary and invertible by checking the rooks of the polyNO2mials.
#dots in the circles show that the model is good for forecasting
autoplot(NO2_arima, main = "NO2 Inverse Characteristic Roots")

#95% confidence interval forecast time series. D=1 sets it to seasonal ARIMA, h is how many periods of forecasting
NO2_arima = forecast(auto.arima(ts(data_ts$NO2_mean, frequency=12, start=c(2001,1)), stepwise=FALSE, approximation=FALSE), h=36, level=c(95))
plot(NO2_arima, main = "Nitrogen Dioxide Time Series Forecast", ylab = "NO2 ug/m3", xlab = "Year")

#Ljung-box test
Box.test(NO2_arima$residuals, lag=20, type="Ljung-Box")

#check residuals
checkresiduals(NO2_arima)

summary(NO2_arima)

NO2_arima = as.ts(NO2_arima)
plot(decompose(NO2_arima))





#NO_mean
#NO
NO_arima = ts(data_ts$NO_mean, frequency = 12)
plot(decompose(NO_arima))

NO_arima = auto.arima(ts(data_ts$NO_mean), stepwise=FALSE, approximation=FALSE)
NO_arima

#https://otexts.org/fpp2/arima-r.html check model is is stationary and invertible by checking the rooks of the polynomials.
#dots in the circles show that the model is good for forecasting
autoplot(NO_arima)

#95% confidence interval forecast time series. D=1 sets it to seasonal ARIMA, h is how many periods of forecasting
NO_arima = forecast(auto.arima(ts(data_ts$NO_mean, frequency=12, start=c(2001,1)), stepwise=FALSE, approximation=FALSE), h=36, level=c(95))
plot(NO_arima, main = "Nitrogen Oxide Time Series Forecast", ylab = "NO ug/m3", xlab = "Year")

#Ljung-box test
Box.test(NO_arima$residuals, lag=20, type="Ljung-Box")

#check residuals
checkresiduals(NO_arima)

summary(NO_arima)

NO_arima = as.ts(NO_arima)
plot(decompose(NO_arima))




#CO_mean
library(imputeTS)
CO_arima = as.data.frame(data_ts$CO_mean)

#as there was a lot of data missing for the first 2 years these rows were removed and the time series plot was plotted from 2003 onwards
CO_arima = CO_arima[-c(1:24),]

CO_arima = ts(CO_arima, frequency = 12, start=c(2003,1))
plot(decompose(CO_arima))

CO_arima = auto.arima(ts(CO_arima), stepwise=FALSE, approximation=FALSE)
CO_arima

#https://otexts.org/fpp2/arima-r.html check model is is stationary and invertible by checking the roots of the polynomials.
#dots in the circles show that the model is good for forecasting
autoplot(CO_arima)

#95% confidence interval forecast time series. D=1 sets it to seasonal ARIMA, h is how many periods of forecasting
CO_arima = forecast(auto.arima(ts(CO_arima, frequency=12, start=c(2001,1)), stepwise=FALSE, approximation=FALSE), h=36, level=c(95))
plot(CO_arima)
plot(CO_arima, main = "Carbon Monoxide Time Series Forecast", ylab = "CO ug/m3", xlab = "Year")


#Ljung-box test
Box.test(CO_arima$residuals, lag=20, type="Ljung-Box")

#check residuals
checkresiduals(CO_arima)

summary(CO_arima)

CO_arima = as.ts(CO_arima)
plot(decompose(CO_arima))



dfc_mean = subset(dfc, select = c(PM10, NO2_mean, CO_mean, NO_mean, ozone_mean, SO2_mean, Cardiovascular_Cases))

#ozone
test = subset(data_ts, select = c(ozone_mean, hour))
test$hour = as.Date(test$hour, format = "%Y-%m-%d")

ozone_arima = ts(data_ts$ozone_mean, frequency = 12)
plot(decompose(ozone_arima))

ozone_arima = auto.arima(ts(data_ts$ozone_mean), stepwise=FALSE, approximation=FALSE)
ozone_arima

#https://otexts.org/fpp2/arima-r.html check model is is stationary and invertible by checking the roots of the polyozonemials.
#dots in the circles show that the model is good for forecasting
autoplot(ozone_arima)

#95% confidence interval forecast time series. D=1 sets it to seasonal ARIMA, h is how many periods of forecasting
ozone_arima = forecast(auto.arima(ts(data_ts$ozone_mean, frequency=12, start=c(2001,1)), stepwise=FALSE, approximation=FALSE), h=36, level=c(95))
plot(ozone_arima, main = "Ozone Time Series Forecast", ylab = "ozone ug/m3", xlab = "Year")
plot(ozone_arima)

#Ljung-box test
Box.test(ozone_arima$residuals, lag=20, type="Ljung-Box")

#check residuals
checkresiduals(ozone_arima)

summary(ozone_arima)

ozone_arima = as.ts(ozone_arima)
plot(decompose(ozone_arima))

ggseasonplot(ozone_arima)
library(zoo)

# 
# ggplot()+
#   #labs(y = "Benzene ng/m3", x = "Time")+
#   #scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
#   #ggtitle("Benzene") + 
#   #theme( plot.title = element_text(size=16, hjust = 0.5, face="bold"),
#      #    axis.title.x = element_text(color="black", size=14, face="bold"),
#       #   axis.title.y = element_text(color="black", size=14, face="bold"))+
#   #geom_line(aes(y = test$ozone_mean, x = test$hour))+
#   autolayer(ozone_arima)
#   #scale_color_manual(values=c("blue"))+
#   #labs(colour = "")
# 






#SO2_mean
SO2_arima = as.data.frame(data_ts$SO2_mean)
SO2_arima = SO2_arima[-c(1:12),]

# SO2_arima = ts(SO2_arima, frequency = 12, start=c(2002,1))
# plot(decompose(SO2_arima))
# 
# SO2_arima = auto.arima(ts(SO2_arima), stepwise=FALSE, approximation=FALSE)
# SO2_arima

#https://otexts.org/fpp2/arima-r.html check model is is stationary and invertible by checking the rooks of the polySO2mials.
#dots in the circles show that the model is good for forecasting
autoplot(SO2_arima)

#95% SO2nfidence interval forecast time series. D=1 sets it to seasonal ARIMA, h is how many periods of forecasting
SO2_arima = forecast(auto.arima(ts(SO2_arima, frequency=12, start=c(2001,1)), stepwise=FALSE, approximation=FALSE), h=36, level=c(95))
plot(SO2_arima)
plot(SO2_arima, main = "Sulphur Dioxide Time Series Forecast", ylab = "SO2 ug/m3", xlab = "Year")

#Ljung-box test
Box.test(SO2_arima$residuals, lag=20, type="Ljung-Box")

#check residuals
checkresiduals(SO2_arima)

summary(SO2_arima)

SO2_arima = as.ts(SO2_arima)
plot(decompose(SO2_arima))



?auto.arima


#HEALTH TIME SERIES


#have to import HIPE data in the next section for this to work properly. getting monthly aggregated data for plotting a cleaner time series rather than using the daily data
monthly = aggregate(hipe2[names(hipe2)!='Date'], list(hour=cut(hipe2$Date,'month')), mean, na.rm=T)
hipe2 = monthly

summary(hipe2)
boxplot(hipe2$Cardiovascular_Cases, main = 'Cardiovascular Boxplot', ylab = "Patients")

#Cardiovascular_Cases
Cardiovascular_Cases_arima = ts(hipe2$Cardiovascular_Cases, frequency = 12, start=c(2007,1))

plot(decompose(Cardiovascular_Cases_arima))

Cardiovascular_Cases_arima = auto.arima(ts(hipe2$Cardiovascular_Cases), stepwise=FALSE, approximation=FALSE)
Cardiovascular_Cases_arima

#https://otexts.org/fpp2/arima-r.html check model is is stationary and invertible by checking the rooks of the polynomials.
#dots in the circles show that the model is good for forecasting
autoplot(Cardiovascular_Cases_arima)

#95% Cardiovascular_Casesnfidence interval forecast time series. D=1 sets it to seasonal ARIMA, h is how many periods of forecasting
Cardiovascular_Cases_arima = forecast(auto.arima(ts(hipe2$Cardiovascular_Cases, frequency = 12, start=c(2007,1)), D=1, stepwise=FALSE, approximation=FALSE), h=36, level=c(95))
plot(Cardiovascular_Cases_arima, main = "Cardiovascular Cases Time Series Forecast", ylab = "Patients", xlab = "Year")

#Ljung-box test
Box.test(Cardiovascular_Cases_arima$residuals, lag=20, type="Ljung-Box")

#check residuals
checkresiduals(Cardiovascular_Cases_arima)

summary(Cardiovascular_Cases_arima)

Cardiovascular_Cases_arima = as.ts(Cardiovascular_Cases_arima)
plot(decompose(Cardiovascular_Cases_arima))



#Respiratory_Cases
boxplot(hipe2$Respiratory_Cases, main = 'Respiratory Boxplot', ylab = "Patients")

Respiratory_Cases_arima = ts(hipe2$Respiratory_Cases, frequency = 12, start=c(2007,1))
plot(decompose(Respiratory_Cases_arima))

Respiratory_Cases_arima = auto.arima(ts(hipe2$Respiratory_Cases), stepwise=FALSE, approximation=FALSE)
Respiratory_Cases_arima

#https://otexts.org/fpp2/arima-r.html check model is is stationary and invertible by checking the rooks of the polynomials.
#dots in the circles show that the model is good for forecasting
autoplot(Respiratory_Cases_arima)

#95% Respiratory_Casesnfidence interval forecast time series. D=1 sets it to seasonal ARIMA, h is how many periods of forecasting
Respiratory_Cases_arima = forecast(auto.arima(ts(hipe2$Respiratory_Cases, frequency=12, start=c(2007,1)), stepwise=FALSE, approximation=FALSE), h=36, level=c(95))
plot(Respiratory_Cases_arima, main = "Respiratory Cases Time Series Forecast", ylab = "Patients", xlab = "Year")

#Ljung-box test
Box.test(Respiratory_Cases_arima$residuals, lag=20, type="Ljung-Box")

#check residuals
checkresiduals(Respiratory_Cases_arima)

summary(Respiratory_Cases_arima)

Respiratory_Cases_arima = as.ts(Respiratory_Cases_arima)
plot(decompose(Respiratory_Cases_arima))








#Graphing ------
#WHO guidelines, values to use when plotting 
#https://www.who.int/news-room/fact-sheets/detail/ambient-(outdoor)-air-quality-and-health
PM25_annual_mean_WHO = 10
PM25_daily_mean_WHO = 25
PM10_annual_mean_WHO = 20
PM10_daily_mean_WHO = 50
ozone_mean_WHO = 100
NO2_1hr_mean_WHO = 200
NO2_annual_mean_WHO = 40
SO2_10min_mean_WHO = 500
SO2_24hr_mean_WHO = 20

#EU guidelines, values to use when plotting 
#http://ec.europa.eu/environment/air/quality/standards.htm
#http://www.epa.ie/air/quality/standards/
CO_EU = 10 #mg, 1000 * ug
PM25_EU_daily = 25
PM25_EU_2020 = 20
SO2_EU_1hr = 350
SO2_EU_24hr = 125
SO2_annual_mean = 20 #protection of vegetation
NO2_EU_1hr = 200
NO2_EU_annual = 40
NO2_EU_daily = 50
PM10_EU_24hr = 50
PM10_EU_annual = 40
Lead_EU_annual = 0.5
Benzene_EU_annual = 5
Ozone_EU_daily = 120
Arsenic_EU_annual = 6 #ng ug/1000
Cadium_EU_annual = 5 #ng
Nickel_EU_annual = 20 #ng
PAH_EU_annual = 1 #ng



#euro standards
#dates retrieved from https://www.theaa.com/driving-advice/fuels-environment/euro-emissions-standards
euro4 = as.Date("2005/01/01")
euro5 = as.Date("2009/09/01")
euro6 = as.Date("2014/09/01")

#Plots pollution ggplot ------

#Plot different pollutants over time - BENZENE
ggplot(data, aes(Date, Benzene_mean))+
  labs(y = "Benzene ng/m3", x = "Time")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ggtitle("Benzene") + 
  theme( plot.title = element_text(size=16, hjust = 0.5, face="bold"),
  axis.title.x = element_text(color="black", size=14, face="bold"),
  axis.title.y = element_text(color="black", size=14, face="bold"))+
  geom_line(aes(y = Benzene_mean, colour = "Benzene"))+
  scale_color_manual(values=c("blue"))+
  labs(colour = "")


#CO mean
ggplot(data, aes(Date, CO_mean))+
  labs(y = "CO mg/m3", x = "Time")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ggtitle("Carbon Monoxide") + 
  theme( plot.title = element_text(size=16, hjust = 0.5, face="bold"),
         axis.title.x = element_text(color="black", size=14, face="bold"),
         axis.title.y = element_text(color="black", size=14, face="bold")) + 
  geom_hline(aes(yintercept= CO_EU, linetype = "EU Carbon Monoxide Level"), colour= 'blue')+
  geom_line(aes(y = CO_mean, colour = "Carbon Monoxide"))+
  scale_color_manual(values=c("blue"))+
  labs(colour = "")

#CO mean and max
ggplot(data, aes(Date, CO_mean))+
  labs(y = "CO mg/m3", x = "Time")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ggtitle("Carbon Monoxide") + 
  theme( plot.title = element_text(size=16, hjust = 0.5, face="bold"),
         axis.title.x = element_text(color="black", size=14, face="bold"),
         axis.title.y = element_text(color="black", size=14, face="bold")) + 
  geom_hline(aes(yintercept= CO_EU, linetype = "CO EU limit"), colour= 'blue')+
  #geom_line(aes(y = CO_max, colour = "Carbon Monoxide Max Daily Values"))+
  geom_line(aes(y = CO_mean, colour = "Carbon Monoxide Mean"))+
  scale_color_manual(values=c("red", "blue"))+
  labs(color = "Pollutant")


#CO smoothed conditional means and max
ggplot(data, aes(Date, CO_mean))+
  geom_smooth()+
  labs(y = "CO mg/m3", x = "Time")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ggtitle("Carbon Monoxide") + 
  theme( plot.title = element_text(size=16, hjust = 0.5, face="bold"),
         axis.title.x = element_text(color="black", size=14, face="bold"),
         axis.title.y = element_text(color="black", size=14, face="bold"))+
  geom_smooth(aes(y = CO_mean, colour = "CO"))+
  labs(color = "Pollutant")+
  geom_hline(aes(yintercept= CO_EU, linetype = "CO EU limit"), colour= 'blue')+
  geom_smooth(aes(y = CO_max, colour = "CO Max"))


#CO mean & max
#remove -inf values in the carbon column
data = do.call(data.frame,lapply(data, function(x) replace(x, is.infinite(x),NA)))

ggplot(data, aes(Date, CO_mean))+
  labs(y = "CO mg/m3", x = "Time")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ggtitle("Carbon Monoxide") + 
  theme( plot.title = element_text(size=16, hjust = 0.5, face="bold"),
         axis.title.x = element_text(color="black", size=14, face="bold"),
         axis.title.y = element_text(color="black", size=14, face="bold"))+ 
  geom_line(aes(y = CO_max, colour = "CO max"))+
  geom_line(aes(y = CO_mean, colour = "CO mean"))+
  labs(colour = "")+
  geom_hline(aes(yintercept= CO_EU, linetype = "CO EU daily Limit"), colour= 'blue')
  #stat_peaks(colour = "black") +  #library(ggpmisc)
  #stat_peaks(geom = "text", colour = "black", x.label.fmt = "%m")


#NO mean
ggplot(data, aes(Date, NO_mean))+
  geom_line()+
  labs(y = "NO ug/m3", x = "Time")+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  ggtitle("Nitrogen Monoxide") + 
  theme( plot.title = element_text(size=16, hjust = 0.5, face="bold"),
         axis.title.x = element_text(color="black", size=14, face="bold"),
         axis.title.y = element_text(color="black", size=14, face="bold"))

#NO mean and max
ggplot(data, aes(Date, NO_mean))+
  geom_line()+
  labs(y = "NO ug/m3", x = "Time")+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  ggtitle("Nitrogen Monoxide") + 
  theme( plot.title = element_text(size=16, hjust = 0.5, face="bold"),
         axis.title.x = element_text(color="black", size=14, face="bold"),
         axis.title.y = element_text(color="black", size=14, face="bold"))+
  geom_line(aes(y = NO_max, colour = "NO max"))+
  geom_line(aes(y = NO_mean, colour = "NO mean"))


#NO mean and max smoooth
ggplot(data, aes(Date, NO_mean))+
  geom_smooth()+
  labs(y = "NO ug/m3", x = "Time")+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  ggtitle("Nitrogen Monoxide") + 
  theme( plot.title = element_text(size=16, hjust = 0.5, face="bold"),
         axis.title.x = element_text(color="black", size=14, face="bold"),
         axis.title.y = element_text(color="black", size=14, face="bold"))+
  geom_smooth(aes(y = NO_max, colour = "NO max"))+
  geom_smooth(aes(y = NO_mean, colour = "NO mean"))



#NO2 mean
ggplot(data, aes(Date, NO2_mean))+
  geom_line()+
  labs(y = "NO2 ug/m3", x = "Time")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ggtitle("Nitrogen Dioxide") + 
  theme( plot.title = element_text(size=16, hjust = 0.5, face="bold"),
         axis.title.x = element_text(color="black", size=14, face="bold"),
         axis.title.y = element_text(color="black", size=14, face="bold")) + 
  geom_hline(aes(yintercept= NO2_annual_mean_WHO, linetype = "NO2 WHO annual mean"), colour= 'red')+
  geom_hline(aes(yintercept= NO2_EU_daily, linetype = "NO2 EU daily Limit"), colour= 'blue')+
  labs(colour = "")



#NO2 mean smooth
ggplot(data, aes(Date, NO2_mean))+
  geom_smooth()+
  labs(y = "NO2 ug/m3", x = "Time")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ggtitle("Nitrogen Dioxide") + 
  theme( plot.title = element_text(size=16, hjust = 0.5, face="bold"),
         axis.title.x = element_text(color="black", size=14, face="bold"),
         axis.title.y = element_text(color="black", size=14, face="bold")) + 
  geom_hline(aes(yintercept= NO2_annual_mean_WHO, linetype = "NO2 WHO annual mean"), colour= 'red')+
  geom_hline(aes(yintercept= NO2_EU_daily, linetype = "NO2 EU daily Limit"), colour= 'blue')+
  labs(colour = "")


#NOx mean
ggplot(data, aes(Date, NOx_mean))+
  geom_line()+
  labs(y = "NOx ug/m3", x = "Time")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ggtitle("Nitrogen Oxide") + 
  theme( plot.title = element_text(size=16, hjust = 0.5, face="bold"),
         axis.title.x = element_text(color="black", size=14, face="bold"),
         axis.title.y = element_text(color="black", size=14, face="bold"))

#NOx mean smooth
ggplot(data, aes(Date, NOx_mean))+
  geom_smooth()+
  labs(y = "NOx ug/m3", x = "Time")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ggtitle("Nitrogen Oxide") + 
  theme( plot.title = element_text(size=16, hjust = 0.5, face="bold"),
         axis.title.x = element_text(color="black", size=14, face="bold"),
         axis.title.y = element_text(color="black", size=14, face="bold"))+
  geom_smooth(aes(y = NOx_max, colour = "NOx max"))+
  geom_smooth(aes(y = NOx_mean, colour = "NOx mean"))




#NO, NO2, NOx combined LINE plot
ggplot(data, aes(Date, NO_mean))+
  geom_line()+
  labs(y = "NO, NO2, NOx ug/m3", x = "Time")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ggtitle("Nitrogen Monoxide") + 
  theme( plot.title = element_text(size=16, hjust = 0.5, face="bold"),
         axis.title.x = element_text(color="black", size=14, face="bold"),
         axis.title.y = element_text(color="black", size=14, face="bold"))+
  geom_line(aes(y = NOx_mean, colour = "NOx"))+
  geom_line(aes(y = NO2_mean, colour = "NO2"))+
  geom_line(aes(y = NO_mean, colour = "NO"))+
  geom_hline(yintercept = NO2_annual_mean_WHO, linetype = "dashed", colour = "red")+
  geom_hline(yintercept = NO2_EU_daily, linetype= "dashed", color = "black")+
  labs(colour = "Pollutant")+
  geom_vline(xintercept = euro4, linetype= "dashed", color = "orange")+
  geom_vline(xintercept = euro5, linetype= "dashed", color = "green")+
  geom_vline(xintercept = euro6, linetype= "dashed", color = "blue")
  # scale_linetype_manual(values = c(1, 3),
  #                       labels = c("Treatment one", "Treatment two", "orange")) +
  # scale_color_manual(values = c("black", "red", "orange"),
  #                    labels = c("Treatment one", "Treatment two", "orange"))




#NO, NO2, NOx combined Smooth line
N = ggplot(data, aes(Date, NO_mean))+
    geom_smooth()+
    labs(y = "NO, NO2, NOx ug/m3", x = "Time")+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    ggtitle("Nitrogen Monoxide") + 
    theme( plot.title = element_text(size=16, hjust = 0.5, face="bold"),
           axis.title.x = element_text(color="black", size=14, face="bold"),
           axis.title.y = element_text(color="black", size=14, face="bold"))+
    geom_smooth(aes(y = NOx_mean, colour = "NOx"))+
    geom_smooth(aes(y = NO2_mean, colour = "NO2"))+
    geom_smooth(aes(y = NO_mean, colour = "NO"))+
    labs(colour = "Pollutant")
N

#EU and WHO guidelines
N + geom_hline(aes(yintercept= NO2_annual_mean_WHO, linetype = "NO2 Annual Mean WHO"), colour= 'red') +
  geom_hline(aes(yintercept= NO2_EU_daily, linetype = "NO2 EU daily"), colour= 'blue') +
  scale_linetype_manual(name = "Recommended Limits", values = c(2, 2), 
                        guide = guide_legend(override.aes = list(color = c("red", "black"))))
  

#add euro emission start dates
N + geom_vline(aes(xintercept= euro6, linetype = "Euro6")) +
    geom_vline(aes(xintercept= euro5, linetype = "Euro5")) +
    geom_vline(aes(xintercept= euro4, linetype = "Euro4")) +
    scale_linetype_manual("Emissions Standards", values=c("Euro6" = 3, "Euro5" = 2 , "Euro4" = 1))


    
#SO2 mean
ggplot(data, aes(Date, SO2_mean))+
  geom_line()+
  labs(y = "SO2 ug/m3", x = "Time")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ggtitle("Suplhur Dioxide") + 
  theme( plot.title = element_text(size=16, hjust = 0.5, face="bold"),
         axis.title.x = element_text(color="black", size=14, face="bold"),
         axis.title.y = element_text(color="black", size=14, face="bold")) + 
  geom_hline(yintercept = SO2_24hr_mean_WHO, linetype="dashed", color = "red")+
  geom_hline(yintercept = SO2_EU_24hr, linetype="dashed", color = "red")+
  geom_line(aes(y = SO2_max, colour = "SO2 max"))+
  geom_line(aes(y = SO2_mean, colour = "SO2 mean"))


#SO2 mean smooth
ggplot(data, aes(Date, SO2_mean))+
  geom_smooth()+
  labs(y = "SO2 ug/m3", x = "Time")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ggtitle("Suplhur Dioxide") + 
  theme( plot.title = element_text(size=16, hjust = 0.5, face="bold"),
         axis.title.x = element_text(color="black", size=14, face="bold"),
         axis.title.y = element_text(color="black", size=14, face="bold")) + 
  geom_hline(aes(yintercept= SO2_24hr_mean_WHO, linetype = "WHO SO2 Daily Limit"), colour= 'red')+
  geom_hline(aes(yintercept= SO2_EU_24hr, linetype = "EU SO2 Daily Limit"), colour= 'blue')+
  labs(colour = "")+
  geom_smooth(aes(y = SO2_max, colour = "SO2 max"))+
  geom_smooth(aes(y = SO2_mean, colour = "SO2 mean"))


#PM10
ggplot(data, aes(Date, PM10))+
  geom_line()+
  labs(y = "PM10 ug/m3", x = "Time")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ggtitle("Particulate Matter 10 microns") + 
  theme( plot.title = element_text(size=16, hjust = 0.5, face="bold"),
         axis.title.x = element_text(color="black", size=14, face="bold"),
         axis.title.y = element_text(color="black", size=14, face="bold")) + 
  geom_hline(aes(yintercept= PM10_daily_mean_WHO, linetype = "EU & WHO PM10 Daily Limit"), colour= 'red')+
  labs(colour = "")

#PM10 smooth
ggplot(data, aes(Date, PM10))+
  geom_smooth()+
  labs(y = "PM10 ug/m3", x = "Time")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ggtitle("Particulate Matter 10 microns") + 
  theme( plot.title = element_text(size=16, hjust = 0.5, face="bold"),
         axis.title.x = element_text(color="black", size=14, face="bold"),
         axis.title.y = element_text(color="black", size=14, face="bold")) + 
  #geom_hline(aes(yintercept= PM10_daily_mean_WHO, linetype = "EU & WHO PM10 Daily Limit"), colour= 'red')+
  labs(colour = "")


#PM2.5 line
ggplot(data_PM25, aes(Date, PM25))+
  geom_line()+
  labs(y = "PM25 ug/m3", x = "Time")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ggtitle("Particulate Matter 2.5 microns") + 
  theme( plot.title = element_text(size=16, hjust = 0.5, face="bold"),
         axis.title.x = element_text(color="black", size=14, face="bold"),
         axis.title.y = element_text(color="black", size=14, face="bold")) + 
  geom_hline(aes(yintercept= PM25_EU_daily, linetype = "EU & WHO PM25 daily"), colour= 'red')+
  geom_hline(aes(yintercept= PM25_EU_2020, linetype = "EU PM25 2020 target"), colour= 'blue')+
  labs(colour = "")


#PM2.5 smooth
ggplot(data_PM25, aes(Date, PM25))+
  geom_smooth()+
  labs(y = "PM25 ug/m3", x = "Time")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ggtitle("Particulate Matter 2.5 microns") + 
  theme( plot.title = element_text(size=16, hjust = 0.5, face="bold"),
         axis.title.x = element_text(color="black", size=14, face="bold"),
         axis.title.y = element_text(color="black", size=14, face="bold")) + 
  geom_hline(aes(yintercept= PM25_EU_daily, linetype = "EU & WHO PM25 daily"), colour= 'blue')+
  geom_hline(aes(yintercept= PM25_EU_2020, linetype = "EU PM25 2020 target"), colour= 'red')+
  labs(colour = "")



#ozone
ggplot(data, aes(Date, ozone_mean))+
  geom_line()+
  labs(y = "Ozone ug/m3", x = "Time")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ggtitle("Ozone") + 
  theme( plot.title = element_text(size=16, hjust = 0.5, face="bold"),
         axis.title.x = element_text(color="black", size=14, face="bold"),
         axis.title.y = element_text(color="black", size=14, face="bold")) + 
  geom_hline(aes(yintercept= Ozone_EU_daily, linetype = "EU ozone daily"), colour= 'blue')+
  geom_hline(aes(yintercept= ozone_mean_WHO, linetype = "WHO ozone daily"), colour= 'red')+
  labs(colour = "")


#ozone smooth
ggplot(data, aes(Date, ozone_mean))+
  geom_smooth()+
  labs(y = "Ozone ug/m3", x = "Time")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ggtitle("Ozone") + 
  theme( plot.title = element_text(size=16, hjust = 0.5, face="bold"),
         axis.title.x = element_text(color="black", size=14, face="bold"),
         axis.title.y = element_text(color="black", size=14, face="bold")) + 
  geom_hline(aes(yintercept= Ozone_EU_daily, linetype = "EU ozone daily"), colour= 'blue')+
  geom_hline(aes(yintercept= ozone_mean_WHO, linetype = "WHO ozone daily"), colour= 'red')+
  labs(colour = "")










#HIPE DATA, Combining ------
#LOAD IN THE HIPE DATA AND COMBINE WITH THE POLLUTANT DATASET

HIPE <- read_csv("~/Documents/PhD/Data/DublinAQData/Analysis/HIPE.csv")

#boxplot and summary of unaltered data
hipe2 = HIPE
hipe2$Cardiovascular_Cases = as.numeric(hipe2$Cardiovascular_Cases )
hipe2$Respiratory_Cases = as.numeric(hipe2$Respiratory_Cases )

boxplot(hipe2$Cardiovascular_Cases, main = "Hospital In-patient Data", ylab = "Cardiovascular Cases")
boxplot(hipe2$Respiratory_Cases, main = "HIPE Hospital Inpatient Data", ylab = "Respiratory Cases")

summary(hipe2$Cardiovascular_Cases, main = "HIPE Hospital Inpatient Data", ylab = "Cardiovascular Cases")
summary(hipe2$Respiratory_Cases, main = "HIPE Hospital Inpatient Data", ylab = "Respiratory Cases")
summary(hipe2)
hist(hipe2$Cardiovascular_Cases)
hist(hipe2$Respiratory_Cases)

#remove the ~ symbol from the HIPE dataset and make the data numeric
#I was told the ~ symbol in the HIPE dataset was when there was 5 or less patients in a given day. This is done for confidentiality
#To ensure more data is available for analysis I changed all of the values to 3. The potential average of the number of patients, ie  (5+4+3+2+1)/5
HIPE$Cardiovascular_Cases = as.numeric(gsub("~", 3, HIPE$Cardiovascular_Cases))
HIPE$Respiratory_Cases = as.numeric(gsub("~", 3, HIPE$Respiratory_Cases))
HIPE$Cardiovascular_Cases = as.numeric(HIPE$Cardiovascular_Cases)
HIPE$Respiratory_Cases = as.numeric(HIPE$Respiratory_Cases)


#import the air pollution mean/max/min data for across Dublin
Dublin_air_quality <- read_csv("~/Documents/PhD/Data/DublinAQData/Analysis/Dublin_2007_2016_MMM_SuitableForAnalysis.csv")

#merge the dublin air pollutants and the HIPE data
df = merge(Dublin_air_quality, HIPE)


#add the average weather data for dublin
weather = read_csv("Analysis/Dublin_daily_avg_weather.csv")
dfw = merge(df, weather)



#PM25 data ------
data_PM25 = subset(data_PM25, select = c(Date, PM25, PM10 , NO2_mean , CO_mean, ozone_mean, SO2_mean))
weather_PM25 = weather[weather[["Date"]] >= "2009-01-01" , ]
data_PM25 = merge(data_PM25, weather_PM25)

#add HIPE data to PM25
HIPE2 = HIPE[HIPE[["Date"]] >= "2009-01-01" , ]

#merge pm25 and HIPE data. create datasets with 1 and 2 day lags
PM25 = merge(data_PM25, HIPE2)

PM25_1DayLag = PM25
PM25_1DayLag$Respiratory_Cases = shift(PM25_1DayLag$Respiratory_Cases, n=1, fill=NA, type="lag")
PM25_1DayLag$Cardiovascular_Cases = shift(PM25_1DayLag$Cardiovascular_Cases, n=1, fill=NA, type="lag")

PM25_2DayLag = PM25_1DayLag
PM25_2DayLag$Respiratory_Cases = shift(PM25_2DayLag$Respiratory_Cases, n=1, fill=NA, type="lag")
PM25_2DayLag$Cardiovascular_Cases = shift(PM25_2DayLag$Cardiovascular_Cases, n=1, fill=NA, type="lag")


#create 2 dataframes one for Respiratory cases, the other for Cardiovascular cases
dfc = df
dfr = df

dfc$Respiratory_Cases = NULL
dfr$Cardiovascular_Cases = NULL


#create a 1 and 2 day lag. high pollution from one or two days ago may have triggered a health problem
dfc_1DayLag = (transform(dfc, Cardiovascular_Cases = c(NA, Cardiovascular_Cases[-nrow(dfc)])))
dfc_2DayLag = transform(dfc_1DayLag, Cardiovascular_Cases = c(NA, Cardiovascular_Cases[-nrow(dfc_1DayLag)]))

dfr_1DayLag = (transform(dfr, Respiratory_Cases = c(NA, Respiratory_Cases[-nrow(dfr)])))
dfr_2DayLag = (transform(dfr_1DayLag, Respiratory_Cases = c(NA, Respiratory_Cases[-nrow(dfr_1DayLag)])))


#explore some relationships between the cardiovascular cases and the various pollutants
ggplot(dfc, aes(Cardiovascular_Cases, CO_max))+
  geom_line()+
  geom_point()

ggplot(dfc, aes(Cardiovascular_Cases, CO_mean))+
  geom_smooth()

ggplot(dfc, aes(Cardiovascular_Cases, NO_mean))+
  geom_smooth()+
  geom_point()

ggplot(dfc, aes(Cardiovascular_Cases, NO_max))+
  geom_smooth()+
  geom_point()

ggplot(dfc, aes(Cardiovascular_Cases, NO2_mean))+
  geom_smooth()+
  geom_point()
  

ggplot(dfc, aes(Cardiovascular_Cases, NO2_max))+
  geom_smooth()

ggplot(dfc, aes(Cardiovascular_Cases, NOx_mean))+
  geom_smooth()

ggplot(dfc, aes(Cardiovascular_Cases, NOx_max))+
  geom_smooth()

ggplot(dfc, aes(Cardiovascular_Cases, SO2_mean))+
  geom_smooth()

ggplot(dfc, aes(Cardiovascular_Cases, SO2_max))+
  geom_smooth()

ggplot(dfc, aes(Cardiovascular_Cases, PM10))+
  geom_smooth()

ggplot(dfc, aes(Cardiovascular_Cases, PM25))+
  geom_smooth()

ggplot(dfc, aes(Cardiovascular_Cases, ozone_mean))+
  geom_smooth()+
  geom_point()

ggplot(dfc, aes(Cardiovascular_Cases, ozone_max))+
  geom_smooth()+
  geom_point()





#explore some relationships between the respiratory cases and the various pollutants
ggplot(dfr, aes(Respiratory_Cases, CO_max))+
  geom_line()+
  geom_point()

ggplot(dfr, aes(Respiratory_Cases, CO_mean))+
  geom_smooth()

ggplot(dfr, aes(Respiratory_Cases, NO_mean))+
  geom_smooth()+
  geom_point()

ggplot(dfr, aes(Respiratory_Cases, NO_max))+
  geom_smooth()+
  geom_point()

ggplot(dfr, aes(Respiratory_Cases, NO2_mean))+
  geom_smooth()+
  geom_point()


ggplot(dfr, aes(Respiratory_Cases, NO2_max))+
  geom_smooth()

ggplot(dfr, aes(Respiratory_Cases, NOx_mean))+
  geom_smooth()

ggplot(dfr, aes(Respiratory_Cases, NOx_max))+
  geom_smooth()

ggplot(dfr, aes(Respiratory_Cases, SO2_mean))+
  geom_smooth()

ggplot(dfr, aes(Respiratory_Cases, SO2_max))+
  geom_smooth()

ggplot(dfr, aes(Respiratory_Cases, PM10))+
  geom_smooth()

ggplot(dfr, aes(Respiratory_Cases, PM25))+
  geom_smooth()+
  geom_point()

ggplot(dfr, aes(Respiratory_Cases, ozone_mean))+
  geom_smooth()+
  geom_point()

ggplot(dfr, aes(Respiratory_Cases, ozone_max))+
  geom_smooth()+
  geom_point()


#PM2.5
ggplot(PM25, aes(Respiratory_Cases, PM25))+
  geom_smooth()+
  geom_point()

ggplot(PM25, aes(Cardiovascular_Cases, PM25))+
  geom_smooth()+
  geom_point()



#Cleaning the data------

#count nas
map(dfc, ~sum(is.na(.)))
map(dfr, ~sum(is.na(.)))


#percentage of nas per column
apply(dfc, 2, function(col)sum(is.na(col))/length(col))
apply(dfr, 2, function(col)sum(is.na(col))/length(col))


#data is MCAR-missing completely at random
#remove columns that have more than 20% of the data missing
dfc = dfc[sapply(dfc, function(x) mean(is.na(x))) < 0.2]
dfc_1DayLag = dfc_1DayLag[sapply(dfc_1DayLag, function(x) mean(is.na(x))) < 0.2]
dfc_2DayLag = dfc_2DayLag[sapply(dfc_2DayLag, function(x) mean(is.na(x))) < 0.2]

#remove columns that have more than 20% of the data missing
dfr = dfr[sapply(dfr, function(x) mean(is.na(x))) < 0.2]
dfr_1DayLag = dfr_1DayLag[sapply(dfr_1DayLag, function(x) mean(is.na(x))) < 0.2]
dfr_2DayLag = dfr_2DayLag[sapply(dfr_2DayLag, function(x) mean(is.na(x))) < 0.2]



#remove the minimum columns
dfc$CO_min = NULL
dfc$NO_min= NULL
dfc$NO2_min = NULL
dfc$NOx_min = NULL
dfc$SO2_min = NULL
dfc$ozone_min = NULL

dfc_1DayLag$CO_min = NULL
dfc_1DayLag$NO_min= NULL
dfc_1DayLag$NO2_min = NULL
dfc_1DayLag$NOx_min = NULL
dfc_1DayLag$SO2_min = NULL
dfc_1DayLag$ozone_min = NULL

dfc_2DayLag$CO_min = NULL
dfc_2DayLag$NO_min= NULL
dfc_2DayLag$NO2_min = NULL
dfc_2DayLag$NOx_min = NULL
dfc_2DayLag$SO2_min = NULL
dfc_2DayLag$ozone_min = NULL

#remove the minimum columns
dfr$CO_min = NULL
dfr$NO_min= NULL
dfr$NO2_min = NULL
dfr$NOx_min = NULL
dfr$SO2_min = NULL
dfr$ozone_min = NULL

dfr_1DayLag$CO_min = NULL
dfr_1DayLag$NO_min= NULL
dfr_1DayLag$NO2_min = NULL
dfr_1DayLag$NOx_min = NULL
dfr_1DayLag$SO2_min = NULL
dfr_1DayLag$ozone_min = NULL

dfr_2DayLag$CO_min = NULL
dfr_2DayLag$NO_min= NULL
dfr_2DayLag$NO2_min = NULL
dfr_2DayLag$NOx_min = NULL
dfr_2DayLag$SO2_min = NULL
dfr_2DayLag$ozone_min = NULL


#remove NOx, no longer needed. just kept for graphing. NOx is just a combined total of NO and NO2
dfc$NOx_mean = NULL
dfc$NOx_max = NULL

dfc_1DayLag$NOx_mean = NULL
dfc_1DayLag$NOx_max = NULL

dfc_1DayLag$NOx_mean = NULL
dfc_1DayLag$NOx_max = NULL

dfr$NOx_mean = NULL
dfr$NOx_max = NULL

dfr_1DayLag$NOx_mean = NULL
dfr_1DayLag$NOx_max = NULL

dfr_1DayLag$NOx_mean = NULL
dfr_1DayLag$NOx_max = NULL

#remove data with infinite values, CO max has 133
dfc = do.call(data.frame,lapply(dfc, function(x) replace(x, is.infinite(x),NA)))
dfc_1DayLag = do.call(data.frame,lapply(dfc_1DayLag, function(x) replace(x, is.infinite(x),NA)))
dfc_2DayLag = do.call(data.frame,lapply(dfc_2DayLag, function(x) replace(x, is.infinite(x),NA)))

dfr = do.call(data.frame,lapply(dfr, function(x) replace(x, is.infinite(x),NA)))
dfr_1DayLag = do.call(data.frame,lapply(dfr_1DayLag, function(x) replace(x, is.infinite(x),NA)))
dfr_2DayLag = do.call(data.frame,lapply(dfr_2DayLag, function(x) replace(x, is.infinite(x),NA)))



#impute missing data
#https://www.jstatsoft.org/article/view/v045i03 
library(mice) #multivariate imputation via chained equations

#pattern of missing data
md.pattern(dfc)

#impute missing data
#m is the number of imputed datasets. maxit no of iterations to impute the missing data. method used is predictive mean matching
dfcimpute = mice(data = dfc, m = 5, method = "pmm", maxit = 10, seed = 100)
dfc_1DayLag_impute = mice(data = dfc_1DayLag, m = 5, method = "pmm", maxit = 10, seed = 100)
dfc_2DayLag_impute = mice(data = dfc_2DayLag, m = 5, method = "pmm", maxit = 10, seed = 100)

dfr_impute = mice(data = dfr, m = 5, method = "pmm", maxit = 10, seed = 100)
dfr_1DayLag_impute = mice(data = dfr_1DayLag, m = 5, method = "pmm", maxit = 10, seed = 100)
dfr_2DayLag_impute = mice(data = dfr_2DayLag, m = 5, method = "pmm", maxit = 10, seed = 100)

#5 imputes were created, taking the first one as our new dataset
dfc = complete(dfcimpute,1)
dfc_1DayLag = complete(dfc_1DayLag_impute,1)
dfc_2DayLag = complete(dfc_2DayLag_impute,1)

dfr = complete(dfr_impute,1)
dfr_1DayLag = complete(dfr_1DayLag_impute,1)
dfr_2DayLag = complete(dfr_2DayLag_impute,1)





#distribution of data. density plot
#histograms and density plots examples https://www.statmethods.net/graphs/density.html
densityplot(dfc$Cardiovascular_Cases)
densityplot(dfc$CO_mean)
densityplot(dfc$NO_mean)
densityplot(dfc$NO2_mean)
densityplot(dfc$NOx_mean)
densityplot(dfc$SO2_mean)
densityplot(dfc$ozone_max)

densityplot(dfr$Respiratory_Cases)
densityplot(dfr$CO_mean)
densityplot(dfr$NO_mean)
densityplot(dfr$NO2_mean)
densityplot(dfr$NOx_mean)
densityplot(dfr$SO2_mean)
densityplot(dfr$ozone_max)


#summary of the data
summary(dfc)
summary(dfr)

#time series dataframes for later analysis
dfc_ts = dfc
dfc_1DayLag_ts = dfc_1DayLag
dfc_2DayLag_ts = dfc_2DayLag

dfr_ts = dfr
dfr_1DayLag_ts = dfr_1DayLag
dfr_2DayLag_ts = dfr_2DayLag


#Datasets ------
#http://www.instantr.com/2012/12/18/performing-a-principal-component-analysis-in-r/
#remove the factor column and run PCA and create a screeplot of the data to see if there are any uncorrleated linear variables aka principle components in the dataset
#spilt the data into mean and max datasets to reduce mulitcollinearity and to see if the max values had a noticeable impact

#weather sets mean pollutants
dfc_weather = subset(dfc, select = c(Date, PM10, NO2_mean, CO_mean, NO_mean, ozone_mean, SO2_mean, Cardiovascular_Cases))
dfr_weather = subset(dfr, select = c(Date, PM10, NO2_mean, CO_mean, NO_mean, ozone_mean, SO2_mean, Respiratory_Cases))

dfc_mean = subset(dfc, select = c(PM10, NO2_mean, CO_mean, NO_mean, ozone_mean, SO2_mean, Cardiovascular_Cases))
dfc_max = subset(dfc, select = c(PM10, NO2_max, CO_max, NO_max, ozone_max, SO2_max, Cardiovascular_Cases ))

dfc_1DayLag_mean = subset(dfc_1DayLag, select = c(PM10, NO2_mean, CO_mean, NO_mean, ozone_mean, SO2_mean, Cardiovascular_Cases ))
dfc_1DayLag_max = subset(dfc_1DayLag, select = c(PM10, NO2_max, CO_max, NO_max, ozone_max, SO2_max, Cardiovascular_Cases ))

dfc_2DayLag_mean = subset(dfc_2DayLag, select = c(PM10, NO2_mean, CO_mean, NO_mean, ozone_mean, SO2_mean, Cardiovascular_Cases ))
dfc_2DayLag_max = subset(dfc_2DayLag, select = c(PM10, NO2_max, CO_max, NO_max, ozone_max, SO2_max, Cardiovascular_Cases ))


dfr_mean = subset(dfr, select = c(PM10, NO2_mean, CO_mean, NO_mean, ozone_mean, SO2_mean, Respiratory_Cases))
dfr_max = subset(dfr, select = c(PM10, NO2_max, CO_max, NO_max, ozone_max, SO2_max, Respiratory_Cases ))

dfr_1DayLag_mean = subset(dfr_1DayLag, select = c(PM10, NO2_mean, CO_mean, NO_mean, ozone_mean, SO2_mean, Respiratory_Cases ))
dfr_1DayLag_max = subset(dfr_1DayLag, select = c(PM10, NO2_max, CO_max, NO_max, ozone_max, SO2_max, Respiratory_Cases ))

dfr_2DayLag_mean = subset(dfr_2DayLag, select = c(PM10, NO2_mean, CO_mean, NO_mean, ozone_mean, SO2_mean, Respiratory_Cases ))
dfr_2DayLag_max = subset(dfr_2DayLag, select = c(PM10, NO2_max, CO_max, NO_max, ozone_max, SO2_max, Respiratory_Cases ))






#remove the factor column
dfc_mean = dfc_mean[-c(7)]
dfc_max = dfc_max[-c(7)] 
dfc_1DayLag_mean = dfc_1DayLag_mean[-c(7)]
dfc_1DayLag_max = dfc_1DayLag_max[-c(7)] 
dfc_2DayLag_mean = dfc_2DayLag_mean[-c(7)]
dfc_2DayLag_max = dfc_2DayLag_max[-c(7)] 


dfr_mean = dfr_mean[-c(7)]
dfr_max = dfr_max[-c(7)] 
dfr_1DayLag_mean = dfr_1DayLag_mean[-c(7)]
dfr_1DayLag_max = dfr_1DayLag_max[-c(7)] 
dfr_2DayLag_mean = dfr_2DayLag_mean[-c(7)]
dfr_2DayLag_max = dfr_2DayLag_max[-c(7)] 


#quick look at principle component analysis
dfc_mean = princomp(dfc_mean)
dfc_max = princomp(dfc_max)
dfc_1DayLag_mean = princomp(dfc_1DayLag_mean)
dfc_1DayLag_max = princomp(dfc_1DayLag_max)
dfc_2DayLag_mean = princomp(dfc_2DayLag_mean)
dfc_2DayLag_max = princomp(dfc_2DayLag_max)

dfr_mean = princomp(dfr_mean)
dfr_max = princomp(dfr_max)
dfr_1DayLag_mean = princomp(dfr_1DayLag_mean)
dfr_1DayLag_max = princomp(dfr_1DayLag_max)
dfr_2DayLag_mean = princomp(dfr_2DayLag_mean)
dfr_2DayLag_max = princomp(dfr_2DayLag_max)

#summary of PCA models, standard dev, variance, cumulative proportion
summary(dfc_mean)
summary(dfc_max)
summary(dfc_1DayLag_mean)
summary(dfc_1DayLag_max)
summary(dfc_2DayLag_mean)
summary(dfc_2DayLag_max)

summary(dfr_mean)
summary(dfr_max)
summary(dfr_1DayLag_mean)
summary(dfr_1DayLag_max)
summary(dfr_2DayLag_mean)
summary(dfr_2DayLag_max)


dfc_mean$loadings
dfc_max$loadings


screeplot(dfc_mean)
screeplot(dfc_max)

?screeplot


#add back in the cardio/respiratory cases into the dataframes
dfc_mean = subset(dfc, select = c(PM10, NO2_mean, CO_mean, NO_mean, ozone_mean, SO2_mean, Cardiovascular_Cases))
dfc_max = subset(dfc, select = c(PM10, NO2_max, CO_max, NO_max, ozone_max, SO2_max, Cardiovascular_Cases ))

dfc_1DayLag_mean = subset(dfc_1DayLag, select = c(PM10, NO2_mean, CO_mean, NO_mean, ozone_mean, SO2_mean, Cardiovascular_Cases ))
dfc_1DayLag_max = subset(dfc_1DayLag, select = c(PM10, NO2_max, CO_max, NO_max, ozone_max, SO2_max, Cardiovascular_Cases ))

dfc_2DayLag_mean = subset(dfc_2DayLag, select = c(PM10, NO2_mean, CO_mean, NO_mean, ozone_mean, SO2_mean, Cardiovascular_Cases ))
dfc_2DayLag_max = subset(dfc_2DayLag, select = c(PM10, NO2_max, CO_max, NO_max, ozone_max, SO2_max, Cardiovascular_Cases ))


dfr_mean = subset(dfr, select = c(PM10, NO2_mean, CO_mean, NO_mean, ozone_mean, SO2_mean, Respiratory_Cases))
dfr_max = subset(dfr, select = c(PM10, NO2_max, CO_max, NO_max, ozone_max, SO2_max, Respiratory_Cases ))

dfr_1DayLag_mean = subset(dfr_1DayLag, select = c(PM10, NO2_mean, CO_mean, NO_mean, ozone_mean, SO2_mean, Respiratory_Cases ))
dfr_1DayLag_max = subset(dfr_1DayLag, select = c(PM10, NO2_max, CO_max, NO_max, ozone_max, SO2_max, Respiratory_Cases ))

dfr_2DayLag_mean = subset(dfr_2DayLag, select = c(PM10, NO2_mean, CO_mean, NO_mean, ozone_mean, SO2_mean, Respiratory_Cases ))
dfr_2DayLag_max = subset(dfr_2DayLag, select = c(PM10, NO2_max, CO_max, NO_max, ozone_max, SO2_max, Respiratory_Cases ))


#the first component is showing that it is responsible for 87% of the variation in the dataset. data must be collinear?

#assuming no collinearity, a linear model was fit https://datascienceplus.com/multicollinearity-in-r/
#remove the row with a 0 in it
dfc_mean = subset(dfc_mean, Cardiovascular_Cases != 0)
dfc_max = subset(dfc_max, Cardiovascular_Cases != 0)


fit = lm(dfc_mean$Cardiovascular_Cases ~  dfc_mean$NO2_mean + dfc_mean$PM10 + dfc_mean$CO_mean + dfc_mean$NO_mean + dfc_mean$ozone_mean + dfc_mean$SO2_mean)
fitmx = lm(dfc_max$Cardiovascular_Cases ~  dfc_max$NO2_max + dfc_max$PM10 + dfc_max$CO_max + dfc_max$NO_max + dfc_max$ozone_max + dfc_max$SO2_max)

fit2 = lm(dfc_mean$Cardiovascular_Cases ~  dfc_mean$NO2_mean + dfc_mean$PM10 + dfc_mean$CO_mean + dfc_mean$ozone_mean + dfc_mean$SO2_mean)
fitmx2 = lm(dfc_max$Cardiovascular_Cases ~  dfc_max$NO2_max + dfc_max$PM10 + dfc_max$CO_max + dfc_max$ozone_max + dfc_max$SO2_max)

fit3 = lm(dfc_mean$Cardiovascular_Cases ~  dfc_mean$NO2_mean + dfc_mean$PM10 + dfc_mean$ozone_mean)
fitmx3 = lm(dfc_max$Cardiovascular_Cases ~  dfc_max$NO2_max + dfc_max$PM10 + dfc_max$ozone_max)
  
fit4 = lm(dfc_mean$Cardiovascular_Cases ~  dfc_mean$NO2_mean)
fitmx4 = lm(dfc_max$Cardiovascular_Cases ~  dfc_max$NO2_max)
  
fit5 = lm(dfc_mean$Cardiovascular_Cases ~  dfc_mean$PM10)
fitmx5 = lm(dfc_max$Cardiovascular_Cases ~  dfc_max$PM10)
  
fit6 = lm(dfc_mean$Cardiovascular_Cases ~  dfc_mean$ozone_mean)
fitmx6 = lm(dfc_max$Cardiovascular_Cases ~  dfc_max$ozone_max)

  
summary(fit)
summary(fitmx)

summary(fit2)
summary(fitmx2)

summary(fit3)
summary(fitmx3)

summary(fit4)
summary(fitmx4)

summary(fit5)
summary(fitmx5)

summary(fit6)
summary(fitmx6)




#diagntoci plot, residuals vs fitted, Q-Q plot, scale-location, residuals vs leverage
par(mfrow=c(2,2))
plot(fit)
plot(fitmx)

plot(fit2)
plot(fitmx2)

plot(fit3)
plot(fitmx3)

plot(fit4)
plot(fitmx4)

plot(fit5)
plot(fitmx5)

plot(fit6)
plot(fitmx6)



#pair wise correlations
correlations = dfc_mean
correlations_max = dfc_max
correlations_1Day = dfc_1DayLag_mean
correlations_1Day_max = dfc_1DayLag_max
correlations_2Day = dfc_2DayLag_mean
correlations_2Day_max = dfc_2DayLag_max

correlationsR = dfr_mean
correlationsR_max = dfr_max
correlationsR_1Day = dfr_1DayLag_mean
correlationsR_1Day_max = dfr_1DayLag_max
correlationsR_2Day = dfr_2DayLag_mean
correlationsR_2Day_max = dfr_2DayLag_max

#remove NO_mean
correlations$NO_mean = NULL
correlations_max$NO_max = NULL
correlations_1Day$NO_mean = NULL
correlations_1Day_max$NO_max = NULL
correlationsR_2Day$NO_mean = NULL
correlationsR_2Day_max$NO_max = NULL

correlationsR$NO_mean = NULL
correlationsR_max$NO_max = NULL
correlationsR_1Day$NO_mean = NULL
correlationsR_1Day_max$NO_max = NULL
correlationsR_2Day$NO_mean = NULL
correlationsR_2Day_max$NO_max = NULL

#plot correlation graph to show pair wise correlation between the variables. high correlations can show multicollinearity. NO, NO2, NOx causing it? All pollutants come from same source
library(GGally)
ggpairs(correlations,upper = list(continuous = wrap("cor", method = "pearson")))
ggpairs(correlations_max,upper = list(continuous = wrap("cor", method = "pearson")))
ggpairs(correlations_1Day, upper = list(continuous = wrap("cor", method = "pearson")))
ggpairs(correlations_1Day_max, upper = list(continuous = wrap("cor", method = "pearson")))
ggpairs(correlations_2Day, upper = list(continuous = wrap("cor", method = "pearson")))
ggpairs(correlations_2Day_max, upper = list(continuous = wrap("cor", method = "pearson")))

?ggpairs
??GGally

ggpairs(correlationsR, upper = list(continuous = wrap("cor", method = "pearson")))
ggpairs(correlationsR_max, upper = list(continuous = wrap("cor", method = "pearson")))
ggpairs(correlationsR_1Day, upper = list(continuous = wrap("cor", method = "pearson")))
ggpairs(correlationsR_1Day_max, upper = list(continuous = wrap("cor", method = "pearson")))
ggpairs(correlationsR_2Day, upper = list(continuous = wrap("cor", method = "pearson")))
ggpairs(correlationsR_2Day_max, upper = list(continuous = wrap("cor", method = "pearson")))


#weather correlations with mean pollutant values and cardiovascular data
dfw = subset(dfw, select = c(PM10, NO2_mean, CO_mean, ozone_mean, SO2_mean, rain, maxtp, mintp, hum, windspeed, winddirection, Cardiovascular_Cases, Respiratory_Cases))
ggpairs(dfw, upper = list(continuous = wrap("cor", method = "pearson")))

#correlation matrix
library(corpcor)
cor2pcor(cov(correlations))
cor2pcor(cov(correlations_max))


#Variance inflation factors- vif
library(car)
vif(fit)


#F-test. Column meaning VIF - variance inflation factor. TOL - tolerance. Wi - Farrar_Glauber F-test
library(mctest)
omcdiag(correlations, correlations$PM10)
omcdiag(correlations, correlations$NO2_mean)
omcdiag(correlations, correlations$ozone_mean)

imcdiag(correlations, correlations$PM10, method = "CVIF", cvif = 5, corr = TRUE)
imcdiag(correlations_max, correlations$PM10, method = "CVIF", cvif = 5, corr = TRUE)



?imcdiag

#partial correlation coeffecients, t-statistic and p-values
library(ppcor)
pcor(correlations, method = "pearson")
pcor(correlations_max, method = "pearson")




#Analysis and Models ------

#linear models of the individual different pollutants vs the amount of cases in the hospital
#general linear models to try and model the count data. https://www.theanalysisfactor.com/generalized-linear-models-in-r-part-6-poisson-regression-count-variables/
#not using LM model due to the datas distribution which is not normal as it is count data


#over dispersion if the data-residual deviance is greater than the degrees of freedom. 
#IE extra variance not accounted for by the model. variance was larger than the mean 
#in the dependent variable is statistically significant
Cardio_model = glm(dfc_mean$Cardiovascular_Cases ~ dfc_mean$PM10 + dfc_mean$NO2_mean + dfc_mean$CO_mean + dfc_mean$NO_mean + dfc_mean$ozone_mean + dfc_mean$SO2_mean, poisson)



#Warning message: glm.fit: algorithm did not converge 
#raised maxit, default is 25 raised it to 30. this increases the maximum number of reweighted least squares iterations
#statistically significant factors
Cardio_model2 = glm(dfc_mean$Cardiovascular_Cases ~ dfc_mean$PM10 + dfc_mean$NO2_mean + dfc_mean$ozone_mean + dfc_mean$SO2_mean, poisson)

PM10_model <- glm(dfc_mean$Cardiovascular_Cases ~ dfc_mean$PM10, poisson) 
PM10_modelmx <- glm(dfc_max$Cardiovascular_Cases ~ dfc_max$PM10, poisson) 

NO2_model <- glm(dfc_mean$Cardiovascular_Cases ~ dfc_mean$NO2_mean, poisson) 
NO2_modelmx <- glm(dfc_max$Cardiovascular_Cases ~ dfc_max$NO2_max, poisson) 

CO_model <- glm(dfc_mean$Cardiovascular_Cases ~ dfc_mean$CO_mean, poisson)
CO_modelmx <- glm(dfc_max$Cardiovascular_Cases ~ dfc_max$CO_max, poisson)

NO_model <- glm(dfc_mean$Cardiovascular_Cases ~ dfc_mean$NO_mean, poisson) 
NO_modelmx <- glm(dfc_max$Cardiovascular_Cases ~ dfc_max$NO_max, poisson) 

ozone_model <- glm(dfc_mean$Cardiovascular_Cases ~ dfc_mean$ozone_mean, poisson)
ozone_modelmx <- glm(dfc_max$Cardiovascular_Cases ~ dfc_max$ozone_max, poisson)

SO2_model <- glm(dfc_mean$Cardiovascular_Cases ~ dfc_mean$SO2_mean, poisson)
SO2_modelmx <- glm(dfc_max$Cardiovascular_Cases ~ dfc_max$SO2_max, poisson) 

#summary of the models
summary(Cardio_model)
summary(Cardio_model2)

summary(PM10_model)
summary(NO2_model)
summary(CO_model)
summary(NO_model)
summary(ozone_model)
summary(SO2_model)

summary(PM10_modelmx)
summary(NO2_modelmx)
summary(CO_modelmx)
summary(NO_modelmx)
summary(ozone_modelmx)
summary(SO2_modelmx)

#plot the models
plot(Cardio_model2)

plot(PM10_model)
plot(NO2_model)
plot(CO_model)
plot(NO_model)
plot(ozone_model)
plot(NOx_model)
plot(SO2_model)

plot(PM10_modelmx)
plot(NO2_modelmx)
plot(CO_modelmx)
plot(NO_modelmx)
plot(ozone_modelmx)
plot(NOx_modelmx)
plot(SO2_modelmx)

#add weather to dfr_mean model
dfr_mean = merge(dfr_mean, weather)

?gam





#Cardiovascular Models ----
Cardio_model = bam(Cardiovascular_Cases ~ PM10 + NO2_mean + CO_mean + ozone_mean + SO2_mean, family = poisson(), scale = 0, data = dfc_mean, select = TRUE)
Cardio_modelb = bam(Cardiovascular_Cases ~ PM10 + NO2_mean + CO_mean + ozone_mean + SO2_mean, family = poisson(), scale = 0, data = dfc_mean, select = TRUE)
Cardio_models = gam(Cardiovascular_Cases ~ s(PM10) + s(NO2_mean) + s(CO_mean) + s(ozone_mean) + s(SO2_mean), family = poisson(), scale = 0, data = dfc_mean, select = TRUE)
Cardio_modelsb = bam(Cardiovascular_Cases ~ s(PM10) + s(NO2_mean) + s(CO_mean) + s(ozone_mean) + s(SO2_mean), family = poisson(), scale = 0, data = dfc_mean, select = TRUE)

?gam
vif(Cardio_model)
vif(Cardio_modelb)
vif(Cardio_models)
vif(Cardio_modelsb)

plot(dfc_mean$PM10, fitted(Cardio_model))

#s(PM10) + s(NO2_mean) + s(CO_mean) + s(ozone_mean) + s(SO2_mean)
#s(PM10) + s(NO2_max) + s(CO_max) + s(ozone_max) + s(SO2_max)
#PM10 + dfc_max$NO2_max + dfc_max$CO_max + dfc_max$NO_max + ozone_max + SO2_max
#PM10 + NO2_mean + CO_mean + NO_mean + ozone_mean + SO2_mean

Cardio_models = bam(Cardiovascular_Cases ~ PM10 + NO2_mean + ozone_mean + SO2_mean, family = poisson(),scale = 0, data = dfc_mean)
Cardio_modelmx = bam(Cardiovascular_Cases ~  PM10 + NO2_max + CO_max + ozone_max + SO2_max, family = poisson(), scale = 0, data = dfc_max)
Cardio_model1Day = bam(Cardiovascular_Cases ~ s(PM10) + s(NO2_mean) + CO_mean + ozone_mean + s(SO2_mean), family = poisson(), scale = 0, data = dfc_1DayLag_mean)
Cardio_model1Daymx = bam(Cardiovascular_Cases ~ s(PM10) + s(NO2_max) + s(CO_max) + s(ozone_max) + s(SO2_max), family = poisson(), scale = 0, data = dfc_1DayLag_max)
Cardio_model2Day = bam(Cardiovascular_Cases ~ s(PM10) + s(NO2_mean) + s(CO_mean) + s(ozone_mean) + s(SO2_mean), family = poisson(), scale = 0, data = dfc_2DayLag_mean)
Cardio_model2Daymx = bam(Cardiovascular_Cases ~ PM10 + dfc_max$NO2_max + dfc_max$CO_max + dfc_max$NO_max + ozone_max + SO2_max, family = poisson(), scale = 0, data = dfc_2DayLag_max)

Cardio_model = gam(Cardiovascular_Cases ~ PM10 + NO2_mean + CO_mean + ozone_mean + SO2_mean, family = poisson(), scale = 0, data = dfc_mean)

library(mgcViz)
library(rgl)
qq(Cardio_models)

plot(Cardio_models)

summary(Cardio_models)

AIC(Cardio_model2Day)

par(mfrow=c(1,3))
plot(Cardio_models, pages = 1, residuals = T, pch = 19, cex = 0.25, scheme = 1, shade = T,  se = TRUE)

#doesnt work with BAM
plot.Gam(Cardio_model,  pages = 1, residuals = T, pch = 19, cex = 0.25, scheme = 1, col = '#FF8000', shade = T, shade.col = 'black', se = TRUE)

vif(Cardio_model)

anova.gam(Cardio_model,Cardio_models, freq = TRUE, test = "Chisq")

?anova.gam
AIC(Cardio_model)
summary.Gam(Cardio_model)
summary(Cardio_modelb)
summary.Gam(Cardio_models)
summary.Gam(Cardio_modelsb)

anova(Cardio_modelsb, Cardio_model, Cardio_modelb, Cardio_models )

#reset par function
dev.off()

#summary and model plot
par(mfrow=c(1,5))
summary(Cardio_model)

test = predict(Cardio_model1Day)
plot(Cardio_model, pages = 1, scheme = 1, all.terms = TRUE, seWithMean = TRUE)
plot(Cardio_model,residuals=TRUE, se = TRUE)
gam.check(Cardio_model)
hist(Cardio_model$residuals)
anova(Cardio_model1Day, Cardio_model) #compare models


summary(Cardio_modelmx)
plot(Cardio_modelmx, se = TRUE)

summary(Cardio_model1Day)
summary(Cardio_model1Daymx)
summary(Cardio_model2Day)
summary(Cardio_model2Daymx)



#significant Cardiovascular models
Cardio_model_sig = bam(Cardiovascular_Cases ~ s(PM10) + s(NO2_mean) + s(ozone_mean) + s(SO2_mean), family = poisson(), scale = 0, data = dfc_mean)
Cardio_model_sig = gam(Cardiovascular_Cases ~ NO2_mean * ozone_mean * NO_mean, poisson, data = dfc_mean) #* are the interaction terms between the variables
Cardio_modelmx_sig = gam(Cardiovascular_Cases ~ CO_max + NO_max + ozone_max, poisson, data = dfc_max)
Cardio_model1Day_sig = gam(Cardiovascular_Cases ~ PM10 + NO_mean + ozone_mean, poisson, data = dfc_1DayLag_mean)
Cardio_model1Daymx_sig = gam(Cardiovascular_Cases ~ PM10 + CO_max + NO_max + ozone_max + SO2_max, poisson, data = dfc_1DayLag_max)
Cardio_model2Day_sig = gam(Cardiovascular_Cases ~ PM10 + NO2_mean + NO_mean + ozone_mean + SO2_mean, poisson, data = dfc_mean)
Cardio_model2Daymx_sig = gam(Cardiovascular_Cases ~ CO_max + NO_max + ozone_max, poisson, data = dfc_2DayLag_max)

summary(Cardio_model_sig)
summary(Cardio_modelmx_sig)
summary(Cardio_model1Day_sig)
summary(Cardio_model1Daymx_sig)
summary(Cardio_model2Day_sig)
summary(Cardio_model2Daymx_sig)

#remove models
rm(Cardio_model_sig, Cardio_modelmx_sig, Cardio_model1Day_sig, Cardio_model1Daymx_sig, Cardio_model2Day_sig, Cardio_model2Daymx_sig)


Cardio_PM10_model = glm(Cardiovascular_Cases ~ PM10, poisson, data = dfc_mean)
Cardio_PM10_modelmx = glm(Cardiovascular_Cases ~ PM10, poisson, data = dfc_max)
Cardio_PM10_model1Day = glm(Cardiovascular_Cases ~ PM10, poisson, data = dfc_1DayLag_mean)
Cardio_PM10_model1Daymx = glm(Cardiovascular_Cases ~ PM10, poisson, data = dfc_1DayLag_max)
Cardio_PM10_model2Day = glm(Cardiovascular_Cases ~ PM10, poisson, data = dfc_2DayLag_mean )
Cardio_PM10_model2Daymx = glm(Cardiovascular_Cases ~ PM10, poisson, data = dfc_2DayLag_max)

plot(Cardio_PM10_model)
summary(Cardio_PM10_model)
summary(Cardio_PM10_modelmx)
summary(Cardio_PM10_model1Day)
summary(Cardio_PM10_model1Daymx)
summary(Cardio_PM10_model2Day)
summary(Cardio_PM10_model2Daymx)

#remove models
rm(Cardio_PM10_model, Cardio_PM10_model, Cardio_PM10_model1Day, Cardio_PM10_model1Day, Cardio_PM10_model2Day, Cardio_PM10_model2Daymx)


Cardio_NO2_model = glm(Cardiovascular_Cases ~ NO2_mean, poisson, data = dfc_mean)
Cardio_NO2_modelmx = glm(Cardiovascular_Cases ~ NO2_max, poisson, data = dfc_max)
Cardio_NO2_model1Day = glm(Cardiovascular_Cases ~ NO2_mean, poisson, data = dfc_1DayLag_mean)
Cardio_NO2_model1Daymx = glm(Cardiovascular_Cases ~ NO2_max, poisson, data = dfc_1DayLag_max)
Cardio_NO2_model2Day = glm(Cardiovascular_Cases ~ NO2_mean, poisson, data = dfc_2DayLag_mean)
Cardio_NO2_model2Daymx = glm(Cardiovascular_Cases ~ NO2_max, poisson, data = dfc_2DayLag_max)

summary(Cardio_NO2_model)
summary(Cardio_NO2_modelmx)
summary(Cardio_NO2_model1Day)
summary(Cardio_NO2_model1Daymx)
summary(Cardio_NO2_model2Day)
summary(Cardio_NO2_model2Daymx)

#remove models
rm(Cardio_NO2_model, Cardio_NO2_modelmx, Cardio_NO2_model1Day, Cardio_NO2_model1Daymx, Cardio_NO2_model2Day, Cardio_NO2_model2Daymx)


Cardio_CO_model = glm(Cardiovascular_Cases ~ CO_mean, poisson, data = dfc_mean)
Cardio_CO_modelmx = glm(Cardiovascular_Cases ~ CO_max, poisson, data = dfc_max)
Cardio_CO_model1Day = glm(Cardiovascular_Cases ~ CO_mean, poisson, data = dfc_1DayLag_mean)
Cardio_CO_model1Daymx = glm(Cardiovascular_Cases ~ CO_max, poisson, data = dfc_1DayLag_max)
Cardio_CO_model2Day = glm(Cardiovascular_Cases ~ CO_mean, poisson, data = dfc_2DayLag_mean)
Cardio_CO_model2Daymx = glm(Cardiovascular_Cases ~ CO_max, poisson, data = dfc_2DayLag_max)

summary(Cardio_CO_model)
summary(Cardio_CO_modelmx)
summary(Cardio_CO_model1Day)
summary(Cardio_CO_model1Daymx)
summary(Cardio_CO_model2Day)
summary(Cardio_CO_model2Daymx)

#remove models
rm(Cardio_CO_model, Cardio_CO_modelmx, Cardio_CO_model1Day, Cardio_CO_model1Daymx, Cardio_CO_model2Day, Cardio_CO_model2Daymx)


Cardio_NO_model = glm(Cardiovascular_Cases ~ NO_mean, poisson, data = dfc_mean)
Cardio_NO_modelmx = glm(Cardiovascular_Cases ~ NO_max, poisson, data = dfc_max)
Cardio_NO_model1Day = glm(Cardiovascular_Cases ~ NO_mean, poisson, data = dfc_1DayLag_mean)
Cardio_NO_model1Daymx = glm(Cardiovascular_Cases ~ NO_max, poisson, data = dfc_1DayLag_max)
Cardio_NO_model2Day = glm(Cardiovascular_Cases ~ NO_mean, poisson, data = dfc_2DayLag_mean)
Cardio_NO_model2Daymx = glm(Cardiovascular_Cases ~ NO_max, poisson, data = dfc_2DayLag_max)

summary(Cardio_NO_model)
summary(Cardio_NO_modelmx)
summary(Cardio_NO_model1Day)
summary(Cardio_NO_model1Daymx)
summary(Cardio_NO_model2Day)
summary(Cardio_NO_model2Daymx)

#remove models
rm(Cardio_NO_model, Cardio_NO_modelmx, Cardio_NO_model1Day, Cardio_NO_model1Daymx, Cardio_NO_model2Day, Cardio_NO_model2Daymx)


Cardio_ozone_model = glm(Cardiovascular_Cases ~ ozone_mean, poisson, data = dfc_mean)
Cardio_ozone_modelmx = glm(Cardiovascular_Cases ~ ozone_max, poisson, data = dfc_max)
Cardio_ozone_model1Day = glm(Cardiovascular_Cases ~ ozone_mean, poisson, data = dfc_1DayLag_mean)
Cardio_ozone_model1Daymx = glm(Cardiovascular_Cases ~ ozone_max, poisson, data = dfc_1DayLag_max)
Cardio_ozone_model2Day = glm(Cardiovascular_Cases ~ ozone_mean, poisson, data = dfc_2DayLag_mean)
Cardio_ozone_model2Daymx = glm(Cardiovascular_Cases ~ ozone_max, poisson, data = dfc_2DayLag_max)

summary(Cardio_ozone_model)
summary(Cardio_ozone_modelmx)
summary(Cardio_ozone_model1Day)
summary(Cardio_ozone_model1Daymx)
summary(Cardio_ozone_model2Day)
summary(Cardio_ozone_model2Daymx)

#remove models due to memory issues
rm(Cardio_ozone_model, Cardio_ozone_modelmx, Cardio_ozone_model1Day, Cardio_ozone_model1Daymx, Cardio_ozone_model2Day, Cardio_ozone_model2Daymx)


Cardio_SO2_model = glm(Cardiovascular_Cases ~ SO2_mean, poisson, data = dfc_mean)
Cardio_SO2_modelmx = glm(Cardiovascular_Cases ~ SO2_max, poisson, data = dfc_max)
Cardio_SO2_model1Day = glm(Cardiovascular_Cases ~ SO2_mean, poisson, data = dfc_1DayLag_mean)
Cardio_SO2_model1Daymx = glm(Cardiovascular_Cases ~ SO2_max, poisson, data = dfc_1DayLag_max)
Cardio_SO2_model2Day = glm(Cardiovascular_Cases ~ SO2_mean, poisson, data = dfc_2DayLag_mean)
Cardio_SO2_model2Daymx = glm(Cardiovascular_Cases ~ SO2_max, poisson, data = dfc_2DayLag_max)

summary(Cardio_SO2_model)
summary(Cardio_SO2_modelmx)
summary(Cardio_SO2_model1Day)
summary(Cardio_SO2_model1Daymx)
summary(Cardio_SO2_model2Day)
summary(Cardio_SO2_model2Daymx)




#s(PM10) + s(NO2_mean) + s(CO_mean) + s(ozone_mean) + s(SO2_mean)
#PM10 + NO2_max + CO_max + NO_max + ozone_max + SO2_max
#PM10 + NO2_max + CO_max + NO_max + ozone_max + SO2_max


rm(Cardio_CO_model, Cardio_CO_model1Day, Cardio_CO_model1Daymx, Cardio_CO_model2Day, Cardio_CO_model2Daymx)
rm(Cardio_SO2_model, Cardio_SO2_modelmx, Cardio_SO2_model1Day, Cardio_SO2_model1Daymx, Cardio_SO2_model2Day, Cardio_SO2_model2Daymx )

rm(Resp_model_sig)

Cardio_models = bam(Cardiovascular_Cases ~ PM10 + NO2_mean + ozone_mean + SO2_mean, family = poisson(),scale = 0, data = dfc_mean)


#Respiratory Models ----
Resp_model = bam(Respiratory_Cases ~ PM10 + NO2_mean + CO_mean + ozone_mean + SO2_mean, family = poisson(), scale = 0, data = dfr_mean, select = TRUE)
Resp_modelb = bam(Respiratory_Cases ~ PM10 + NO2_mean + CO_mean + ozone_mean + SO2_mean, family = poisson(), scale = 0, data = dfr_mean, select = TRUE)
Resp_models = bam(Respiratory_Cases ~ s(PM10) + s(NO2_mean) + s(CO_mean) + s(ozone_mean) + s(SO2_mean), family = poisson(), scale = 0, data = dfr_mean, select = TRUE)
Resp_modelsb = bam(Respiratory_Cases ~ s(PM10) + s(NO2_mean) + s(CO_mean) + s(ozone_mean) + s(SO2_mean), family = poisson(), scale = 0, data = dfr_mean, select = TRUE)

?gam
vif(Resp_model)

#PM10 + NO2_max + CO_max + NO_max + ozone_max + SO2_max
# PM10 + NO2_mean + CO_mean + NO_mean + ozone_mean + SO2_mean
#s(PM10) + s(NO2_mean) + s(CO_mean) + s(ozone_mean) + s(SO2_mean)

Resp_model = bam(Respiratory_Cases ~ PM10 + NO2_mean + CO_mean + + ozone_mean + SO2_mean, family = poisson(), scale = 0, data = dfr_mean)
Resp_modelmx = bam(Respiratory_Cases ~ PM10 + NO2_max + CO_max +  ozone_max + SO2_max, family = poisson(), scale = 0, data = dfr_max)
Resp_model1Day = bam(Respiratory_Cases ~ PM10 + NO2_mean + CO_mean +  ozone_mean + SO2_mean, family = poisson(), scale = 0, data = dfr_1DayLag_mean)
Resp_model1Daymx = bam(Respiratory_Cases ~PM10 + NO2_max + CO_max + NO_max + ozone_max + SO2_max, family = poisson(), scale = 0, data = dfr_1DayLag_max)
Resp_model2Day = bam(Respiratory_Cases ~ PM10 + NO2_mean + CO_mean +  ozone_mean + SO2_mean, family = poisson(), scale = 0, data = dfr_2DayLag_mean)
Resp_model2Daymx = bam(Respiratory_Cases ~ PM10 + NO2_max + CO_max +  ozone_max + SO2_max, family = poisson(), scale = 0, data = dfr_2DayLag_max)

Resp_model = bam(Respiratory_Cases ~ s(PM10) + s(NO2_mean) + s(CO_mean) + s(ozone_mean) + s(SO2_mean), family = poisson(), scale = 0, data = dfr_mean)
Resp_modelmx = bam(Respiratory_Cases ~ s(PM10) + s(NO2_max) + s(CO_max)  + s(ozone_max) + s(SO2_max), family = poisson(), scale = 0, data = dfr_max)
Resp_model1Day = bam(Respiratory_Cases ~ s(PM10) + s(NO2_mean) + s(CO_mean) + s(ozone_mean) + s(SO2_mean), family = poisson(), scale = 0, data = dfr_1DayLag_mean)
Resp_model1Daymx = bam(Respiratory_Cases ~ s(PM10) + s(NO2_max) + s(CO_max) + s(ozone_max) + s(SO2_max), family = poisson(), scale = 0, data = dfr_1DayLag_max)
Resp_model2Day = bam(Respiratory_Cases ~ s(PM10) + s(NO2_mean) + s(CO_mean) + s(ozone_mean) + s(SO2_mean), family = poisson(), scale = 0, data = dfr_2DayLag_mean)
Resp_model2Daymx = bam(Respiratory_Cases ~ s(PM10) + s(NO2_max) + s(CO_max) + s(ozone_max) + s(SO2_max), family = poisson(), scale = 0, data = dfr_2DayLag_max)


library(splines)
splineR = interpSpline(Resp_model)

summary(Resp_model)
summary(Resp_modelmx)
summary(Resp_model1Day)
summary(Resp_model1Daymx)
summary(Resp_model2Day)
summary(Resp_model2Daymx)



par(mfrow=c(1,5))
plot(Resp_modelmx, pages = 1, scheme = 1, all.terms = TRUE, seWithMean = TRUE)

qq(Resp_model)

plot(Resp_model)

AIC(Resp_model)

par(mfrow=c(1,3))
plot(Resp_model, pages = 1, residuals = T, pch = 19, cex = 0.25, scheme = 1, shade = T,  se = TRUE)


?gam
Resp_model = gam(Respiratory_Cases ~ PM10 + NO2_mean + CO_mean + ozone_mean + SO2_mean, family = poisson(), scale = 0, data = dfr_mean)

Resp_model
plot.Gam(Resp_model, se = TRUE, ask = FALSE)
?par

help(summary.gam)
anova.gam(Resp_model,Resp_modelb, Resp_models, Resp_modelsb, freq = TRUE, test = "Chisq")

?anova.gam
AIC(Resp_model)
summary(Resp_model)
summary.Gam(Resp_modelb)
summary.Gam(Resp_models)
summary.Gam(Resp_modelsb)

anova(Resp_modelsb, Resp_model, Resp_modelb, Resp_models )

#reset par function
dev.off()

#summary and model plot
par(mfrow=c(1,5))
summary(Resp_model)

test = predict(Resp_model1Day)
plot(Resp_model, pages = 1, scheme = 1, all.terms = TRUE, seWithMean = TRUE)
plot(Resp_model,residuals=TRUE, se = TRUE)
gam.check(Resp_model)
hist(Resp_model$residuals)
anova(Resp_model1Day, Resp_model) #compare models


summary(Resp_modelmx)
plot(Resp_modelmx, se = TRUE)

summary(Resp_model1Day)
summary(Resp_model1Daymx)
summary(Resp_model2Day)
summary(Resp_model2Daymx)



#significant respiratory models
Resp_model_sig = bam(Respiratory_Cases ~ NO2_mean * ozone_mean * NO_mean, poisson, data = dfr_mean) #* are the interaction terms between the variables
Resp_modelmx_sig = bam(Respiratory_Cases ~ CO_max + NO_max + ozone_max, poisson, data = dfr_max)
Resp_model1Day_sig = bam(Respiratory_Cases ~ PM10 + NO_mean + ozone_mean, poisson, data = dfr_1DayLag_mean)
Resp_model1Daymx_sig = bam(Respiratory_Cases ~ PM10 + CO_max + NO_max + ozone_max + SO2_max, poisson, data = dfr_1DayLag_max)
Resp_model2Day_sig = bam(Respiratory_Cases ~ PM10 + NO2_mean + NO_mean + ozone_mean + SO2_mean, poisson, data = dfr_mean)
Resp_model2Daymx_sig = bam(Respiratory_Cases ~ CO_max + NO_max + ozone_max, poisson, data = dfr_2DayLag_max)

summary(Resp_model_sig)
summary(Resp_modelmx_sig)
summary(Resp_model1Day_sig)
summary(Resp_model1Daymx_sig)
summary(Resp_model2Day_sig)
summary(Resp_model2Daymx_sig)


Resp_PM10_model = gam(Respiratory_Cases ~ PM10, poisson, data = dfr_mean)
Resp_PM10_modelmx = gam(Respiratory_Cases ~ PM10, poisson, data = dfr_max)
Resp_PM10_model1Day = gam(Respiratory_Cases ~ PM10, poisson, data = dfr_1DayLag_mean)
Resp_PM10_model1Daymx = gam(Respiratory_Cases ~ PM10, poisson, data = dfr_1DayLag_max)
Resp_PM10_model2Day = gam(Respiratory_Cases ~ PM10, poisson, data = dfr_2DayLag_mean )
Resp_PM10_model2Daymx = gam(Respiratory_Cases ~ PM10, poisson, data = dfr_2DayLag_max)

summary(Resp_PM10_model)
summary(Resp_PM10_modelmx)
summary(Resp_PM10_model1Day)
summary(Resp_PM10_model1Daymx)
summary(Resp_PM10_model2Day)
summary(Resp_PM10_model2Daymx)

Resp_NO2_model = gam(Respiratory_Cases ~ NO2_mean, poisson, data = dfr_mean)
Resp_NO2_modelmx = gam(Respiratory_Cases ~ NO2_max, poisson, data = dfr_max)
Resp_NO2_model1Day = gam(Respiratory_Cases ~ NO2_mean, poisson, data = dfr_1DayLag_mean)
Resp_NO2_model1Daymx = gam(Respiratory_Cases ~ NO2_max, poisson, data = dfr_1DayLag_max)
Resp_NO2_model2Day = gam(Respiratory_Cases ~ NO2_mean, poisson, data = dfr_2DayLag_mean)
Resp_NO2_model2Daymx = gam(Respiratory_Cases ~ NO2_max, poisson, data = dfr_2DayLag_max)

summary(Resp_NO2_model)
summary(Resp_NO2_modelmx)
summary(Resp_NO2_model1Day)
summary(Resp_NO2_model1Daymx)
summary(Resp_NO2_model2Day)
summary(Resp_NO2_model2Daymx)


Resp_CO_model = gam(Respiratory_Cases ~ CO_mean, poisson, data = dfr_mean)
Resp_CO_modelmx = gam(Respiratory_Cases ~ CO_max, poisson, data = dfr_max)
Resp_CO_model1Day = gam(Respiratory_Cases ~ CO_mean, poisson, data = dfr_1DayLag_mean)
Resp_CO_model1Daymx = gam(Respiratory_Cases ~ CO_max, poisson, data = dfr_1DayLag_max)
Resp_CO_model2Day = gam(Respiratory_Cases ~ CO_mean, poisson, data = dfr_2DayLag_mean)
Resp_CO_model2Daymx = gam(Respiratory_Cases ~ CO_max, poisson, data = dfr_2DayLag_max)

summary(Resp_CO_model)
summary(Resp_CO_modelmx)
summary(Resp_CO_model1Day)
summary(Resp_CO_model1Daymx)
summary(Resp_CO_model2Day)
summary(Resp_CO_model2Daymx)

Resp_NO_model = gam(Respiratory_Cases ~ NO_mean, poisson, data = dfr_mean)
Resp_NO_modelmx = gam(Respiratory_Cases ~ NO_max, poisson, data = dfr_max)
Resp_NO_model1Day = gam(Respiratory_Cases ~ NO_mean, poisson, data = dfr_1DayLag_mean)
Resp_NO_model1Daymx = gam(Respiratory_Cases ~ NO_max, poisson, data = dfr_1DayLag_max)
Resp_NO_model2Day = gam(Respiratory_Cases ~ NO_mean, poisson, data = dfr_2DayLag_mean)
Resp_NO_model2Daymx = gam(Respiratory_Cases ~ NO_max, poisson, data = dfr_2DayLag_max)

summary(Resp_NO_model)
summary(Resp_NO_modelmx)
summary(Resp_NO_model1Day)
summary(Resp_NO_model1Daymx)
summary(Resp_NO_model2Day)
summary(Resp_NO_model2Daymx)

Resp_ozone_model = gam(Respiratory_Cases ~ ozone_mean, poisson, data = dfr_mean)
Resp_ozone_modelmx = gam(Respiratory_Cases ~ ozone_max, poisson, data = dfr_max)
Resp_ozone_model1Day = gam(Respiratory_Cases ~ ozone_mean, poisson, data = dfr_1DayLag_mean)
Resp_ozone_model1Daymx = gam(Respiratory_Cases ~ ozone_max, poisson, data = dfr_1DayLag_max)
Resp_ozone_model2Day = gam(Respiratory_Cases ~ ozone_mean, poisson, data = dfr_2DayLag_mean)
Resp_ozone_model2Daymx = gam(Respiratory_Cases ~ ozone_max, poisson, data = dfr_2DayLag_max)

summary(Resp_ozone_model)
summary(Resp_ozone_modelmx)
summary(Resp_ozone_model1Day)
summary(Resp_ozone_model1Daymx)
summary(Resp_ozone_model2Day)
summary(Resp_ozone_model2Daymx)

Resp_SO2_model = gam(Respiratory_Cases ~ SO2_mean, poisson, data = dfr_mean)
Resp_SO2_modelmx = gam(Respiratory_Cases ~ SO2_max, poisson, data = dfr_max)
Resp_SO2_model1Day = gam(Respiratory_Cases ~ SO2_mean, poisson, data = dfr_1DayLag_mean)
Resp_SO2_model1Daymx = gam(Respiratory_Cases ~ SO2_max, poisson, data = dfr_1DayLag_max)
Resp_SO2_model2Day = gam(Respiratory_Cases ~ SO2_mean, poisson, data = dfr_2DayLag_mean)
Resp_SO2_model2Daymx = gam(Respiratory_Cases ~ SO2_max, poisson, data = dfr_2DayLag_max)

summary(Resp_SO2_model)
summary(Resp_SO2_modelmx)
summary(Resp_SO2_model1Day)
summary(Resp_SO2_model1Daymx)
summary(Resp_SO2_model2Day)
summary(Resp_SO2_model2Daymx)


predict(Resp_PM10_model, type = "link")


#weather model
Resp_model_w = bam(Respiratory_Cases ~ PM10 + NO2_mean + CO_mean +ozone_mean + SO2_mean + rain + maxtp + mintp + hum + windspeed+ winddirection, family = poisson(), scale = 0, data = dfw)
Cardio_model_w = bam(Cardiovascular_Cases ~ PM10 + NO2_mean + CO_mean +ozone_mean + SO2_mean + rain + maxtp + mintp + hum + windspeed+ winddirection, family = poisson(), scale = 0, data = dfw)

#add splines
Resp_model_ws = bam(Respiratory_Cases ~ s(PM10) + s(NO2_mean) + s(CO_mean) + s(ozone_mean) + s(SO2_mean) + s(rain) + s(maxtp) + s(mintp) + s(hum) + s(windspeed) + s(winddirection), family = poisson(), scale = 0, data = dfw)
Cardio_model_ws = bam(Cardiovascular_Cases ~  s(PM10) + s(NO2_mean) + s(CO_mean) + s(ozone_mean) + s(SO2_mean) + s(rain) + s(maxtp) + s(mintp) + s(hum) + s(windspeed) + s(winddirection), family = poisson(), scale = 0, data = dfw)

summary(Resp_model_w)
summary(Cardio_model_w)

summary(Resp_model_ws)
summary(Cardio_model_ws)



#weather and PM2.5 -dataset is 2 years less of data
PM25_w = merge(data_PM25, PM25)

Resp_model_w25 = bam(Respiratory_Cases ~ PM10 + PM25 + NO2_mean + CO_mean +ozone_mean + SO2_mean + rain + maxtp + mintp + hum + windspeed+ winddirection, family = poisson(), scale = 0, data = PM25_w)
Cardio_model_w25 = bam(Cardiovascular_Cases ~ PM10 + PM25 + NO2_mean + CO_mean +ozone_mean + SO2_mean + rain + maxtp + mintp + hum + windspeed+ winddirection, family = poisson(), scale = 0, data = PM25_w)

Resp_model_ws25 = bam(Respiratory_Cases ~ s(PM10) + s(PM25) + s(NO2_mean) + s(CO_mean) + s(ozone_mean) + s(SO2_mean) + s(rain) + s(maxtp) + s(mintp) + s(hum) + s(windspeed) + s(winddirection), family = poisson(), scale = 0, data = PM25_w)
Cardio_model_ws25 = bam(Cardiovascular_Cases ~  s(PM10) + s(PM25) + s(NO2_mean) + s(CO_mean) + s(ozone_mean) + s(SO2_mean) + s(rain) + s(maxtp) + s(mintp) + s(hum) + s(windspeed) + s(winddirection), family = poisson(), scale = 0, data = PM25_w)

summary(Resp_model_w25)
summary(Cardio_model_w25)

summary(Resp_model_ws25)
summary(Cardio_model_ws25)

anova(Cardio_model_ws25,Cardio_model_w25)
anova(Resp_model_w25, Resp_model_ws25)


#day lags
Cardio_model_w25_1Day = bam(Cardiovascular_Cases ~ PM10 + PM25 + NO2_mean + CO_mean + ozone_mean + SO2_mean + rain + maxtp + mintp + hum + windspeed+ winddirection, family = poisson(), scale = 0, data = PM25_1DayLag)
Cardio_model_w25_2Day = bam(Cardiovascular_Cases ~ PM10 + PM25 + NO2_mean + CO_mean +ozone_mean + SO2_mean + rain + maxtp + mintp + hum + windspeed+ winddirection, family = poisson(), scale = 0, data = PM25_2DayLag)

Resp_model_w25_1Day = bam(Respiratory_Cases ~ PM10 + PM25 + NO2_mean + CO_mean + ozone_mean + SO2_mean + rain + maxtp + mintp + hum + windspeed+ winddirection, family = poisson(), scale = 0, data = PM25_1DayLag)
Resp_model2_w25_2Day = bam(Respiratory_Cases ~ PM10 + PM25 + NO2_mean + CO_mean +ozone_mean + SO2_mean + rain + maxtp + mintp + hum + windspeed+ winddirection, family = poisson(), scale = 0, data = PM25_2DayLag)

summary(Cardio_model_w25_1Day)
summary(Cardio_model_w25_2Day)

summary(Resp_model_w25_1Day)
summary(Resp_model2_w25_2Day)








#try quasi-poisson model to account for over dispersion
PM10_modelq <- glm(dfc_mean$Cardiovascular_Cases ~ dfc_mean$PM10, quasipoisson)
summary(PM10_modelq)


#http://rcompanion.org/handbook/J_01.html
library(car)

Anova(Cardio_model2, 
      type="II", 
      test="LR")

Anova(Resp_model)


library(rcompanion)

nagelkerke(Cardio_model2)
  

library(multcompView)

library(emmeans)

marginal = emmeans(Cardio_model2, ~ PM10)

pairs(marginal,
      adjust="tukey")

cld(marginal,
    alpha=0.05, 
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")   ### Tukey adjustment for multiple comparisons

  

#test for dispersion
library(AER)

dispersiontest(Cardio_model2,trafo=1)
?dispersiontest

#there is dispersion in the data
#so trying negative binomial regression 

library(MASS)

Cardio_model_nbr = glm.nb(dfc_mean$Cardiovascular_Cases ~ dfc_mean$PM10 + dfc_mean$NO2_mean + dfc_mean$ozone_mean + dfc_mean$SO2_mean)

Anova(Cardio_model_nbr, 
      type="II", 
      test="LR")

nagelkerke(Cardio_model_nbr)

marginal = emmeans(Cardio_model_nbr, 
                   ~ PM10)

pairs(marginal,
      adjust="tukey")

cld(marginal,
    alpha   = 0.05, 
    Letters = letters,    ### Use lower-case letters for .group
    type    = "response", ### Report emmeans in orginal scale
    adjust =  "tukey")



  
  
  

#Time Series during HIPE data -------
#dfc_ts 
#dfr_ts 


dfc_mean_ts = subset(dfc_ts, select = c(Date, PM10, NO2_mean, CO_mean, NO_mean, ozone_mean, SO2_mean, Cardiovascular_Cases))
dfc_max_ts = subset(dfc_ts, select = c(Date, PM10, NO2_max, CO_max, NO_max, ozone_max, SO2_max, Cardiovascular_Cases ))

dfr_mean_ts = subset(dfr_ts, select = c(Date, PM10, NO2_mean, CO_mean, NO_mean, ozone_mean, SO2_mean, Respiratory_Cases))
dfr_max_ts = subset(dfr_ts, select = c(Date, PM10, NO2_max, CO_max, NO_max, ozone_max, SO2_max, Respiratory_Cases ))


#aggregate data into monthly data for time series
monthly = aggregate(dfc_mean_ts[names(dfc_mean_ts)!='Date'], list(hour=cut(dfc_mean_ts$Date,'month')), mean, na.rm=T)
dfc_mean_ts = monthly

monthly = aggregate(dfc_max_ts[names(dfc_max_ts)!='Date'], list(hour=cut(dfc_max_ts$Date,'month')), max, na.rm=T)
dfc_max_ts = monthly

monthly = aggregate(dfr_mean_ts[names(dfr_mean_ts)!='Date'], list(hour=cut(dfr_mean_ts$Date,'month')), mean, na.rm=T)
dfr_mean_ts = monthly

monthly = aggregate(dfr_max_ts[names(dfr_max_ts)!='Date'], list(hour=cut(dfr_max_ts$Date,'month')), max, na.rm=T)
dfr_max_ts = monthly


#clean enviroment 
rm(list= ls()[!(ls() %in% c('dfc_ts','dfr_ts', 'dfc_mean_ts', 'dfc_max_ts', 'dfr_mean_ts', 'dfr_max_ts'))])



#plot some time series graphs
ts.plot(ts(dfc_mean_ts$PM10, frequency=12, start=c(2007,1)))
ts.plot(ts(dfc_mean_ts$NO2_mean, frequency=12, start=c(2007,1)))
ts.plot(ts(dfc_mean_ts$CO_mean, frequency=12, start=c(2007,1)))
ts.plot(ts(dfc_mean_ts$NO_mean, frequency=12, start=c(2007,1)))
ts.plot(ts(dfc_mean_ts$ozone_mean, frequency=12, start=c(2007,1)))
ts.plot(ts(dfc_mean_ts$SO2_mean, frequency=12, start=c(2007,1)))
ts.plot(ts(dfc_mean_ts$Cardiovascular_Cases, frequency=12, start=c(2007,1)))

ts.plot(ts(dfc_max_ts$PM10, frequency=12, start=c(2007,1)))
ts.plot(ts(dfc_max_ts$NO2_max, frequency=12, start=c(2007,1)))
ts.plot(ts(dfc_max_ts$CO_max, frequency=12, start=c(2007,1)))
ts.plot(ts(dfc_max_ts$NO_max, frequency=12, start=c(2007,1)))
ts.plot(ts(dfc_max_ts$ozone_max, frequency=12, start=c(2007,1)))
ts.plot(ts(dfc_max_ts$SO2_max, frequency=12, start=c(2007,1)))
ts.plot(ts(dfc_max_ts$Cardiovascular_Cases, frequency=12, start=c(2007,1)))

ts.plot(ts(dfr_mean_ts$Respiratory_Cases, frequency=12, start=c(2007,1)))
ts.plot(ts(dfr_max_ts$Respiratory_Cases, frequency=12, start=c(2007,1)))


#apply a moving average to the data to smooth the data
library(TTR)
dfc_max_ts_MA = (SMA(ts(dfc_max_ts$ozone_max, frequency=12, start=c(2007,1)),  n=6))


#estimate the trend, seasonal and irregular components
dfc_max_ts_components =  decompose(dfc_max_ts_MA)
plot(dfc_max_ts_components)

#seasonal adjusting
dfc_max_ts_MA = dfc_max_ts_MA - dfc_max_ts_components$seasonal

#seasonal variation has been removed. left with the trend and irregular component
plot(dfc_max_ts_MA)



#or would log transforming the data work better
d.new <- dfc_mean_ts
d.new[, 2:7] <- log(dfc_mean_ts[2:7], 2)
ts.plot(ts(d.new$PM10, frequency=12, start=c(2007,1)))







#ARIMA MODELS #https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html#arima-models


CardioCases_difference = diff(dfc_mean_ts$Cardiovascular_Cases, differences=1)
plot.ts(CardioCases_difference, frequency=12, start=c(2007,1))


acf(dfc_mean_ts, lag.max=20)             # plot a correlogram
acf(dfc_mean_ts, lag.max=20, plot=FALSE) # get the autocorrelation values



pacf(dfc_mean_ts$PM10, lag.max=20)             # plot a partial correlogram
pacf(dfc_mean_ts$PM10, lag.max=20, plot=FALSE) # get the partial autocorrelation values


#use auto arima function to find appropriate ARIMA model to run for each series
library(forecast)

#?auto.arima
auto.arima(dfc_mean_ts$PM10) #ARIMA(4,0,1)
auto.arima(dfc_mean_ts$NO2_mean) #ARIMA(3,1,1)
auto.arima(dfc_mean_ts$CO_mean) #ARIMA(1,0,0)
auto.arima(dfc_mean_ts$NO_mean) #ARIMA(3,0,3) 
auto.arima(dfc_mean_ts$ozone_mean) #ARIMA(1,0,0)
auto.arima(dfc_mean_ts$SO2_mean) #ARIMA(0,1,1)
auto.arima(dfc_mean_ts$Cardiovascular_Cases) #ARIMA(3,1,2)

auto.arima(dfc_max_ts$PM10) #ARIMA(1,0,0)
auto.arima(dfc_max_ts$NO2_max) #ARIMA(2,1,1)
auto.arima(dfc_max_ts$CO_max) #ARIMA(1,1,1)
auto.arima(dfc_max_ts$NO_max) #ARIMA(1,0,0) 
auto.arima(dfc_max_ts$ozone_max) #ARIMA(2,0,3)
auto.arima(dfc_max_ts$SO2_max) #ARIMA(1,1,0)
auto.arima(dfc_max_ts$Cardiovascular_Cases) #ARIMA(0,1,1)

auto.arima(ts(dfr_mean_ts$Respiratory_Cases), seasonal = T) #ARIMA(1,1,2) #same as dfc mean arima models, same values

auto.arima(dfr_max_ts$PM10) #ARIMA(1,0,0)
auto.arima(dfr_max_ts$NO2_max) #ARIMA(2,1,1)
auto.arima(dfr_max_ts$CO_max) #ARIMA(1,1,2)       #slightly different arima models as it came from different imputations
auto.arima(dfr_max_ts$NO_max) #ARIMA(1,0,0) 
auto.arima(dfr_max_ts$ozone_max) #ARIMA(3,0,1)
auto.arima(dfr_max_ts$SO2_max) #ARIMA(1,1,0)
auto.arima(dfr_max_ts$Respiratory_Cases) #ARIMA(0,1,0)



#Forecasting Time series ------
#Respiratory mean
Respiratory_arima = arima(dfr_mean_ts$Respiratory_Cases, order=c(1,1,2))
Respiratory_arima

#https://otexts.org/fpp2/arima-r.html check model is is stationary and invertible by checking the rooks of the polynomials.
#dots in the circles show that the model is good for forecasting
autoplot(Respiratory_arima)

#95% confidence interval forecast time series. D=1 sets it to seasonal ARIMA, h is how many periods of forecasting
Respiratory_arima = forecast(auto.arima(ts(dfr_mean_ts$Respiratory_Cases, frequency=12, start=c(2007,1)), D=1), h=36, level=c(95))
plot(Respiratory_arima)


#correlogram of forecast errors
acf(Respiratory_arima$residuals, lag.max=120)

#Ljung-box test
Box.test(Respiratory_arima$residuals, lag=20, type="Ljung-Box")

#time plot & histogram of forecast errors
plot.ts(Respiratory_arima$residuals)          
#plotForecastErrors(Respiratory_arima$residuals) #doesnt work make own histogram
hist(Respiratory_arima$residuals, 
     main="Histogram of Residuals",
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(Respiratory_arima$residuals))





#Cardiovascular
Cardiovascular_arima = arima(dfc_mean_ts$Cardiovascular_Cases, order=c(1,1,2))
Cardiovascular_arima

#https://otexts.org/fpp2/arima-r.html check model is is stationary and invertible by checking the rooks of the polynomials.
#dots in the circles show that the model is good for forecasting
autoplot(Cardiovascular_arima)

#95% confidence interval forecast time series. D=1 sets it to seasonal ARIMA, h is how many periods of forecasting
Cardiovascular_arima = forecast(auto.arima(ts(dfc_mean_ts$Cardiovascular_Cases, frequency=12, start=c(2007,1)), D=1), h=36, level=c(95))
plot(Cardiovascular_arima)


#correlogram of forecast errors
acf(Cardiovascular_arima$residuals, lag.max=120)

#Ljung-box test
Box.test(Cardiovascular_arima$residuals, lag=20, type="Ljung-Box")

#time plot & histogram of forecast errors
plot.ts(Cardiovascular_arima$residuals)          

#plotForecastErrors(Cardiovascular_arima$residuals) #doesnt work make own histogram
hist(Cardiovascular_arima$residuals, 
     main="Histogram of Residuals",
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(Cardiovascular_arima$residuals))





#Pollutant Time series-the means
#PM10
PM10_arima = arima(dfc_mean_ts$PM10, order=c(4,0,1))
PM10_arima

#https://otexts.org/fpp2/arima-r.html check model is is stationary and invertible by checking the rooks of the polynomials.
#dots in the circles show that the model is good for forecasting
autoplot(PM10_arima)

#95% confidence interval forecast time series. D=1 sets it to seasonal ARIMA, h is how many periods of forecasting
PM10_arima = forecast(auto.arima(ts(dfc_mean_ts$PM10, frequency=12, start=c(2007,1)), D=1), h=36, level=c(95))
plot(PM10_arima)

#correlogram of forecast errors
acf(PM10_arima$residuals, lag.max=120)

#Ljung-box test
Box.test(PM10_arima$residuals, lag=20, type="Ljung-Box")

#time plot & histogram of forecast errors
plot.ts(PM10_arima$residuals)          

#plotForecastErrors(PM10_arima$residuals) #doesnt work make own histogram
hist(PM10_arima$residuals, 
     main="Histogram of Residuals",
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(PM10_arima$residuals))




#NO2_mean
NO2_mean_arima = arima(dfc_mean_ts$NO2_mean, order=c(3,1,1))
NO2_mean_arima

#https://otexts.org/fpp2/arima-r.html check model is is stationary and invertible by checking the rooks of the polynomials.
#dots in the circles show that the model is good for forecasting
autoplot(NO2_mean_arima)

#95% confidence interval forecast time series. D=1 sets it to seasonal ARIMA, h is how many periods of forecasting
NO2_mean_arima = forecast(auto.arima(ts(dfc_mean_ts$NO2_mean, frequency=12, start=c(2007,1)), D=1), h=36, level=c(95))
plot(NO2_mean_arima)

#correlogram of forecast errors
acf(NO2_mean_arima$residuals, lag.max=120)

#Ljung-box test
Box.test(NO2_mean_arima$residuals, lag=20, type="Ljung-Box")

#time plot & histogram of forecast errors
plot.ts(NO2_mean_arima$residuals)          

#plotForecastErrors(NO2_mean_arima$residuals) #doesnt work make own histogram
hist(NO2_mean_arima$residuals, 
     main="Histogram of Residuals",
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(NO2_mean_arima$residuals))



#CO_mean
CO_mean_arima = arima(dfc_mean_ts$CO_mean, order=c(1,0,0))
CO_mean_arima

#https://otexts.org/fpp2/arima-r.html check model is is stationary and invertible by checking the rooks of the polynomials.
#dots in the circles show that the model is good for forecasting
autoplot(CO_mean_arima)

#95% confidence interval forecast time series. D=1 sets it to seasonal ARIMA, h is how many periods of forecasting
CO_mean_arima = forecast(auto.arima(ts(dfc_mean_ts$CO_mean, frequency=12, start=c(2007,1)), D=1), h=36, level=c(95))
plot(CO_mean_arima)

#correlogram of forecast errors
acf(CO_mean_arima$residuals, lag.max=120)

#Ljung-box test
Box.test(CO_mean_arima$residuals, lag=20, type="Ljung-Box")

#time plot & histogram of forecast errors
plot.ts(CO_mean_arima$residuals)          

#plotForecastErrors(CO_mean_arima$residuals) #doesnt work make own histogram
hist(CO_mean_arima$residuals, 
     main="Histogram of Residuals",
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(CO_mean_arima$residuals))


#NO_mean
NO_mean_arima = arima(dfc_mean_ts$NO_mean, order=c(3,0,3))
NO_mean_arima

#https://otexts.org/fpp2/arima-r.html check model is is stationary and invertible by checking the rooks of the polynomials.
#dots in the circles show that the model is good for forecasting
autoplot(NO_mean_arima)

#95% confidence interval forecast time series. D=1 sets it to seasonal ARIMA, h is how many periods of forecasting
NO_mean_arima = forecast(auto.arima(ts(dfc_mean_ts$NO_mean, frequency=12, start=c(2007,1)), D=1), h=36, level=c(95))
plot(NO_mean_arima)

#correlogram of forecast errors
acf(NO_mean_arima$residuals, lag.max=120)

#Ljung-box test
Box.test(NO_mean_arima$residuals, lag=20, type="Ljung-Box")

#time plot & histogram of forecast errors
plot.ts(NO_mean_arima$residuals)          

#plotForecastErrors(NO_mean_arima$residuals) #doesnt work make own histogram
hist(NO_mean_arima$residuals, 
     main="Histogram of Residuals",
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(NO_mean_arima$residuals))




#ozone_mean
ozone_mean_arima = arima(dfc_mean_ts$ozone_mean, order=c(1,0,0))
ozone_mean_arima

#https://otexts.org/fpp2/arima-r.html check model is is stationary and invertible by checking the rooks of the polynomials.
#dots in the circles show that the model is good for forecasting
autoplot(ozone_mean_arima)

#95% confidence interval forecast time series. D=1 sets it to seasonal ARIMA, h is how many periods of forecasting
ozone_mean_arima = forecast(auto.arima(ts(dfc_mean_ts$ozone_mean, frequency=12, start=c(2007,1)), D=1), h=36, level=c(95))
plot(ozone_mean_arima)

#correlogram of forecast errors
acf(ozone_mean_arima$residuals, lag.max=120)

#Ljung-box test
Box.test(ozone_mean_arima$residuals, lag=20, type="Ljung-Box")

#time plot & histogram of forecast errors
plot.ts(ozone_mean_arima$residuals)          

#plotForecastErrors(ozone_mean_arima$residuals) #doesnt work make own histogram
hist(ozone_mean_arima$residuals, 
     main="Histogram of Residuals",
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(ozone_mean_arima$residuals))




#SO2_mean
SO2_mean_arima = arima(dfc_mean_ts$SO2_mean, order=c(0,1,1))
SO2_mean_arima

#https://otexts.org/fpp2/arima-r.html check model is is stationary and invertible by checking the rooks of the polynomials.
#dots in the circles show that the model is good for forecasting
autoplot(SO2_mean_arima)

#95% confidence interval forecast time series. D=1 sets it to seasonal ARIMA, h is how many periods of forecasting
SO2_mean_arima = forecast(auto.arima(ts(dfc_mean_ts$SO2_mean, frequency=12, start=c(2007,1)), D=1), h=36, level=c(95))
plot(SO2_mean_arima)

#correlogram of forecast errors
acf(SO2_mean_arima$residuals, lag.max=120)

#Ljung-box test
Box.test(SO2_mean_arima$residuals, lag=20, type="Ljung-Box")

#time plot & histogram of forecast errors
plot.ts(SO2_mean_arima$residuals)          

#plotForecastErrors(SO2_mean_arima$residuals) #doesnt work make own histogram
hist(SO2_mean_arima$residuals, 
     main="Histogram of Residuals",
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(SO2_mean_arima$residuals))







#Max forecasts-not needed make sense?
#Pollutant Time series-the maxs
#PM10
PM10_arima = arima(dfc_max_ts$PM10, order=c(4,0,1))
PM10_arima

#https://otexts.org/fpp2/arima-r.html check model is is stationary and invertible by checking the rooks of the polynomials.
#dots in the circles show that the model is good for forecasting
autoplot(PM10_arima)

#95% confidence interval forecast time series. D=1 sets it to seasonal ARIMA, h is how many periods of forecasting
PM10_arima = forecast(auto.arima(ts(dfc_max_ts$PM10, frequency=12, start=c(2007,1)), D=1), h=36, level=c(95))
plot(PM10_arima)

#correlogram of forecast errors
acf(PM10_arima$residuals, lag.max=120)

#Ljung-box test
Box.test(PM10_arima$residuals, lag=20, type="Ljung-Box")

#time plot & histogram of forecast errors
plot.ts(PM10_arima$residuals)          

#plotForecastErrors(PM10_arima$residuals) #doesnt work make own histogram
hist(PM10_arima$residuals, 
     main="Histogram of Residuals",
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(PM10_arima$residuals))




#NO2_max
NO2_max_arima = arima(dfc_max_ts$NO2_max, order=c(3,1,1))
NO2_max_arima

#https://otexts.org/fpp2/arima-r.html check model is is stationary and invertible by checking the rooks of the polynomials.
#dots in the circles show that the model is good for forecasting
autoplot(NO2_max_arima)

#95% confidence interval forecast time series. D=1 sets it to seasonal ARIMA, h is how many periods of forecasting
NO2_max_arima = forecast(auto.arima(ts(dfc_max_ts$NO2_max, frequency=12, start=c(2007,1)), D=1), h=36, level=c(95))
plot(NO2_max_arima)

#correlogram of forecast errors
acf(NO2_max_arima$residuals, lag.max=120)

#Ljung-box test
Box.test(NO2_max_arima$residuals, lag=20, type="Ljung-Box")

#time plot & histogram of forecast errors
plot.ts(NO2_max_arima$residuals)          

#plotForecastErrors(NO2_max_arima$residuals) #doesnt work make own histogram
hist(NO2_max_arima$residuals, 
     main="Histogram of Residuals",
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(NO2_max_arima$residuals))



#CO_max
CO_max_arima = arima(dfc_max_ts$CO_max, order=c(1,0,0))
CO_max_arima

#https://otexts.org/fpp2/arima-r.html check model is is stationary and invertible by checking the rooks of the polynomials.
#dots in the circles show that the model is good for forecasting
autoplot(CO_max_arima)

#95% confidence interval forecast time series. D=1 sets it to seasonal ARIMA, h is how many periods of forecasting
CO_max_arima = forecast(auto.arima(ts(dfc_max_ts$CO_max, frequency=12, start=c(2007,1)), D=1), h=36, level=c(95))
plot(CO_max_arima)

#correlogram of forecast errors
acf(CO_max_arima$residuals, lag.max=120)

#Ljung-box test
Box.test(CO_max_arima$residuals, lag=20, type="Ljung-Box")

#time plot & histogram of forecast errors
plot.ts(CO_max_arima$residuals)          

#plotForecastErrors(CO_max_arima$residuals) #doesnt work make own histogram
hist(CO_max_arima$residuals, 
     main="Histogram of Residuals",
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(CO_max_arima$residuals))


#NO_max
NO_max_arima = arima(dfc_max_ts$NO_max, order=c(3,0,3))
NO_max_arima

#https://otexts.org/fpp2/arima-r.html check model is is stationary and invertible by checking the rooks of the polynomials.
#dots in the circles show that the model is good for forecasting
autoplot(NO_max_arima)

#95% confidence interval forecast time series. D=1 sets it to seasonal ARIMA, h is how many periods of forecasting
NO_max_arima = forecast(auto.arima(ts(dfc_max_ts$NO_max, frequency=12, start=c(2007,1)), D=1), h=36, level=c(95))
plot(NO_max_arima)

#correlogram of forecast errors
acf(NO_max_arima$residuals, lag.max=120)

#Ljung-box test
Box.test(NO_max_arima$residuals, lag=20, type="Ljung-Box")

#time plot & histogram of forecast errors
plot.ts(NO_max_arima$residuals)          

#plotForecastErrors(NO_max_arima$residuals) #doesnt work make own histogram
hist(NO_max_arima$residuals, 
     main="Histogram of Residuals",
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(NO_max_arima$residuals))




#ozone_max
ozone_max_arima = arima(dfc_max_ts$ozone_max, order=c(1,0,0))
ozone_max_arima

#https://otexts.org/fpp2/arima-r.html check model is is stationary and invertible by checking the rooks of the polynomials.
#dots in the circles show that the model is good for forecasting
autoplot(ozone_max_arima)

#95% confidence interval forecast time series. D=1 sets it to seasonal ARIMA, h is how many periods of forecasting
ozone_max_arima = forecast(auto.arima(ts(dfc_max_ts$ozone_max, frequency=12, start=c(2007,1)), D=1), h=36, level=c(95))
plot(ozone_max_arima)

#correlogram of forecast errors
acf(ozone_max_arima$residuals, lag.max=120)

#Ljung-box test
Box.test(ozone_max_arima$residuals, lag=20, type="Ljung-Box")

#time plot & histogram of forecast errors
plot.ts(ozone_max_arima$residuals)          

#plotForecastErrors(ozone_max_arima$residuals) #doesnt work make own histogram
hist(ozone_max_arima$residuals, 
     main="Histogram of Residuals",
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(ozone_max_arima$residuals))




#SO2_max
SO2_max_arima = arima(dfc_max_ts$SO2_max, order=c(0,1,1))
SO2_max_arima

#https://otexts.org/fpp2/arima-r.html check model is is stationary and invertible by checking the rooks of the polynomials.
#dots in the circles show that the model is good for forecasting
autoplot(SO2_max_arima)

#95% confidence interval forecast time series. D=1 sets it to seasonal ARIMA, h is how many periods of forecasting
SO2_max_arima = forecast(auto.arima(ts(dfc_max_ts$SO2_max, frequency=12, start=c(2007,1)), D=1), h=36, level=c(95))
plot(SO2_max_arima)

#correlogram of forecast errors
acf(SO2_max_arima$residuals, lag.max=120)

#Ljung-box test
Box.test(SO2_max_arima$residuals, lag=20, type="Ljung-Box")

#time plot & histogram of forecast errors
plot.ts(SO2_max_arima$residuals)          

#plotForecastErrors(SO2_max_arima$residuals) #doesnt work make own histogram
hist(SO2_max_arima$residuals, 
     main="Histogram of Residuals",
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(SO2_max_arima$residuals))



