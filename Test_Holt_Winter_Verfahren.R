## Holt-Winters Filtering HoltWinters() {stats}
## 
## 
require(tidyverse)
require(magrittr)
require(forecast)
library(tsibble)
library(feasts)

city <- "Basel"
# weather_data_wide <- 
#   readRDS(paste0("../R-WeatherAnalysis/WeatherData_", city, ".rds")) %>% 
#   filter(Temp_Precip == "Temperature") %>% 
#   dplyr::select(Year, Jan:Dec)
WeatherData <- readRDS(paste0("../R-WeatherAnalysis/WeatherData_", city, ".rds"))
# CO2_data_wide <- readRDS("./Data_MaunaLoa_CO2/CO2_MaunaLoa.rds")

WeatherData_Temp_Precip_yearly <- WeatherData %>%
  dplyr::select(Year, Temp_Precip, Year_avg)

WeatherData_Temp_Precip_monthly <- WeatherData %>%
  pivot_longer(cols = Jan:Dec,
               names_to = c("Month"),
               values_to = "count") %>% 
  dplyr::select(Year, Month, Temp_Precip, count)

monthly_data <- FALSE  # !monthly_data => yearly
temperature <- TRUE   # !temperature data => Percipitation

if (monthly_data & temperature) {
  data <- WeatherData_Temp_Precip_monthly %>% filter(Temp_Precip == "Temperature")
  freq = 12
} else if (!monthly_data & temperature) {
  data <- WeatherData_Temp_Precip_yearly %>% filter(Temp_Precip == "Temperature")
  data %<>% rename(count = Year_avg)
  freq = 1
} else {stop("considering Percipitation comes later")}

data %<>% filter(Year >= 1970) 
nrow(data)
# first_full_year <- data %>%  filter(Month == "Jan") %$% min(Year)
# last_full_year <-  data %>%  filter(Month == "Dec") %$% max(Year)
first_full_year <- data %$% min(Year)
last_full_year <-  data %$% max(Year)
last_full_year - first_full_year + 1

# check for missing year data
all.equal(nrow(data), (last_full_year - first_full_year + 1)*freq)

# !! Year Spalte muss weg !! 
# !! kommt im ts-object via start = first year und frequncy rein !
data_ts <- ts(as.vector(data$count), start = first_full_year, end = last_full_year,
                   frequency = freq)
head(data_ts)

####### Holt-Winters Filtering - Exponential Smoothing 
# w/ & w/o trend and w/ & w/o seasonal component
# alpha parameter of Holt-Winters Filter
# beta parameter of Holt-Winters Filter, beta=FALSE => exponential smoothing
# beta=gamma=FALSE =>
#             exponential smoothing without trend and without seasonal component
# beta=NULL (default), gamma=FALSE =>          
#             exponential smoothing with trend and without seasonal component  
# gamma for the seasonal component, gamma=FALSE => non-seasonal model
# 
# Beispiel 2.33    
# y <- scan("./Data_Schlittgen/hsv.dat")
# y <- ts(y,start=1964)
# out <- HoltWinters(y,alpha=NULL,beta=FALSE,gamma=FALSE)  
# p <- predict(out, n.ahead=4)
# plot(out,p)    

# !! wenn zwei "Datenreihen" im ts-object vorhandens sind, wird auch über beide
# !! extrapoliert, also auch wenn die Year Spalte extra vorhanden ist

############### HoltWwinters w/o trend & w/o season
(data_hw_exp <- HoltWinters(data_ts, beta=FALSE, gamma=FALSE)) # no trend & no season
plot(data_hw_exp)
plot(data_hw_exp, xlim=c(1864, 1874))
plot(data_hw_exp, xlim=c(1990, 2019))
length(data_ts) - nrow(data_hw_exp$fitted)
head(data_hw_exp$fitted)
plot(fitted(data_hw_exp))

head(data_hw_exp$x)                    # orig data
head(data_hw_exp$fitted[, "xhat"])     # filtered data


############### with Trend
data_hw_exp_T <- HoltWinters(data_ts, gamma=FALSE) # black line = observed/counted
data_hw_exp_T
length(data_ts) - nrow(data_hw_exp_T$fitted)
head(data_hw_exp_T$x)                    # orig data
head(data_hw_exp_T$fitted[, "xhat"])     # filtered data
data_hw_exp_T$SSE      # sum-of-squared-errors for the in-sample forecast errors

plot(data_hw_exp_T) # original time series: black line
                    # forecasted values: red line = xhat = level + trend + (no) season
plot(data_hw_exp_T, xlim=c(1864, 1874))
plot(data_hw_exp_T, xlim=c(1990, 2019)) 
lines(fitted(data_hw_exp_T)[,2], col = "orange") 
                 # orange line = level (exp filter) - xhat - trend - (no) season 
lines(fitted(data_hw_exp)[,1], col = "green") # green line = xhat from fitted w/o trend 
head(data_hw_exp_T$fitted)
plot(fitted(data_hw_exp_T))
predict(data_hw_exp, n.ahead = 5, prediction.interval = TRUE)


head(data_hw_exp_T$x)                    # orig data
head(data_hw_exp_T$fitted[, "xhat"])     # filtered data
library(forecast)
data_ets <- ets(data_ts)
plot(data_ets)
plot(forecast(data_ets))
predict(data_ets, n.ahead = 5, prediction.interval = TRUE)

### Welcome to a Little Book of R for Time Series! #############################
# https://a-little-book-of-r-for-time-series.readthedocs.io/

### skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)



skirtsseries <- ts(skirts,start=c(1866))

# skirtsseries <- data_ts # ts from MaunaLoa 1958 -2019

plot.ts(skirtsseries)

skirtsseriesforecasts <- HoltWinters(skirtsseries, gamma=FALSE)
plot(skirtsseriesforecasts)

skirtsseriesforecasts2 <- ets(skirtsseries)
skirtsseriesforecasts_ETS <- ETS(skirtsseries)
plot(forecast(skirtsseriesforecasts2, h = 19)) # predict 19 data points

acf(skirtsseriesforecasts2$residuals, lag.max=20)
Box.test(skirtsseriesforecasts2$residuals, lag=20, type="Ljung-Box")

plot(skirtsseriesforecasts2$residuals)            # make a time plot
source("./ggts_TimeSeries.R")
source("./uts_TimeSeries.R")  # utility functions for time series
plotForecastErrors(skirtsseriesforecasts2$residuals) # make a histogram

fit <- as_tsibble(skirtsseries) %>%  
  model(ETS())
report(fit)

as_tsibble(skirtsseries) %>%  
  model(ETS()) %>% 
  report(fit) %>% 
  components()

test <- residuals(fit)

components(fit)

components(fit) %>%
  autoplot() +
  ggtitle("ETS() components")

residuals(fit)
residuals(fit, type = "response")


ggts_histo_forecast_resid(residuals(fit)) # make a histogram

skirtsseriesdiff1 <- diff(skirtsseries, differences=1)
plot.ts(skirtsseriesdiff1)

##########################################################################

plot.ts(data_ts)
data_ts1 <- diff(data_ts, differences=1)
plot.ts(data_ts1)
data_hw_exp_T 
plot(data_hw_exp_T)              # red line = xhat = level + trend + (no) season

data_ets <- ets(data_ts)
plot(data_ets)
plot(forecast(data_ets, h = 19))  # predict 19 data points
predict(data_hw_exp_T, n.ahead = 5, prediction.interval = TRUE)

acf(data_ets$residuals, lag.max=20) # correlogram: the sample autocorrelation 
    # for the in-sample forecast errors does not exceeds the significance bounds
Box.test(data_ets$residuals, lag=20, type="Ljung-Box")
# Ljung-Box test: p-value = 0.899 => there is little evidence of 
#    non-zero autocorrelations in the in-sample forecast errors at lags 1-20.

plot.ts(data_ets$residuals)            # make a time plot
plotForecastErrors(data_ets$residuals)

data_diff1 <- diff(data_ts, differences=1)
plot.ts(data_diff1)

## !! Formal tests for stationarity ###############
y <- data_diff1
out <- statcheck(y,3)
a<- acf(y,lag.max=15,lwd=3,plot=F)
plot(0:15,a$acf,type="l",lwd=2,xlab="Lag",ylab="ACF")
lines(0:12,out$ac[,2],lty=2) # since length(out$ac[,2]) = 13 => 0:12
lines(0:12,out$ac[,3],lty=2)
lines(0:12,out$ac[,4],lty=2)
lines(c(0,15),c(0,0)) 

data_diff2 <- diff(data_ts, differences=2)
plot.ts(data_diff2)
## !! Formal tests for stationarity ###############
y <- data_diff2
source("./Data_Schlittgen/tsutil.R")
out <- statcheck(y,3)
a <- acf(y,lag.max=15,lwd=3,plot=F)
plot(0:15,a$acf,type="l",lwd=2,xlab="Lag",ylab="ACF")
lines(0:12,out$ac[,2],lty=2)  # since length(out$ac[,2]) = 13 => 0:12
lines(0:12,out$ac[,3],lty=2)
lines(0:12,out$ac[,4],lty=2)






############## with Trend and Season
# if yearly data => not working, yearly data have no season, freq = 1
if (freq > 1) {
  (data_hw_exp_TS <- HoltWinters(data_ts)) # w/ Trend & Season
  length(data_ts) - nrow(data_hw_exp_TS$fitted) # w/o first season values
  plot(data_hw_exp_TS)
  plot(data_hw_exp_TS, xlim=c(1864, 1874))
  plot(data_hw_exp_TS, xlim=c(1990, 2019))
  head(data_hw_exp_TS$fitted)
  plot(fitted(data_hw_exp_TS))
  head(data_hw_exp_TS$x)                    # orig data
  head(data_hw_exp_TS$fitted[, "xhat"])     # filtered data
  
  predict(data_hw_exp_TS, n.ahead = 25, prediction.interval = TRUE)
}

data_monthly %>% 
  filter(Temp_Precip == "Temperature") %>% 
  model(additive = ETS(count ~ error("A") + trend("A") + season("A")),
        multiplicative = ETS(count ~ error("M") + trend("A") + season("M"))) %>% 
  components() %>%
  autoplot() + theme(legend.position = "bottom") 

data_ets_add <- data_monthly %>% 
  filter(Temp_Precip == "Temperature") %>% 
  model(additive = ETS(count ~ error("A") + trend("A") + season("A"))) 
data_ets_add %>% forecast(h = "5 years") %>% 
  autoplot(data_ets_add) + theme(legend.position = "bottom") 


data_model <- data_monthly %>%
  filter(Temp_Precip == "Temperature")

# error(method = c("A", "M") : error term form: additive ("A"), multiplicative ("M")
# trend(method = c("N", "A", "Ad") : trend term form: none ("N"), additive ("A"), multiplicative ("M") or damped (gedämpft) variants ("Ad", "Md").
# season(method = c("N", "A", "M") : 
#   form of seasonal term: none ("N"), additive ("A"), multiplicative ("M")
data_ets_add_mult <- data_model %>%  model(
  add_ANN = ETS(count ~ error("A") + trend("N") + season("N")),    # s=N: no season
  add_ANA = ETS(count ~ error("A") + trend("N") + season("A")),    # T=N: expon. => no trend
  add_AAA = ETS(count ~ error("A") + trend("A") + season("A")),      # best behaviour
  add_AAdA = ETS(count ~ error("A") + trend("Ad") + season("A")),  # too damped, like N
  # add_AMA = ETS(count ~ error("A") + trend("M") + season("A")),      # little too much trend
  # add_AMdA = ETS(count ~ error("A") + trend("Md") + season("A")),  # very damped, like N
  add_AAM = ETS(count ~ error("A") + trend("A") + season("M")),    # s=M: läuft weg
  mult_MNA = ETS(count ~ error("M") + trend("N") + season("A")),   # T=N: expon. => no trend
  mult_MAA = ETS(count ~ error("M") + trend("A") + season("A")),   # e=M: läuft nach oben weg
  # mult_MMA = ETS(count ~ error("M") + trend("M") + season("A")),   # e=M: läuft nach unten weg
  mult_MAM = ETS(count ~ error("M") + trend("A") + season("M")),   # s=M: explodiert
  mult_MAdA = ETS(count ~ error("M") + trend("Ad") + season("A"))
)
components(data_ets_add_mult) # provides count, level, slope, season, remainder
                            # starts one year ahead (one full season) with seasonal start values
                            # starts in Dec of ahead year with level 0
augment(data_ets_add_mult)  # provides data tsibble with count, .fitted, .resid
tidy(data_ets_add_mult)     # provides term estimates
accuracy(data_ets_add_mult) # provides RMSE, MAE, ..
                            # => performance of the models with lowest RMSE, MAE
glance(data_ets_add_mult)   # provides AIC, AICc,  BIC, MSE, MAE
                    # => performance of the models with lowest AICc, MSE/CV, MAE
report(data_ets_add_mult)   # provides AIC, BIC, but only for single models
data_ets_add_mult$add_AAA   # get model of model-name
data_ets_add_mult$add_ANA

data_ets_add_mult <- data_model %>%  
  model(add_AAA = ETS(count ~ error("A") + trend("A") + season("A")))

data_monthly %>% gg_tsdisplay(count)
data_ets_add_mult %>% gg_tsresiduals()
data_ets_add_mult %>% filter(Temp_Precip == "Temperature") %>% gg_tsresiduals()
data_ets_add_mult %>% filter(Temp_Precip == "Precipitation") %>% gg_tsresiduals()

report(data_ets_add_mult)   # provides AIC, BIC, but only for single models
data_forec <- data_ets_add_mult %>% forecast(h = "15 years")
filter(data_forec, Year_Month == yearmonth("2034 Dec"))

data_forec %>%
  autoplot(data_model, level = NULL) + # level: => no conf level
  geom_line(aes(y = .fitted, colour = .model), data = augment(data_ets_add_mult)) +
  facet_wrap(vars(.model), ncol = 1, scales = "free") +
  xlab("Year") +
  ylab("Temperature / Grad Celsius") +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  xlim(yearmonth(2010), yearmonth(2035))

################### group_by() => index_by() ###################################
as_tibble(data_monthly) %>% 
  filter(Year_Month >= yearmonth("2015")) %>% 
  group_by(Temp_Precip, Year) %>% summarise(Mean = mean(count))
data_monthly %>%  
  # index_by() is the counterpart of group_by() in temporal context, 
  # but it only groups the time index. 
  filter(Year_Month >= yearmonth("2015")) %>%  # & Temp_Precip == "Temperature"
  index_by(Yearly = ~ year(.)) %>% 
  group_by(Temp_Precip) %>% 
  summarise(Mean = mean(count))

as_tibble(data_monthly) %>% 
  filter(Year_Month >= yearmonth("2015")) %>%  # & Temp_Precip == "Temperature"
  group_by(Temp_Precip, Month) %>% summarise(Mean = mean(count))
data_monthly %>%  
  # index_by() is the counterpart of group_by() in temporal context, 
  # but it only groups the time index. 
  filter(Year_Month >= yearmonth("2015")) %>%  # & Temp_Precip == "Temperature"
  index_by(Monthly = ~ month(.)) %>% 
  group_by(Temp_Precip) %>% 
  summarise(Mean = mean(count))
##################################################################

as_tibble(data_forec) %>% 
  group_by(year(Year_Month)) %>% summarise(Mean = mean(count))

tail(data_monthly)  # starting with: 1864 Jan 
# 5   2019 Nov  2019 Nov     6.3 Temperature
# 6   2019 Dez  2019 Dec     4.9 Temperature

tail(augment(data_ets_add_mult))
# Temp_Precip   .model  Year_Month count .fitted .resid
# 5 Temperature add_AMA   2019 Nov   6.3    6.98 -0.681
# 6 Temperature add_AMA   2019 Dez   4.9    3.91  0.992
tail(filter(augment(data_ets_add_mult), .model == "add_AAA"))
# 5 Temperature add_AAA   2019 Nov   6.3    6.87 -0.565
# 6 Temperature add_AAA   2019 Dez   4.9    3.77  1.13 

filter(data_forec, Year_Month == yearmonth("2020 Nov") | Year_Month == yearmonth("2020 Dec"))
# 1 Temperature add_AAA   2020 Nov  6.93 N(6.9, 3.4)    
# 2 Temperature add_AAA   2020 Dez  3.85 N(3.9, 3.4)    
# 3 Temperature add_AMA   2020 Nov  7.04 sim(=dbl[5000])
# 4 Temperature add_AMA   2020 Dez  4.00 sim(=dbl[5000])

filter(data_forec, Year_Month == yearmonth("2034 Nov") | Year_Month == yearmonth("2034 Dec"))
# 1 Temperature add_AAA   2034 Nov  7.57 N(7.6, 4.4)    
# 2 Temperature add_AAA   2034 Dez  4.50 N(4.5, 4.4)    
# 3 Temperature add_AMA   2034 Nov  7.79 sim(=dbl[5000])
# 4 Temperature add_AMA   2034 Dez  4.75 sim(=dbl[5000])

as_tibble(data_monthly) %>% 
  filter(Year_Month >= yearmonth("2015")) %>%  # & Temp_Precip == "Temperature"
  group_by(Temp_Precip, Month) %>% summarise(Mean = mean(count))
# 10 Nov    6.58
# 3 Dec     3.9 
#  count_2019 Mean_last_5y AAA_2019  AMA_2019 AAA_2020 AMA_2019 AAA_2034 AMA_2034 AAA_2049  AMA_2049
# Nov     6.3         6.58     6.87      6.98     6.93     7.04     7.57     7.79     8.26      8.65
# Dec     4.9         3.9      3.77      3.91     3.85     4.00     4.50     4.75     5.19      5.61

Beschlaege = c(13317, 12930, 11643, 13098, 12223, 12161, 13230, 14065) 
Beschlaege 
# [1] 13317 12930 11643 13098 12223 12161 13230 14065 
(Beschlaege <- ts(Beschlaege, start = 2000, end = 2007, frequency =1)) 
(Beschlaege.es1 <- HoltWinters(Beschlaege, beta=FALSE, gamma=FALSE))

(Beschlaege.es1f = Beschlaege.es1$fitted)

(Beschlaege.es1f = Beschlaege.es1$fitted[,1])

(Beschlaege.es1p = predict(Beschlaege.es1, n.ahead=8))
