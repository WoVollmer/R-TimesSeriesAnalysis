# Time Series Features
## Simple statistics & ACF & STL

data_monthly %>% features(count, mean)
# # A tibble: 2 x 2
#   Measure      V1
#   <chr>         <dbl>
# 1 Precipitation 66.0 
# 2 Temperature    9.50

data_monthly  %>% features(count, mean)

data_monthly %>% features(count, feat_acf)
data_monthly %>% features(count, feat_stl)
data_monthly %>% features(count, feat_spectral) 
# entropy close to 0 => series has strong trend and seasonality (=> easy to forecast)
# entropy close to 1 => series is very noisy (and so is difficult to forecast) 

plot_1 <- data_monthly %>% features(count, feat_acf) %>%
  ggplot(aes(x=acf1, y=season_acf1, col=Measure)) +
  geom_point() + facet_wrap(vars(Measure)) +
  theme(legend.position = "none") +
  ggtitle("Seasonal ACF lag1 vs. ACF lag1")
plot_2 <- data_monthly %>% features(count, feat_stl) %>%
  ggplot(aes(x=trend_strength, y=seasonal_strength_year, col=Measure)) +
  geom_point() + facet_wrap(vars(Measure)) +
  theme(legend.position = "none") +
  ggtitle("Seasonal vs. Trend Strength")
gridExtra::grid.arrange(plot_1, plot_2)

# Arima Models
## Stationarity and differencing

data_monthly %>% features(count, unitroot_nsdiffs)
#  returns 1 => one seasonal difference is required for stationarity

data_monthly %>% 
  mutate(diff_lag_12_count = difference(count, 12)) %>% 
  features(diff_lag_12_count, unitroot_ndiffs)
# returns 0 =>  no further one step differencing required for stationarity
# 

data_monthly %>% ACF(count) %>% autoplot()
data_monthly %>%
  mutate(diff_count = difference(count, 1)) %>% ACF(diff_count) %>% autoplot()
data_monthly %>%
  mutate(diff_count = difference(count, 7)) %>% ACF(diff_count) %>% autoplot()

data_monthly %>%
  mutate(diff_count = difference(count, 12)) %>% ACF(diff_count) %>% autoplot() 
as_tibble(data_monthly) %>%
  mutate(diff_count = difference(count, 12)) %>% summarise(Sum = sum(diff_count, na.rm = TRUE))

# null hypothesis of independence in a given time series
#                 => h0 ablehnen, für p < alpha
# p-value < 0.05 => null hypothesis to be rejected 
# => data in the given time series are dependent
#    => differenced data are needed to get stationary
data_monthly %>%
  mutate(diff_count = difference(count, 12)) %>%
  features(diff_count, ljung_box, lag = 10)




## Non-seasonal ARIMA for Seasonally adjusted data

data_dcmp <- data_monthly %>%
  model(STL(count ~ season(window="periodic"))) %>%
  components() %>%
  # select(-.model) %>%
  as_tsibble()
data_dcmp %>%
  autoplot(season_adjust)

data_dcmp %>%
  gg_tsdisplay(difference(season_adjust), plot_type='partial')

fit <- data_dcmp %>%
  model(
    arima = ARIMA(season_adjust ~ pdq(3,1,1) + PDQ(0,0,0))
  )

report(fit)
fit %>% gg_tsresiduals()

augment(fit) %>%
  features(.resid, ljung_box, lag = 24, dof = 4)

fit %>% forecast() %>% autoplot(data_dcmp) + 
  xlim(yearmonth("2010"), yearmonth("2025"))

gg_arma(fit)


## Seasonal ARIMA

data_monthly %>% autoplot(count)

# data are non-stationary, with seasonality 
# => first take a seasonal difference
data_monthly %>% gg_tsdisplay(difference(count, 12), plot_type='partial')

# if seasonally differenced data appear to be non-stationary
# => take an additional first seasonal difference
#    note: for data_monthly it's worthening
data_monthly %>% gg_tsdisplay(count %>% difference(12) %>% difference(),
                              plot_type='partial')

# find an appropriate ARIMA model based on the ACF and PACF
fit <- data_monthly %>%
  model(arima = ARIMA(count ~ pdq(0,1,2) + PDQ(0,1,2)))
fit %>% gg_tsresiduals(lag_max=36)

report(fit)   # AICc=7559.2 smaller => better for pdq(0,1,2) instead pdq((0,1,1)

augment(fit) %>%
  features(.resid, ljung_box, lag = 36, dof = 6)
# There are a few significant spikes in the ACF
# but the model does not fails the Ljung-Box test (P = 0.228 > 0.05)

fit %>% forecast(h=144) %>% autoplot(data_monthly) + 
  xlim(yearmonth("2010"), yearmonth("2025"))


data_monthly %>%       #  automatically select pdq() and PDQ()
  model(ARIMA(count))  #  => <ARIMA(2,0,0)(1,1,0)[12]>

fit <- data_monthly %>%
  model(arima = ARIMA(count ~ pdq(2,0,0) + PDQ(1,1,0)))
fit %>% gg_tsresiduals()
report(fit)   #     AICc=8219 worsening !! => increasing connfidence ranges
# old AICc=7559.2 better W/ pdq(0,1,2) $PDQ((0,1,1)

fit %>% forecast(h=144) %>% autoplot(data_monthly) + 
  xlim(yearmonth("2010"), yearmonth("2025"))



## Comparing ARIMA() and ETS() on seasonal data
# Consider the data beginning in 1901
data_20_century <- data_monthly %>%
  filter(year(Year_Month) >= 1901)

# Use first 30 years of the data as the training set
# train <- data_20_century  %>%
#   filter(year(Year_Month) <= 1930)
train <- data_20_century  %>%
  filter(year(Year_Month) >= 1971 & year(Year_Month) <=2000 )

# fit_arima <- train %>% model(ARIMA(count))
# automatically select pdq() and PDQ() provides 
# Model: ARIMA(0,0,1)(1,1,1)[12] w/ AICc=1406.62
fit_arima <- train %>% model(arima = ARIMA(count ~ pdq(0,1,2) + PDQ(0,1,2)))
# Model: ARIMA(0,1,2)(0,1,2)[12] w/ AICc=1397.99
report(fit_arima)

gg_tsresiduals(fit_arima, lag_max = 24) + ggtitle("fit_arima")
augment(fit_arima) %>%
  features(.resid, ljung_box, lag = 24, dof = 5)

# fit_ets <- train %>% model(ETS(count))
# automatically select ETS() provides  
# Model: ETS(A,N,A) w/ AICc 2515.437
fit_ets  <- train %>%
  model(ETS(count ~ error("A") + trend("A") + season("A")))
# Model: ETS(A,A,A) w/ AICc 2521.208
report(fit_ets)

gg_tsresiduals(fit_ets, lag_max = 24) + ggtitle("fit_ETS")
augment(fit_ets) %>%
  features(.resid, ljung_box, lag = 24, dof = 6)

# Generate forecasts and compare accuracy over the test set
bind_rows(
  fit_arima %>% accuracy(),
  fit_ets %>% accuracy(),
  fit_arima %>% forecast(h = "10 year") %>%
    accuracy(data_20_century),
  fit_ets %>% forecast(h = "10 years") %>%
    accuracy(data_20_century)
)
# output evaluates the forecasting performance of the two competing models
#   over the test set. ARIMA model seems to be the slightly more accurate model
#   based on the smaller values of test set RMSE, MAPE (here: INF) and MASE
#   
# ETS model fits the training data slightly better than the ARIMA model, 
# but that the ARIMA model provides more accurate forecasts on the test set
# A good fit to training data is never an indication that the model will 
# forecast well. 
# 
# Below we generate and plot forecasts from an ETS model for the next 10 years.
# Generate forecasts from an ETS model
data_20_century  %>% model(ARIMA(count)) %>% forecast(h="10 years") %>%
  autoplot(data_20_century) +
  xlim(yearmonth("2010"), yearmonth("2030")) +
  labs(title = "Forecast with ARIMA Model")

data_20_century %>% model(ETS(count)) %>% forecast(h="10 years") %>%
  autoplot(data_20_century) +
  xlim(yearmonth("2010"), yearmonth("2030")) +
  labs(title = "Forecast with ETS Model")

data_20_century  %>% 
  model(arima = ARIMA(count ~ pdq(0,1,2) + PDQ(0,1,2))) %>% 
  forecast(h="10 years") %>%
  autoplot(data_20_century) +
  xlim(yearmonth("2010"), yearmonth("2030")) +
  labs(title = "Forecast with fixed Model ARIMA(0,1,2)(0,1,2)[12]")

data_20_century %>% 
  model(ETS(count ~ error("A") + trend("A") + season("A"))) %>% 
  forecast(h="10 years") %>%
  autoplot(data_20_century) +
  xlim(yearmonth("2010"), yearmonth("2030")) +
  labs(title = "Forecast with fixed Model ETS(A,A,A)")




## else
## taken over - as far as needed taken over
 
############################# Year_avg, Winter_avg
#############################

slice(group_by(data_yearly, City, Measure), 1:6)
slice(group_by(data_monthly, Measure), 1:6)
group_vars(data_yearly)

data <- data_monthly %>% group_by(City, Measure)
slice(data, 1:6)
group_vars(data)  # returns a character vector
groups(data)      # returns a list of symbols 
key_vars(data) # returns a character vector
key(data)      # returns a list of symbols 

data %<>% group_by(City, Measure) %>%  mutate(count_lag = lag(count))
data %>% summarise(Mean = mean(count))
slice(data, 1:6)
data %>% index_by(yearquarter())

first_last_year <- 
  as_tibble(data_monthly) %>% 
  group_by(City, Measure) %>% 
  summarise(mean = mean(count), n = n(),
            first_year = min(Year),
            last_year = max(Year))

# mean of month over all years grouped by (Month, City, Measure)
mean_monthly <- data_monthly %>%  
  index_by(Month = ~ month(.)) %>%  
  as_tibble() %>%   
  mutate(Month = factor(Month)) %>% 
  group_by(Month, City, Measure) %>% 
  summarise(Month_avg = mean(count))
levels(mean_monthly$Month) <- month.abb  # rename levels 1->Jan, ...
slice(mean_monthly, 1:6)

mean_yearly <- data_monthly %>%
  index_by(Year = ~ year(.)) %>%
  as_tibble() %>% 
  group_by(Measure, City, Year) %>% 
  summarise(Year_avg = mean(count))  # na.rm = TRUE not feasible for mean(year)
mean_yearly  # datatable(mean_yearly)
# slice(mean_yearly, 1:6)

mean_season <- data_monthly %>%  
  group_by(City, Measure) %>%  
  mutate(count_lag = lag(count)) %>% 
  index_by(Year_Season = ~ yearquarter(.)) %>%  
  as_tibble() %>% 
  group_by(Measure, City, Year_Season) %>% 
  summarise(Season_avg = mean(count_lag)) %>% 
  mutate(Year = year(Year_Season),
         Season = quarter(Year_Season))
# slice(mean_season, 1:6)

mean_season_wide <- mean_season %>%
  pivot_wider(id_cols = c(Year, Measure, City),
              names_from = Season,
              values_from = Season_avg) %>%
  rename(Winter_avg = '1', Spring_avg = '2', Summer_avg = '3', Fall_avg = '4')
# slice(mean_season_wide, 1:6)

data_yearly <- inner_join(mean_season_wide, mean_yearly)
slice(data_yearly, 1:6)


################################ double  for loop

if (n_keys(data_monthly) == 1 | length(key(data_monthly)) == 2) { 
  for (i in unique(key_data(data_monthly)[[1]])) {     # City
    for (j in unique(key_data(data_monthly)[[2]])) {   # Measure
      
      data_filter <- filter(data_monthly, City == i, Measure == j)
      ## actions
    }
  }
} else {
  abort(paste("nothing implemented for n_keys/key_data/key(data_monthly) =", 
              n_keys(data_monthly), key_data(data_monthly), (key(data_monthly))))
}

