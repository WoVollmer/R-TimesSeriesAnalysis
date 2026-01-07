# code from file
# D:\Wolfgang\Programs-R\R-TimeSeriesAnalysis\old
# CO2_Mauna_Loa_Analysis_last_save.Rmd
# 
# 
# # https://www.esrl.noaa.gov/gmd/ccgg/trends/
# %>% source: https://www.esrl.noaa.gov/gmd/ccgg/trends/data.html
# 
# Global Monitoring Laboratory (%>% start ~ 1968 ...)
# Download area (also wordlwide stations, different gases):
# https://www.esrl.noaa.gov/gmd/dv/data/index.php?pageID=8&category=Greenhouse%2BGases&site=MLO

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

suppressMessages(library(tidyverse))
suppressMessages(library(magrittr))
library(lubridate)
library(fpp3)  # tsibble, tsibbledata, fable, and feasts packages & some tidyverse
library(stlplus) # Enhanced Seasonal Decomposition of Time Series by Loess
library(DT)      # R interface to the JavaScript library DataTables
library(rlang)

######  util functions
library(pkgTS)
# source("./uts_TimeSeries.R")  # utility functions for time series

city <- "Mauna Loa"

# if (city == "Mauna Loa") {
  header <- FALSE  # header line with column names exist
  sep <- " " # field separator in txt file = blank
  dec <- "."
  skip <- 42 # old: 58, 53 # old: 72 # skip # lines, start with next line and succeeding data
  na_sign <- "-99.99"  # NA value indicator in data source
  file <- "./Data_orig_Temp_Precip_CO2/Mauna_Loa_CO2_Data/co2_mm_mlo.txt"
  # verify if data file exists
  if(!file.exists(file))
    stop("Data file ", file, " not found!")
  
  mauna_loa_co2 <- read_table(file, col_names = header, na = na_sign, skip = skip)
  # names(mauna_loa_co2) <- c("Year", "Month", "year.month", "average",  
  #                           "interpolated", "trend", "#days")
  names(mauna_loa_co2) <- c("Year", "Month", "year.month", "average", 
                            "de-season_alized",  "#days", "st_dev", "uncertain")
  # "average" column:  monthly mean;  Missing months have been interpolated
  #                    no NA = -99.99 since already interpolated,
  # ex "interpolated": average values with interpolation for NAs => no longer separate
  # "de-season alized" (ex. trend): value for each month by removing the seasonal cycle
  #  Year Month year.month average interpolated `trend (season corr)`
  #  1958     3   1958.208 	315.71	     315.71	      314.62
  #  :
  #  2019  	 12	  2019.958	411.76	     411.76	      412.43
  
  ## monthly data - check for gaps and fill w/ NAs as far as needed
  data_monthly <- mauna_loa_co2 %>% 
    mutate(Month = factor(Month)) %>% 
		rename(count = average)  %>% 
    dplyr::select(Year, Month, count)
  levels(data_monthly$Month) <- month.abb # first Month 3 -> Mar correctly assigned
  
  data_monthly <- uts_data_check_and_fill_w_na(data_monthly) %>% 
    mutate(City = city,
           Measure = "CO2")
  
  ## monthly %>% wide 
  ## - adds automatically NAs for months before first month in first year and
  ## - adds automatically NAs for months after  last  month in last year
  data_monthly_wide <- data_monthly  %>% 
    as_tibble() %>% 
    pivot_wider(id_cols = c(Year, City, Measure), 
                names_from = Month, 
                values_from = count) %>% 
    mutate(Measure = factor(Measure)) %>% 
    dplyr::select(City, Measure, Year, all_of(month.abb))
  # to get the right ordering Jan, Feb, ..., Dec
  
  # data_monthly - store with 'completed' first year, now with NA for Jan and Feb
  data_monthly <- data_monthly_wide %>%
    pivot_longer(cols = Jan:Dec, # cols = "1":"12", #
                 names_to = c("Month"),
                 values_to = "count")  %>%
    unite(Year_Month, Year, Month, sep = "-", remove = FALSE) %>% 
    # => keep Year, Month column
    mutate(Year_Month = yearmonth(Year_Month)) %>% 
    as_tsibble(index = Year_Month, key = Measure)

f_name <- paste0( "./Data_rds_Temp_Precip_CO2/", city, "_wide.rds")
saveRDS(data_monthly_wide, f_name )
# data_rds <- readRDS(f_name)



