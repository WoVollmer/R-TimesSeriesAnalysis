# Read data from data file and process them for further analysis

setwd(dirname(rstudioapi::getSourceEditorContext()$path))


library(lubridate)
suppressMessages(library(tidyverse))
suppressMessages(library(magrittr))
library(tsibble)

######  util functions
library(pkgTS)
# source("./uts_climate_not_pkg.R")  # utility functions for time series

city_dwd <- c("Cottbus", "Giessen", "Hohenpeissenberg","Mannheim", "Potsdam")
city_list <- c(city_dwd, "Basel", "Davos", "England") # all
city <- c("Basel")  # selected

# loop for city <- city_list
for (city in city_list) {
  weather_data <- NULL  	# assign/initialize InputData vector to NULL

  
  if (city %in% city_dwd) {
    data_format_source = "DWD"  
  } else if  (city == "Basel" | city == "Davos") {
    data_format_source = "CH"
  } else if  (city == "England") {
    data_format_source = "England"
  } else if (TRUE) {
    stop("No valid city for input data given!")
  }
  
  # DWD Data history & latest: separate zip folders: *_his & *.akt
  # produkt_klima_monat_17810101_20181231_02290.txt 
  #     => for spread sheet reading can be changed to csv
  # with  sep = ";"  & dec ="."
  # dir("./Data_orig_Temp_Precip_CO2/DWD_Weather_Data/")
  
  if (city == "Cottbus") {
    file_his <-
      "./Data_orig_Temp_Precip_CO2/DWD_Weather_Data/Cottbus_klarchiv_00880_month_his/produkt_klima_monat_18881101_20211231_00880.txt"
    file_akt <-
      "./Data_orig_Temp_Precip_CO2/DWD_Weather_Data/Cottbus_klarchiv_00880_month_akt/produkt_klima_monat_20210701_20230131_00880.txt"
  } else if  (city == "Giessen") {
    file_his <- "./Data_orig_Temp_Precip_CO2/DWD_Weather_Data/Giessen_klarchiv_01639_month_his/produkt_klima_monat_18810101_20211231_01639.txt"
    file_akt <- "./Data_orig_Temp_Precip_CO2/DWD_Weather_Data/Giessen_klarchiv_01639_month_akt/produkt_klima_monat_20210701_20230131_01639.txt"
  } else if  (city == "Hohenpeissenberg") {
    file_his <- "./Data_orig_Temp_Precip_CO2/DWD_Weather_Data/Hohenpeissenberg_klarchiv_02290_month_his/produkt_klima_monat_17810101_20211231_02290.txt"
    file_akt <- "./Data_orig_Temp_Precip_CO2/DWD_Weather_Data/Hohenpeissenberg_klarchiv_02290_month_akt/produkt_klima_monat_20210701_20230131_02290.txt"
  } else if  (city == "Mannheim") {
    file_his <-
      "./Data_orig_Temp_Precip_CO2/DWD_Weather_Data/Mannheim_klarchiv_05906_month_his/produkt_klima_monat_18810101_20211231_05906.txt"
    file_akt <-
      "./Data_orig_Temp_Precip_CO2/DWD_Weather_Data/Mannheim_klarchiv_05906_month_akt/produkt_klima_monat_20210701_20230131_05906.txt"
  } else if  (city == "Potsdam") {
    file_his <-
      "./Data_orig_Temp_Precip_CO2/DWD_Weather_Data/Potsdam_klarchiv_03987_month_his/produkt_klima_monat_18930101_20211231_03987.txt"
    file_akt <-
      "./Data_orig_Temp_Precip_CO2/DWD_Weather_Data/Potsdam_klarchiv_03987_month_akt/produkt_klima_monat_20210701_20230131_03987.txt"
  } 
  
  weather_data_file <- paste("./Data_orig_Temp_Precip_CO2/Weather-Data-Input_", 
                             city, ".csv", sep = "")
  
  if (data_format_source == "DWD") {
    # STATIONS_ID;MESS_DATUM_BEGINN;MESS_DATUM_ENDE;...;MO_TT;...;MO_RR;...
    # 2290;17810101;17810131;...  -1.87;...;    42.1;...
    header <- TRUE  # header line with column names exist
    sep <- ";" # field separator in txt file = blank
    dec <- "."
    skip <- 0 # skip no line, start with header line and succeeding data
    na_sign <- "-999"  # NA temperature/precipitation values are indicated by -999
    
    # verify if data file exists
    if(!file.exists(file_his))
      stop("Data file ", file_his, " not found!")
    if(!file.exists(file_akt))
      stop("Data file ", file_akt, " not found!")
    # read historical and latest weather data and replace -999 by NA
    # select MESS_DATUM_BEGINN (YYYYmmdd e.g. 18881101), 
    # Temperature & Precipitation columns
    # MO_TT (Temperature; Monatsmittel der Lufttemperatur in 2m Höhe in C and 
    # MO_RR (Precipitation; Monatssumme der Niederschlagshoehe in mm
    #
    #  and union them w/o overlapping rows
    weather_data <- 
      read_delim(file_his,delim = sep, trim_ws = TRUE, na = na_sign, 
                 col_names = header, skip = skip) %>%  
      dplyr::select(MESS_DATUM_BEGINN, MO_TT, MO_RR)
    weather_data_latest <- 
      read_delim(file_akt,delim = sep, trim_ws = TRUE, na = na_sign, 
                 col_names = header, skip = skip) %>% 
      dplyr::select(MESS_DATUM_BEGINN, MO_TT, MO_RR)
    
    weather_data <- dplyr::union(weather_data, weather_data_latest) %>% 
      rename(Temperature = MO_TT, 
             Precipitation = MO_RR) %>% 
      mutate(  
        # provide separate Year and Month column
        # split date add column $year.month with decimal month values
        #     %Y%m%d     Year       Month     =>  Year       Month  
        #   YYYYmmdd     18881101   18881101  =>  %Y         %m
        Year =
          year(ymd(MESS_DATUM_BEGINN)),
        Month =
          month(ymd(MESS_DATUM_BEGINN))) %>% 
      dplyr::select(Year, Month, Temperature, Precipitation)
    #    Year Month Temperature Precipitation
    #   <dbl> <dbl>       <dbl>         <dbl>
    # 1  1893     1       -8.19          27.7
    # 2  1893     2        1.81          79.6
    
    # check for double lines - as given for Cottbus his data
    double_lines <- 
      nrow(distinct(weather_data)) - nrow(distinct(weather_data, Year, Month))
    if(double_lines != 0)
      stop("Data file ", file_his," has ", double_lines, " lines with same 'Year' and 'Month'!")
    # issue for Cottbus his data => correct/delete in source file:
    # 	1887	4
    # 	1889	3
    # 	1890	3
    # issue for Hohenpeissenber his data: overlapp his and akt data !!
    #  	1887	4 no longer
    #  	1888  11 no longe
    #  	1889  3 no longe
    #  	1890	3 no longe
    # issue for Giessen data: usefull only as of 1939
    #  	1944, 1945, 1946 missing, w/o NA's etc. 

    
    #### replace isolated monthly NA Temperatures by mean temp ####
    # cat("replace isolated NA Temperatures by mean temp value from month before and ahead \n")
    # filter(WeatherData, is.na(Temperature))
    
    weather_data <- weather_data %>%  
      mutate(Temp_avg = (lag(Temperature) + lead(Temperature))/2,
             Temperature = case_when(is.na(Temperature) ~ Temp_avg,
                                     TRUE ~ Temperature)) %>% 
      dplyr::select(-Temp_avg)
    
    # head(slice(WeatherData, 100:120), 20)
    
    # cat("Remaining NA Temperatures - not replaced by mean temp value from month before and ahead \n")
    # filter(WeatherData, is.na(Temperature))
    
    weather_data <- weather_data %>%
      pivot_longer(cols = Temperature:Precipitation,
                   names_to = c("Measure"),
                   values_to = "count")
    if (city == "Giessen") {
      weather_data <- weather_data %>% 
        filter(Year >= 1901)
      weather_data <- weather_data %>%  
        filter(Measure != "Precipitation" | Year >= 1939)
      # Temperature data are starting on 1901-01, 
      # min/max Temperature data already 1881-01, Precipitation just on 1939-01-01
      # results in STL decomposition issue
    }    
    if (city == "Mannheim") {
      weather_data <- weather_data %>%  
        filter(Measure != "Precipitation" | Year >= 1890)
      # Temperature data are starting on 1881-01, Precipitation just on 1890-01-01
      # results in STL decomposition issue
    }
    ############## end DWD data ##########################
    
  } else if (data_format_source == "CH") {
    # Year  Month        Temperature      Precipitation  
    # 1864      1               -5.5               20.6  
    header <- TRUE  # header line with column names exist
    sep <- " " # field separator in txt file = blank
    dec <- "."
    skip <- 27 # skip no line, start with header line and succeeding data
    file <- paste("./Data_orig_Temp_Precip_CO2/CH_Weather_Data/", city, "-Weather-Data.txt", sep = "")
    # verify if data file exists
    if(!file.exists(file))
      stop("Data file ", file, " not found!")
    
    weather_data <- read_table(file, col_names = header, skip = skip)
    
    weather_data <- weather_data %>%
      pivot_longer(cols = Temperature:Precipitation,
                   names_to = c("Measure"),
                   values_to = "count")
    
    ############## end CH data ##########################
    
  } else if (data_format_source == "England") {
    #      JAN  FEB MAR APR MAY   JUN   JUL   AUG   SEP   OCT  NOV   DEC     YEAR
    # 1659 3.0  4.0 6.0 7.0 11.0  13.0  16.0  16.0  13.0  10.0 5.0   2.0     8.87
    file     <- "./Data_orig_Temp_Precip_CO2/England_Weather_Data/England_as_of_1659_meantemp_monthly_totals.txt"
    sep <- "" # field separator in txt file = blank
    header <- FALSE  # header line with column names exist (line 28 in txt file)
    skip <- 5 # skip lines incl. header since "Year" name in header line missing
              # Year - head for years to be added and also month.abb, "avgYEAR"
              # given average YEAR values to be deleted rsp. will not be used
    
    # verify if data file exists
    if(!file.exists(file))
      stop("Data file ", file, " not found!")
    # read txt data file
    # weather_data <- read_table(file, col_names = header, skip = skip) %>% 
    #   rename(Year = X1) 
    # names(weather_data) <- c("Year", month.abb, "avgYEAR") # replace names
    
    # jump directly to data line and add column names with character vector
    weather_data <- read_table(file, col_names = c("Year", month.abb, "avgYEAR"), 
                               skip = skip) %>% 
      mutate(Measure = "Temperature")
    
    weather_data %<>% dplyr::select(Year, Measure, Jan:Dec)
    
    # weather_data_old <- weather_data
    weather_data <- weather_data %>%
      pivot_longer(cols = Jan:Dec, # cols = "1":"12", #
                   names_to = c("Month"),
                   values_to = "count")

    ############## end England data ##########################
  }
  
  
  if (data_format_source == "DWD" | data_format_source == "CH") {
    weather_data <- weather_data %>% 
      mutate(Month = factor(Month))
    levels(weather_data$Month) <- month.abb 
    # first Month 3 -> Mar correctly assigned
  }
  
  data_monthly <- weather_data %>% 
    uts_data_check_and_fill_w_na(key = "Measure") %>%
    mutate(City = city)    
  # uts_data_check_and_fill_w_na(add_precip = FALSE, key = "Measure") 
  
  # data_yearly_seasonal <- uts_gen_yearly_seasonal_avg(data_monthly)
  # 

  data_monthly <- data_monthly %>%       
    mutate(  
    # provide separate Year and Month column, to eleminate NA's for year or month
    Year = year(Year_Month),
    Month = month(Year_Month)) %>% 
    mutate(Month = factor(Month))
  levels(data_monthly$Month) <- month.abb 
  
  data_monthly_wide <- data_monthly  %>% 
    as_tibble() %>% 
    pivot_wider(id_cols = c(Year, City, Measure), 
                names_from = Month, 
                values_from = count) %>% 
    dplyr::select(City, Measure, Year, all_of(month.abb))
  # to get the right ordering Jan, Feb, ..., Dec
  
  data_monthly_wide$Measure <- 
    factor(data_monthly_wide$Measure, levels =  c("Temperature", "Precipitation"))
  # change level order: relevel(data_new$Measure, ref = "Temperature")
  # rename levels:      levels(data_new$Month) <- month.abb
  data_monthly_wide %<>% arrange(Measure)
  
  f_name <- paste0( "./Data_rds_Temp_Precip_CO2/", city, "_wide.rds")
  saveRDS(data_monthly_wide, f_name)
  # data_rds <- readRDS(f_name)
  
  # data_monthly_wide <- left_join(data_monthly_wide, data_yearly_seasonal)
  # data_monthly <- uts_gen_yearly_seasonal_avg(data_monthly)
  # data_monthly <- data_monthly %>% 
  #   pivot_longer(cols = Jan:Dec,
  #                names_to = c("Month"),
  #                values_to = "count") %>% 
  #   dplyr::select(Year, Month, count, City, Measure)
  #   uts_gen_yearly_seasonal_avg(data_monthly)
  
  # Year Measure   Jan   Feb  ... Dec Year_avg Winter_avg Spring_avg Summer_avg
  # <dbl> <fct>       <dbl> <dbl> ... <dbl>  <dbl>      <dbl>      <dbl>      <dbl>
  # 1887 Temperature     NA    NA  ... NA     NA         NA         NA         NA  
  # :
  # 1889 Temperature    -3.1 -2.09 ...-1.01   8.28      -1.27      8.97       18.4
  
  # csv format: "." for the decimal point, comma for the separator
  # write.csv(data_monthly_wide, weather_data_file,
  #           row.names = FALSE,
  #           fileEncoding = "utf8")
  
}

##### for csv2 format tests only, write.csv2
#     write.csv2 uses a comma for the decimal point and a semicolon for the
# write.csv2(data_monthly, paste(input_data_file, "2.csv", sep = ""),
#           row.names = FALSE,
#           fileEncoding = "utf8")

## or with tidyverse readr package function
# write_excel_csv2(data_monthly, weather_data_file)




