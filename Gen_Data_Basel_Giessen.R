
gen_data_basel_giessen <- function() {
  city <- "Basel_Giessen"
  if (city == "Basel_Giessen") {
    city <- "Basel"
    data_monthly_wide <- 
      readRDS(paste0("../R-WeatherAnalysis/WeatherData_", city, ".rds")) %>% 
      mutate(City = city)
    city <- "Giessen"
    data_monthly_wide_2 <- 
      readRDS(paste0("../R-WeatherAnalysis/WeatherData_", city, ".rds"))  %>% 
      mutate(City = city)  
    data_monthly_wide <- bind_rows(data_monthly_wide, data_monthly_wide_2) %>% 
      filter(Year >= 1960 & Year <= 2019) %>% 
      rename(Measure = Temp_Precip)
    # saveRDS(data_monthly_wide, "../R-WeatherAnalysis/WeatherData_Basel_Giessen.rds")
    # readRDS("../R-WeatherAnalysis/WeatherData_Basel_Giessen.rds")
    # if (temperature) {
    #   data_monthly_wide %<>% filter(Measure == "Temperature") %>% 
    #     dplyr::select(Year, Jan:Dec)
    # } else {
    #   data_monthly_wide %<>% filter(Measure == "Precipitation") %>% 
    #     dplyr::select(Year, Jan:Dec)
    # }
    
    weather_test_w_na <- FALSE
    if (weather_test_w_na) {
      test_monthly_wide <- filter(data_monthly_wide, Year >= 2000 & Year != 2009)
      # note:   & Year & Year != 1949 not feasible, otherwise ts() indexing wrong
      #   running from 1940 - 1953 instead 1954 if 1949 is taken out
      test_monthly_wide[1, 9:11] <- NA
      test_monthly_wide[2, 6:7] <- NA
      test_monthly_wide[2, 12] <- NA
      
      data_monthly_wide <- test_monthly_wide
    }
    data_monthly <- data_monthly_wide %>% 
      pivot_longer(cols = Jan:Dec,
                   names_to = c("Month"),
                   values_to = "count") 
    # %>% 
    #  dplyr::select(Year, Month, count)
    
    data_monthly_complete <- uts_data_check_and_fill_w_na(data_monthly, 
                                                 key = c("City", "Measure")) 
    data_monthly <- data_monthly_complete %>% 
      dplyr::select(Year_Month, Year, Month, City, Measure, count)
  }
  return(data_monthly)
}