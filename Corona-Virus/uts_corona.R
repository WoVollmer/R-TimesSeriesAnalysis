require(checkmate)
require(tsibble)

#' @title Utilities for corona data analysis and data
#'
#' @description provide data in wide form and adapt names
#' @param tsibble data frame 
#' @details The data input nust be ...
#'
#' @return tsibble data frame
#' 
#' @encoding UTF-8
#' @md
#' 
get_corona_data_wide <- function(data) {
  
  names_data <- c("Country", "Date", "Population", "Case_Type", "Cases",
                  "Daily_Cases", "Cases_100k", "Daily_Cases_100k",
                  "Daily_Cases_Mean", "Daily_Cases_100k_Mean")
  assert_names(names(data), subset.of = names_data)
  
  data %>% 
    pivot_wider(names_from = Case_Type, 
                values_from = c(Cases, Daily_Cases, 
                                Cases_100k, Daily_Cases_100k, 
                                Daily_Cases_Mean, Daily_Cases_100k_Mean)) %>% 
    rename(Confirmed = Cases_Confirmed,
           Deaths = Cases_Deaths,
           Daily_Conf = Daily_Cases_Confirmed,
           Daily_Deaths = Daily_Cases_Deaths,
           Conf_100k = Cases_100k_Confirmed,
           Deaths_100k = Cases_100k_Deaths,
           Daily_Conf_100k = Daily_Cases_100k_Confirmed,
           Daily_Deaths_100k = Daily_Cases_100k_Deaths,
           Daily_Conf_Mean = Daily_Cases_Mean_Confirmed, 
           Daily_Deaths_Mean = Daily_Cases_Mean_Deaths,
           Daily_Conf_100k_Mean = Daily_Cases_100k_Mean_Confirmed, 
           Daily_Deaths_100k_Mean = Daily_Cases_100k_Mean_Deaths) %>% 
    dplyr::select(Country, Population, Date, 
                  Confirmed, Daily_Conf, Daily_Conf_Mean, Conf_100k, 
                  Daily_Conf_100k, Daily_Conf_100k_Mean, 
                  Deaths, Daily_Deaths, Daily_Deaths_Mean, Deaths_100k, 
                  Daily_Deaths_100k, Daily_Deaths_100k_Mean)
}