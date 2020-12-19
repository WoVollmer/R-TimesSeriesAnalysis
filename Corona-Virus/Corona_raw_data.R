#' Read John Hopkins University Corona raw data
#'
#' Reads the separate *.csv data for confirmed, deaths and recovered and binds
#' them to one tibble.
#'
#' @param data_path character string for data path to local GitHub repository
#'
#' @return tibble
read_raw_data <- function(data_path) {
  `Province/State` <- `Country/Region` <- Case_Type <- Lat <- Long <- NULL
  # read data, assign to Case_Type, bind rows and change to long format
  corona_confirmed <- 
    read_csv(paste0(data_path, "/time_series_covid19_confirmed_global.csv")) %>%
    mutate(Case_Type = "Confirmed")
  corona_deaths <- 
    read_csv(paste0(data_path, "/time_series_covid19_deaths_global.csv"))  %>%
    mutate(Case_Type = "Deaths")
  corona_recovered <- 
    read_csv(paste0(data_path, "/time_series_covid19_recovered_global.csv")) %>%
    mutate(Case_Type = "Recovered")
  
  corona <- bind_rows(corona_confirmed, corona_deaths, corona_recovered) %>% 
    pivot_longer(cols = c(-`Province/State`, -`Country/Region`, -Case_Type, 
                          -Lat,  -Long ), 
                 names_to = c("Date"),
                 values_to = "Cases")
  
  corona <- corona %>% 
    mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>% 
    rename(Country = `Country/Region`) 
  
  # take out numbers of cruise linets
  corona <- corona %>% filter(Country != "Diamond Princess" & Country != "MS Zaandam")
}


#' Clean country names
#'
#' Clean-up names of column Country / denmark "regions" to country.
#' Same country names for  
#' covid-19 data & population_countries are needed for data join and the
#' country names have to fit to highcharter world map names
#' 
#' https://code.highcharts.com/mapdata/ => 
#' World with Palestine areas, high resolution Demo  => Highmaps basic demo
#' 
#' hint: see also  corona %>% janitor::clean_names()
#'
#' @param data A data.frame
#'
#' @return A data.frame
clean_country_names <- function(data) {
  Country <- `Province/State` <- Case_Type <- NULL
  data <- data %>% 
    mutate(
      Country = case_when(
        Country == "Burma" ~ "Myanmar",
        Country == "Mainland China" ~ "China",
        Country == "Czechia" ~ "Czech Republic",
        Country == "Denmark" & `Province/State` == "Greenland" ~ "Greenland",
        Country == "Denmark" & `Province/State` == "Faroe Islands" ~ "Faroe Islands",
        Country == "Guinea-Bissau" ~ "Guinea Bissau",
        Country == "Hong Kong SAR" ~ "Hong Kong",
        Country == "Holy See" ~ "Holy See (Vatican City)",
        Country == "Iran (Islamic Republic of)" ~ "Iran",
        Country == "Macao SAR" ~ "Macao",
        Country == "occupied Palestinian territory" ~ "Palestine",
        Country == "Republic of Korea" ~ "South Korea",
        Country == "Cote d'Ivoire" ~ "Ivory Coast",
        Country == "Congo (Brazzaville)" ~ "Republic of Congo",
        Country == "Congo (Kinshasa)" ~ "Democratic Republic of the Congo",
        Country == "Korea, South" ~ "South Korea",
        Country == "Serbia" ~ "Republic of Serbia",
        Country == "North Macedonia" ~ "Macedonia",
        Country == "Eswatini" ~ "Swaziland",
        Country == "Taiwan*" ~ "Taiwan",
        Country == "Tanzania" ~ "United Republic of Tanzania",
        Country == "US" ~ "United States of America",
        Country == "Viet Nam" ~ "Vietnam",
        TRUE ~ as.character(Country)))
  
  # clean-up content of column `Province/State`
  # replace NAs by Country
  # replace "Bavaria"  by  Country since `Province/State` exists only until 
  # end of January, as begin of February inluded in "Germany" data
  # `Province/State` == "Bavaria" ~ Country, not needed in time series format,
  #  needed for data file: covid_19_data.csv
  data <- data %>%  mutate(    
    `Province/State` = case_when(
      is.na(`Province/State`) ~ Country,
      TRUE ~ `Province/State`)) %>% 
    arrange(Country, `Province/State`) %>% 
    group_by(Case_Type, Country, `Province/State`)
  
  # View(corona %>% dplyr::filter(Date == last_date & Case_Type == "Confirmed"))
}



#' Read and process John Hopkins University Corona raw data
#'
#' Reads the separate *.csv data for confirmed, deaths and recovered and binds
#' them to one tibble.
#'
#' The country names will be cleaned-up, daily cases will be added and 
#' summary data for 'World' and 'EU' will be added
#'
#' @param data_path character string for data path to local GitHub repository
#' @param eu character vector with names of EU countries 
#' 
#' @return tibble
read_and_process_raw_data <- function(data_path, eu) {
  Country <- Case_Type <- Cases <- NULL
  data <- read_raw_data(data_path)
  data <- clean_country_names(data)
  
  # add all data from provinces to country data 
  data_country <- data %>% 
    group_by(Country, Date, Case_Type) %>% 
    summarise(Cases = sum(Cases, na.rm = TRUE))
  
  # add daily cases to country data (note: diff(x, default: lag = 1))
  data_country <- data_country %>% 
    group_by(Country, Case_Type) %>% 
    mutate(Daily_Cases = c(NA, diff(Cases)))
  
  # filter out Case_Type "Recovered"  - no real value add, no consistent data
  data_country <- data_country %>% 
    dplyr::filter(Case_Type != "Recovered")
  
  
  # add country World data
  # provide sum of all countries as country "World" data
  # for sum( , na.rm = TRUE) removing missing values is important,
  # otherwise one region with NA (no cases) results in NA worldwide
  data_country_sum <- data_country %>% 
    group_by(Date, Case_Type) %>% 
    summarise(Cases = sum(Cases, na.rm = TRUE))
  data_country_sum <- data_country_sum %>%  
    group_by(Case_Type) %>% 
    mutate(Daily_Cases = c(NA, diff(Cases)),
           Country = "World")
  
  data_country <- bind_rows(data_country, data_country_sum)
  
  # add country EU data
  ### provide sum of all EU countries as country "EU" data
  
  data_country_sum <- data_country %>% 
    dplyr::filter(Country %in% eu) %>% 
    group_by(Date, Case_Type) %>% 
    summarise(Cases = sum(Cases, na.rm = TRUE))
  data_country_sum <- data_country_sum %>%  
    group_by(Case_Type) %>% 
    mutate(Daily_Cases = c(NA, diff(Cases)),
           Country = "EU")
  
  data_country <- bind_rows(data_country, data_country_sum)
}


#' Add countries population data and rolling means to data
#'
#' Reads the separate *.csv data for confirmed, deaths and recovered and binds
#' them to one tibble.
#'
#' The country names will be cleaned-up, daily cases will be added and 
#' summary data for 'World' and 'EU' will be added
#'
#' @param data A data.frame
#' 
#' @return tsibble with index = Date, key = c(Country, Case_Type)
add_popul_rolling_mean <- function(data) { 
  Country <- Population <- Case_Type <- Cases <- Daily_Cases <- NULL
  Daily_Cases_100k <- NULL
  # read UN world population data for normalization per 100k inhabitants
  population_data <- readRDS("./world_population_un.RDS") # data from UN
  
  # add world population and normalize Confirmed and Deaths per 100k inhabitants
  
  population_data <- population_data %>%
    dplyr::select(Country, Population)
  # note: deselects "Type", which provides split "Country" vs. "Region" World & EU
  
  corona_data <- full_join(data, population_data)
  
  # add relative data per 100k inhabitants
  corona_data <- corona_data %>% 
    dplyr::filter(!is.na(Date)) %>% 
    mutate(Cases_100k = round(Cases/(Population/100000), digits = 1),
           Daily_Cases_100k = round(Daily_Cases/(Population/100000), digits = 2))
  
  # add rolling mean  per country to data
  # slide_dbl() => rolling mean double value, changed to before only
  #                             no longer centered with .before = .after
  # note: for last date rolling mean before is needed, otherwise all are NA
  corona_data <- corona_data %>%
    group_by(Country, Case_Type) %>% 
    mutate(Mean_Daily_Cases =  round(
      slider::slide_dbl(Daily_Cases,  ~ mean(.),
                        .before = 2*q, .after = 0, .complete = TRUE),
      digits = 1),
      Mean_Daily_Cases_100k = round(
        slider::slide_dbl(Daily_Cases_100k,  ~ mean(.), 
                          .before = 2*q, .after = 0, .complete = TRUE),
        digits = 2)) %>% 
    ungroup()
  
  corona_data <- corona_data  %>%
    tsibble::as_tsibble(index = Date, key = c(Country, Case_Type)) %>% 
    dplyr::select(Country, Population, Date, Case_Type,
                  Cases, Daily_Cases, Mean_Daily_Cases, 
                  Cases_100k, Daily_Cases_100k, Mean_Daily_Cases_100k)
}