# Read data from data file and process them for further analysis



library(lubridate)
suppressMessages(library(tidyverse))
suppressMessages(library(magrittr))

source("../../R-TimeSeriesAnalysis/uts_TimeSeries.R")

input_file <- "D:/Wolfgang/Programs-R/R-TimeSeriesAnalysis/Corona-Virus/World_Population/world_population.txt"

# View(corona_country %>% filter(Case_Type == "Deaths" & Date == last_date))
# write_csv((corona_country %>% filter(Case_Type == "Deaths" & Date == last_date)), "country_list.csv")
#           
          

world_population <- 
  read_delim(input_file, delim = ",", trim_ws = TRUE, 
             col_names = c("Country", "Country_CIA", "Population"), 
             skip = 3) %>%
  dplyr::select("Country", "Population")
world_population %<>% filter(!is.na(Country)) 

saveRDS(world_population, "./World_Population/world_population.RDS")





world_population <- readRDS("./World_Population/world_population.RDS")

f_join <- full_join(filter(corona_country_last, Case_Type == "Confirmed"), 
                    world_population)
View(f_join)

View(full_join(corona_country_last, world_population) %>% arrange(Country))

filter(full_join(corona_country_last, world_population), Country == "Bosnia and Herzegovina")  
