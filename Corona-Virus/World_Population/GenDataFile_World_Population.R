# Read data from data file and process them for further analysis

suppressMessages(library(tidyverse))
suppressMessages(library(magrittr))

input_file <- "D:/Wolfgang/Programs-R/R-TimeSeriesAnalysis/Corona-Virus/World_Population/world_population.txt"

# View(corona_country %>% filter(Case_Type == "Deaths" & Date == last_date))
# write_csv((corona_country %>% filter(Case_Type == "Deaths" & Date == last_date)), "country_list.csv")
#           
          

world_population <- 
  read_delim(input_file, delim = ",", trim_ws = TRUE, 
             col_names = c("Country", "Country_CIA", "Population"), 
             skip = 3) %>%
  dplyr::select("Country", "Population")
world_population %<>% filter(!is.na(Country)) %>% 
  arrange(Country)
world_population$Population <- as.numeric(world_population$Population)
saveRDS(world_population, "./world_population.RDS")

check <- FALSE
if (check) {
  world_population <- readRDS("./Corona-Virus/world_population.RDS")
  corona_country_last <- readRDS("./Corona-Virus/corona_country.RDS")
  
  last_date_last <- max(corona_country_last$Date)
  f_join <- full_join(filter(corona_country_last, Case_Type == "Confirmed"),
                      world_population)
  View(filter(f_join, Date == last_date_last))
  
}

