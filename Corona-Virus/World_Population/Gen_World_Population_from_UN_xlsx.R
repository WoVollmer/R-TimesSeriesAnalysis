# Read data from data file and process them for further analysis

suppressMessages(library(tidyverse))
suppressMessages(library(magrittr))

input_file <- "D:/Wolfgang/Programs-R/R-TimeSeriesAnalysis/Corona-Virus/World_Population/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx"

# source link: https://population.un.org/wpp/Download/Standard/Population/
# Total Population - Both Sexes. De facto population in a country, 
# area or region as of 1 July of the year indicated. 
# Figures are presented in thousands.

# View(corona_country %>% filter(Case_Type == "Deaths" & Date == last_date))
# write_csv((corona_country %>% filter(Case_Type == "Deaths" & Date == last_date)), "country_list.csv")
#           

world_population_un <- read_xlsx(input_file, sheet = "ESTIMATES", 
          # trim_ws = TRUE, 
          skip = 16) %>% 
  dplyr::select("Region, subregion, country or area *", "Type", "2020") %>% 
  filter(Type == "World" | Type == "Country/Area") %>% 
  rename(Country = "Region, subregion, country or area *",
         Population = "2020") %>% 
  mutate(Population = as.numeric(Population) * 1000)
# xlsx file: population figures are presented in thousand plus 3 decimal numbers 

# split serbia to serbia and kosovo 
pop_kosovo <- 1932774
kosovo <- filter(world_population_un, Country == "Serbia") %>% 
  mutate(Country = "Kosovo",
         Population = pop_kosovo) 
serbia <- filter(world_population_un, Country == "Serbia") %>% 
  mutate(Population = Population - pop_kosovo)

world_population_un <- 
  bind_rows(filter(world_population_un, Country != "Serbia"), serbia, kosovo)

# clean-up names of column Country
# same country names for covid-19 data and population_countries are needed
# names have to fit to names used by
# https://code.highcharts.com/mapdata/ => 
# World with Palestine areas, high resolution Demo  => Highmaps basic demo
world_population_un %<>%  mutate(
  Country = case_when(
    Country == "Bolivia (Plurinational State of)" ~ "Bolivia",
    Country == "Brunei Darussalam" ~ "Brunei",
    Country == "China, Taiwan Province of China" ~ "Taiwan",
    Country == "China, Macao SAR" ~ "Macao",
    Country == "Côte d'Ivoire" ~ "Ivory Coast",
    Country == "Czechia" ~ "Czech Republic",
    Country == "Guinea-Bissau" ~ "Guinea Bissau",
    Country == "China, Hong Kong SAR" ~ "Hong Kong",
    Country == "Eswatini" ~ "Swaziland",
    Country == "Holy See" ~ "Holy See (Vatican City)",
    Country == "Iran (Islamic Republic of)" ~ "Iran",
    Country == "Lao People's Democratic Republic" ~ "Laos",
    Country == "Republic of Moldova" ~ "Moldova",
    Country == "Republic of Korea" ~ "South Korea",
    Country == "Russian Federation" ~ "Russia",
    Country == "Republic of Korea" ~ "South Korea",
    Country == "Serbia" ~ "Republic of Serbia",
    Country == "North Macedonia" ~ "Macedonia",
    Country == "State of Palestine" ~ "West Bank and Gaza",
    Country == "Syrian Arab Republic" ~ "Syria",
    Country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    Country == "Congo" ~ "Republic of Congo",
    Country == "Congo (Kinshasa)" ~ "Democratic Republic of the Congo",
    Country == "Viet Nam" ~ "Vietnam",
    Country == "WORLD" ~ "World",
    TRUE ~ as.character(Country))) %>% 
  arrange(Country) 
# View(world_population_un)

saveRDS(world_population_un, "./world_population_un.RDS")

check <- FALSE
if (check) {
  world_population <- readRDS("./Corona-Virus/world_population.RDS")
  corona_country_last <- readRDS("./Corona-Virus/corona_country.RDS")
  
  last_date_last <- max(corona_country_last$Date)
  f_join <- full_join(filter(corona_country_last, Case_Type == "Confirmed"),
                      world_population)
  View(filter(f_join, Date == last_date_last))
  
}

