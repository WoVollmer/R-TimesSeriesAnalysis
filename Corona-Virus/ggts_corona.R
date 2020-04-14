
ggts_cases_facet <- function(data, x = Date, y = Cases, col = Case_Type) {
  # col_scheme <- "Set1" # "RdYlGn" #"YlOrRd" #"Oranges" # "YlGnBu" # 
  ggplot(data, aes_(substitute(x), substitute(y), col = substitute(Case_Type))) +
    facet_wrap(vars(Case_Type), ncol = 1, scales = "free_y",
               strip.position = "left") +
    geom_point(size = 1, na.rm = TRUE) +
    geom_line(na.rm = TRUE) +  
    theme(legend.position = "none")  +
    labs(y = "") + 
    scale_x_date(date_labels = "%b %d", date_breaks = "14 days") +
    # scale_colour_distiller(palette = col_scheme, direction = 1) +
    # scale_colour_brewer(palette = col_scheme, direction = 1) +
    # scale_color_discrete(c("blue",  "green", "red")) +
    ggtitle("Confirmed and Death Cases")
}


# function to get grid plot with Cases trend and Daily_Cases
ggts_trend_daily <- function(data, i) {
  plot_cases <- ggts_cases_facet(data, y = Cases) +
    labs(title = paste(i, "- Cumulated Cases (all)"))
  
  # for Daily_Cases calculate rolling mean
  span <- 7 # rolling mean over 7 days
  data %<>% mutate(rol_mean = 
                     stats::filter(Daily_Cases, 
                                   filter = rep(1 / span, span)))
  plot_daily_cases <- ggts_cases_facet(filter(data, Date >= last_date - 27), y = Daily_Cases) +
    geom_line(aes(y = rol_mean, col = "Rolling Mean"), size = 1, na.rm = TRUE) +
    scale_x_date(date_labels = "%b %d", date_breaks = "7 days") +
    labs(title = paste(i, "- Daily Cases (past 4 weeks"),
         subtitle = paste("with", span, "-day Rolling Mean"))
  gridExtra::grid.arrange(plot_cases, plot_daily_cases, ncol = 2)
}


# grid plot Confirmed / Death for selected countries 
ggts_conf_deaths_facet <- function(data, x = Date, y = Cases, col = Case_Type) {
  # col_scheme <- "Set1" # "RdYlGn" #"YlOrRd" #"Oranges" # "YlGnBu" # 
  ggplot(data, aes_(substitute(x), substitute(y), col = substitute(Case_Type))) +
    facet_grid(vars(Country, Case_Type), scales = "free_y") +
    geom_point(size = 1.5, na.rm = TRUE) +
    geom_line(size = 1, na.rm = TRUE) +  
    theme(legend.position = "none")  +
    labs(y = "") + 
    scale_x_date(date_labels = "%b %d", date_breaks = "7 days") +
    # scale_colour_distiller(palette = col_scheme, direction = 1) +
    # scale_colour_brewer(palette = col_scheme, direction = 1) +
    # scale_color_discrete(c("blue",  "green", "red")) +
    ggtitle("Confirmed and Death - Daily Cases (past 3 weeks)")
}

# https://api.highcharts.com/highcharts/title
# https://rdrr.io/cran/highcharter/man/hc_xAxis.html
world_map_plot <- function(data, i) {
  highchart() %>%
    hc_add_series_map(worldgeojson, 
                      data, 
                      value = 'Cases', 
                      joinBy = c('name', 'Country'))  %>% 
    #hc_colors(c("darkorange", "darkgray")) %>% 
    hc_colorAxis(stops = color_stops()) %>% 
    hc_title(text = "Spread of Coronavirus SARS-CoV-2") %>% 
    hc_subtitle(text = paste(i, "- Cumulated Cases / Country - Actual Figures"))
}


# Visualization with top 10 country bar chart
# https://rdrr.io/cran/highcharter/man/hc_xAxis.html
bar_chart_countries <- function(data, i) {
  data %>%
    arrange(desc(Cases)) %>% 
    head(15) %>%
    hchart("bar",hcaes(x = Country,  y = Cases)) %>%
    hc_title(text = paste(i, "- Cumulated Cases (Descending Order)")) %>% 
    hc_add_theme(hc_theme_sandsignika())
}


# Visualization with top 10 country/100k bar chart
bar_chart_countries_pop <- function(data, i) {
  data <- bind_rows(
    data %>% 
      filter(Case_Type == i, Country != "World") %>% 
      arrange(desc(Cases_100k)) %>% head(14), 
    data %>%
      filter(Case_Type == i, Country == "World"))
  
  data %>%
    arrange(desc(Cases_100k)) %>% 
    head(20) %>%
    hchart("bar",hcaes(x = Country,  y = Cases_100k)) %>%
    hc_title(
      text = paste(i, "- Cases per 100k Inhabitants (Descending Order)")) %>% 
    hc_yAxis(title = list(text = ("Cases per 100k Inhabitants"))) %>% 
    hc_add_theme(hc_theme_sandsignika())
}


# plot countries on log10scale
gg_logscale <- function(data, x = Date, y = Cases) {
  gg_plot <-  
    ggplot(data, aes_(substitute(x), y= substitute(log10(y)))) +
    labs(x = "Date", y = "Cumulated Cases",
         title = 
           "Virus Spread (with log10 scale) - World and selected Countries") + 
    geom_point(aes(col = Country)) +
    # geom_line(aes(col = Country), size = 1) +
    # geom_smooth(method="lm", aes(col = Country), lty = "dashed", se=FALSE) +
    theme(legend.position = "bottom") +
    facet_wrap(vars(Case_Type), ncol = 2, scales = "free_y",
               strip.position = "left") 
}


# source("./ggts_corona.R") # ggplot2 functions for time series plots
 