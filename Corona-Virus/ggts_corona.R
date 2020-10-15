
ggts_cases_facet <- function(data, x = Date, y = Cases, col = Case_Type) {
  # col_scheme <- "Set1" # "RdYlGn" #"YlOrRd" #"Oranges" # "YlGnBu" # 
  p <- ggplot(data, aes({{ x }}, {{ y }}, col = {{  col }})) +
    facet_wrap(vars({{ col }}), ncol = 1, scales = "free_y",
               strip.position = "left") +
    geom_point(size = 1, na.rm = TRUE) +
    geom_line(na.rm = TRUE) +  
    theme(legend.position = "none")  +
    labs(y = "") + 
    scale_x_date(date_labels = "%b %d", date_breaks = "28 days") +
    # scale_colour_distiller(palette = col_scheme, direction = 1) +
    # scale_colour_brewer(palette = col_scheme, direction = 1) +
    # scale_color_discrete(c("blue",  "green", "red")) +
    ggtitle("Confirmed and Death Cases") +
    theme(plot.title = element_text(size = 10))
  p # ggplotly(p)
}

# function to get grid plot with Cases trend and Daily_Cases
ggts_trend_daily <- function(data, i, span = 7, weeks = 6) {
  plot_cases <- ggts_cases_facet(data, y = Cases) +
    labs(title = paste(i, "- Cumulated Cases (all)"))
  
  last_date <- max(data$Date)
  plot_daily_cases <- ggts_cases_facet(
    filter(data, Date >= last_date - weeks * 7 + 1), y = Daily_Cases) +
    geom_line(aes(y = Cases_rol_mean, col = "Rolling Mean"), 
              size = 1, na.rm = TRUE) +
    scale_x_date(date_labels = "%b %d", date_breaks = "14 days") +
    labs(title = paste(i, "- Daily Cases (past", weeks, "weeks)"),
         subtitle = paste0("with Rolling Mean of past ", span, " days"))
  # gridExtra::grid.arrange(plot_cases, plot_daily_cases, ncol = 2)
  plot_cases + plot_daily_cases 
            # + plot_annotation(tag_levels = "A", title = "title annot")
}


# grid plot Confirmed / Death for selected countries 
ggts_conf_deaths_facet <- function(data, x = Date, y = Cases, col = Case_Type,
                                   vars_2 = Country) {
  # col_scheme <- "Set1" # "RdYlGn" #"YlOrRd" #"Oranges" # "YlGnBu" # 
  ggplot(data, aes({{ x }}, {{ y }}, col = {{  col }})) +
    facet_grid(vars({{ vars_2 }}, {{ col }}), scales = "free_y") +
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

### highchart #############################

# https://api.highcharts.com/highcharts/title
# https://rdrr.io/cran/highcharter/man/hc_xAxis.html
world_map_plot <- function(data, i, value, title) {
  highchart() %>%
    hc_add_series_map(worldgeojson, 
                      data, 
                      value = value, 
                      joinBy = c('name', 'Country'))  %>% 
    #hc_colors(c("darkorange", "darkgray")) %>% 
    hc_colorAxis(stops = color_stops()) %>% 
    hc_title(text = title) %>% 
    hc_yAxis(title = list(text = ("Cumulated Cases")))  
}

### bar chart #############################

# Visualization with top x country bar chart
# https://rdrr.io/cran/highcharter/man/hc_xAxis.html
bar_chart_countries <- function(data, i) {
  data %>%
    arrange(desc(Cases)) %>% 
    head(15) %>%
    hchart("bar",hcaes(x = Country,  y = Cases)) %>%
    hc_title(text = paste(i, "- Cumulated Cases (Descending Order)")) %>% 
    hc_yAxis(title = list(text = ("Cumulated Cases per 100k Inhabitants"))) %>% 
    hc_add_theme(hc_theme_sandsignika())
}


# Visualization with top x country/100k bar chart
bar_chart_countries_pop <- function(data, i) {
  data <- bind_rows(
    data %>% 
      filter(Case_Type == i, Country != "World") %>% 
      arrange(desc(Cases_100k)) %>% head(14), 
    data %>%
      filter(Case_Type == i, Country == "World"))
  
  data %>%
    arrange(desc(Cases_100k)) %>% 
    # to get world data in the right order
    hchart("bar",hcaes(x = Country,  y = Cases_100k)) %>%
    hc_title(
      text = paste(i, "- Cumulated Cases per 100k Inhabitants (Descending Order)")) %>% 
    hc_yAxis(title = list(text = ("Cumulated Cases per 100k Inhabitants"))) %>% 
    hc_add_theme(hc_theme_sandsignika())
}


# Visualization with top x hot spots country/mean bar chart
bar_chart_countries_hot_spots <- function(data, i) {
  data <- bind_rows(
    data %>% 
      filter(Case_Type == i, Country != "World") %>% 
      arrange(desc(Cases_100k_rol_mean)) %>% head(14), 
    data %>%
      filter(Case_Type == i, Country == "World"))
  
  data %>%
    arrange(desc(Cases_100k_rol_mean)) %>% 
    # to get world data in the right order
    hchart("bar",hcaes(x = Country,  y = Cases_100k_rol_mean)) %>%
    hc_title(
      text = paste(i, "- Mean Daily Cases per 100k Inhabitants (Descending Order)")) %>% 
    hc_yAxis(title = list(text = ("Mean Cases per 100k Inhabitants"))) %>% 
    hc_add_theme(hc_theme_sandsignika())
}

### log scale #############################

# plot countries on log10scale
gg_logscale <- function(data, x = Date, y = Cases, col = Country, 
                        vars_1 = Case_Type) {
  gg_plot <-  
    ggplot(data, aes({{ x }}, y= log10({{ y }}), col = {{ col }})) +
    labs(x = "Date", y = substitute(y),
         title = 
           "Virus Spread (with log10 scale) - World and selected Countries") + 
    geom_point() +
    # geom_line(aes(col = {{ col }}), size = 1) +
    # geom_smooth(method="loess", aes(col = {{ col }}), lty = "dashed", se=FALSE) +
    theme(legend.position = "bottom") +
    facet_wrap(vars({{ vars_1 }}), ncol = 2, scales = "free_y",
               strip.position = "left") 
}

#### Reproduction number calculation - source from TU Ilmenau - GitHub 
#
repronum <- function(
  new.cases, # I
  profile, # w
  window = 1, # H
  delay = 0, # Delta
  conf.level = 0.95, # 1-alpha
  pad.zeros = FALSE,
  min.denominator = 5,
  min.numerator = 5
) {
  # pad zeros if desired
  if (pad.zeros) new.cases <- c(rep(0, length(profile) - 1), new.cases)
  
  # compute convolutions over h, tau and both, respectively
  sum.h.I <- as.numeric(stats::filter(new.cases, rep(1, window),
                                      method = "convolution", sides = 1))
  sum.tau.wI <- as.numeric(stats::filter(new.cases, c(0, profile),
                                         method = "convolution", sides = 1))
  sum.htau.wI <- as.numeric(stats::filter(sum.tau.wI, rep(1, window),
                                          method = "convolution", sides = 1))
  
  # estimators
  repronum <- ifelse(sum.h.I < min.numerator, NA, sum.h.I) / ifelse(sum.htau.wI < min.denominator, NA, sum.htau.wI)
  
  # standard errors
  repronum.se <- sqrt(repronum / sum.htau.wI)
  
  # shift by delay
  repronum <- c(repronum, rep(NA, delay))[(1:length(repronum)) + delay]
  repronum.se <- c(repronum.se,
                   rep(NA, delay))[(1:length(repronum.se)) + delay]
  
  # standard normal qunatile
  q <- qnorm(1 - (1 - conf.level) / 2)
  
  # return data.frame with as many rows as new.cases
  ret <- data.frame(
    repronum = repronum,
    repronum.se = repronum.se,
    ci.lower = repronum - q * repronum.se,
    ci.upper = repronum + q * repronum.se
  )
  if (pad.zeros) ret[-(1:(length(profile) - 1)),] else ret
}

# https://rstudio.github.io/dygraphs/gallery-series-highlighting.html
# dygraph interactive plot for time series data:
# input must be a named list or data frame, where the first element/column
# provides x-axis values and all subsequent elements/columns provide one or more
# series of y-values.
plot_dygraph_daily <- 
  function(data_xts, country_select, last_date, span = 7, weeks = 12) {
    dygraph(data_xts, 
            main = paste0(country_select, 
                          " - Daily Cases with Rolling Mean of past ", 
                          span, " days")) %>% 
      dyAxis("y", label = "Daily Confirmed Cases") %>%
      dyAxis("y2", label = "Daily Death Cases",  independentTicks = TRUE) %>%
      dyLegend(width = 400) %>% 
      dySeries("Daily_Confirmed", 
               drawPoints = TRUE, pointSize = 3, pointShape = "circle", 
               color = "tomato") %>%  
      dySeries("Conf_rol_mean", drawPoints = FALSE,  color = "red") %>% 
      dySeries("Daily_Deaths", 
               drawPoints = TRUE, pointSize = 3, pointShape = "triangle", 
               color = "turquoise", axis = "y2") %>%     
      dySeries("Deaths_rol_mean", drawPoints = FALSE, 
               color = "blue", axis = "y2") %>% 
      dyRangeSelector(dateWindow = 
                        c(as.character(last_date - weeks * 7), as.character(last_date)))
  }

plot_dygraph_daily_repro <- 
  function(data_xts, country_select, last_date, span = 7, weeks = 12) {
    dygraph(data_xts, 
            main =  paste0(country_select, 
            " - Reproduction Number based on Daily Confirmed Cases (",
            span, "-day window)")) %>% 
      dyAxis("y", label = "Reproduction Number w/ Confidence Interval") %>%
      dyAxis("y2", label = "Daily Confirmed Cases",
             independentTicks = TRUE) %>%
      dyLegend(width = 400) %>%    
      dySeries(c("ci.lower","Repro_number", "ci.upper"), 
               color = "black") %>% 
      dyLimit(1, "Repro_number = 1",
              strokePattern = "dashed", color = "black") %>%
      dySeries("Daily_Confirmed", 
               drawPoints = TRUE, pointSize = 3, pointShape = "triangle", 
               color = "tomato", axis = "y2") %>%
      dySeries("Conf_rol_mean", drawPoints = FALSE,  
               color = "red", axis = "y2") %>%
      dyRangeSelector(dateWindow =
                        c(as.character(last_date - weeks * 7), as.character(last_date)))
  }
