#' provide trend plot for cumulated and daily cases 
#' for all case types (e.g. Confirmed and Deaths)
ggts_cum_daily <- function(data, 
                           y_cum = Cases, y_daily = Daily_Cases,
                           daily_mean = Daily_Cases_Mean,
                           country, span = 7, weeks = 12) {
  data <- data %>% filter(Country == country)
  
  plot_cum_cases <- ggts_trend_facet(data, y = {{ y_cum }}) +
    labs(title = paste(country, "- Cumulated Cases (since Jan 2020)"))
  
  last_date <- max(data$Date)
  data <- data %>% filter(Date >= last_date - 7 * weeks + 1)
  
  plot_daily_cases <- ggts_trend_facet(data, y = {{ y_daily }}) +
    geom_line(aes(y = {{ daily_mean }}, col = "Rolling Mean"), 
              size = 1, na.rm = TRUE) +
    scale_x_date(date_labels = "%b %d", date_breaks = "14 days") +
    labs(title = paste(country, "- Daily Cases (past", weeks, "weeks)"),
         subtitle = paste0("with Rolling Mean of past ", span, " days"))
  
  plot_cum_cases + plot_daily_cases 
  # + plot_annotation(tag_levels = "A", title = "title annot")
}

#' provide trend facet plot for case types (e.g. Confirmed and Deaths)
ggts_trend_facet <- function(data, x = Date, y = Cases, col = Case_Type) {
  # col_scheme <- "Set1" # "RdYlGn" #"YlOrRd" #"Oranges" # "YlGnBu" # 
  p <- ggplot(data, aes({{ x }}, {{ y }}, col = {{  col }})) +
    facet_wrap(vars({{ col }}), ncol = 1, scales = "free_y",
               strip.position = "left") +
    geom_point(size = 1, na.rm = TRUE) +
    geom_line(na.rm = TRUE) +  
    scale_x_date(date_labels = "%b %d", date_breaks = "28 days") +
    theme(legend.position = "none")  +
    theme(plot.title = element_text(size = 10))
  # scale_colour_distiller(palette = col_scheme, direction = 1) +
  # scale_colour_brewer(palette = col_scheme, direction = 1) +
  # scale_color_discrete(c("blue",  "green", "red")) +
  p # ggplotly(p)
}



# grid plot Confirmed / Death for selected countries 
ggts_conf_deaths_facet <- function(data, x = Date, y = Cases, col = Case_Type,
                                   vars_2 = Country) {
  # col_scheme <- "Set1" # "RdYlGn" #"YlOrRd" #"Oranges" # "YlGnBu" # 
  ggplot(data, aes({{ x }}, {{ y }}, col = {{ col }})) +
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
world_map_plot <- function(data, value, title) {
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
# 
# to get ordering plus World => via bind_rows()
#   data <- bind_rows(
#    data %>% filter(Country != "World") %>% 
#     arrange(desc(Cases_100k)) %>% head(14), 
#    data %>%
#     filter(Country == "World"))
#
bar_chart_country <- function(data, y, title = "Text", n = 15) {
  data %>% # ungroup() %>% 
    slice_max(order_by = .data[[y]], n = n) %>% 
    rename(col_name = .data[[y]]) %>%
    # hchart() does not work with {{y}} or substitute(), ....
    #      => fixed colname to be used and hc_yAxis() needed, otherwise "value"
    head(n) %>%
    hchart("bar", hcaes(x = Country,  y = col_name)) %>%
    hc_title(text = title) %>%
    hc_yAxis(title = list(text = y)) %>%
    hc_add_theme(hc_theme_sandsignika())
  #

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
      dySeries("Daily_Conf", 
               drawPoints = TRUE, pointSize = 3, pointShape = "circle", 
               color = "tomato") %>%  
      dySeries("Daily_Conf_Mean", drawPoints = FALSE,  color = "red") %>% 
      dySeries("Daily_Deaths", 
               drawPoints = TRUE, pointSize = 3, pointShape = "triangle", 
               color = "turquoise", axis = "y2") %>%     
      dySeries("Daily_Deaths_Mean", drawPoints = FALSE, 
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
      dySeries("Daily_Conf", 
               drawPoints = TRUE, pointSize = 3, pointShape = "triangle", 
               color = "tomato", axis = "y2") %>%
      dySeries("Daily_Conf_Mean", drawPoints = FALSE,  
               color = "red", axis = "y2") %>%
      dyRangeSelector(dateWindow =
                        c(as.character(last_date - weeks * 7), as.character(last_date)))
  }
