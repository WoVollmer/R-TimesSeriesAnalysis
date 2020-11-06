# require(tidyverse)
require(ggplot2)  # for ggplot(), geom_...()
require(dplyr) # for filter() and "%>%"
# require(magrittr) # for "%>%"
require(tidyr) # for pivot_wider

# @import tidyverse
# @import ggplot2
# @import dplyr
# @import tidyr

require(patchwork) # package for composing plots
require(highcharter) # package for highchart plots
require(dygraphs) # package for dygraph plots

#' Cumulative and daily data trend plot
#' 
#' Provide **trend plot** for *cumulative* and *daily* cases with 
#' facets of `vars_1 = Case_Type`
#'
#' @param data A data frame
#' @param y_cum Unquoted `column name` of the data frame's cumulative cases
#' @param y_daily Unquoted `column name` of the data frame's daily cases
#' @param daily_mean Unquoted `column name` of the data frame's daily rolling mean data
#' @param country Unquoted `column name` of the data frame's countries
#' @param span Numeric, span used for rolling mean calculation
#' @param weeks Numeric, number of time range weeks weeks for the daily data, 
#'   dates are provided in column `default = Date`
#' @param ... Variable pass through to [ggts_trend_facet()]. 
#' Unquoted `column name` of the data frame's dates. 
#' 
#' @return plot object of mode "`plot`"
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import patchwork
#' @export 
#' @seealso [ggts_trend_facet], [ggts_conf_deaths_facet] and [ggplot2::ggplot]
#' 
#' @examples 
#' # Corona data of "Germany")
#' ggts_cum_daily(corona_data, country = "Germany", weeks = 6)
ggts_cum_daily <- function(data, 
                           y_cum = Cases, y_daily = Daily_Cases,
                           daily_mean = Daily_Cases_Mean,
                           country, span = 7, weeks = 12, ...) {
  data <- data %>% dplyr::filter(Country == country)
  
  plot_cum_cases <- ggts_trend_facet(data, y = {{ y_cum }}) +
    labs(title = paste(country, "- cumulative Cases (since Jan 2020)"))
  
  last_date <- max(data$Date)
  data <- data %>% dplyr::filter(Date >= last_date - 7 * weeks + 1)
  
  plot_daily_cases <- ggts_trend_facet(data, y = {{ y_daily }}) +
    geom_line(aes(y = {{ daily_mean }}, col = "Rolling Mean"), 
              size = 1, na.rm = TRUE) +
    scale_x_date(date_labels = "%b %d", date_breaks = "14 days") +
    labs(title = paste(country, "- Daily Cases (past", weeks, "weeks)"),
         subtitle = paste0("with Rolling Mean of past ", span, " days"))
  
  plot_cum_cases + plot_daily_cases 
  # + plot_annotation(tag_levels = "A", title = "title annot")
}

#' Trend facet plot for each case type
#' 
#' Provide trend facet plot for each case type (e.g. Confirmed and Deaths)
#'
#' @param data A data frame
#' @param x Unquoted `column name` of the data frame's dates
#' @param y Unquoted `column name` of the data frame's cases
#' @param vars_1 Unquoted `column name` of the data frame's case types
#' @return plot object of mode "`plot`"
#' @export 
#' @seealso [ggts_cum_daily]
#' @examples 
#' # Corona data of "Germany" - plot cumulative cases for 'Confirmed' and
#' # 'Deaths' and 'Confirmed'
#' ggts_trend_facet(corona_data %>% dplyr::filter(Country == "Germany"))
#' ggts_trend_facet(corona_data %>% dplyr::filter(Country == "Germany"), y = Cases_100k)
ggts_trend_facet <- function(data, x = Date, y = Cases, vars_1 = Case_Type) {
  # col_scheme <- "Set1" # "RdYlGn" #"YlOrRd" #"Oranges" # "YlGnBu" # 
  p <- ggplot2::ggplot(data, aes({{ x }}, {{ y }}, col = {{ vars_1 }})) +
    facet_wrap(vars({{ vars_1 }}), ncol = 1, scales = "free_y",
               strip.position = "left") +
    geom_point(size = 1, na.rm = TRUE) +
    geom_line(na.rm = TRUE) +  
    scale_x_date(date_labels = "%b %d", date_breaks = "28 days") +
    theme(legend.position = "none")  +
    theme(plot.title = element_text(size = 10))
  # scale_colour_distiller(palette = col_scheme, direction = 1) +
  # scale_colour_brewer(palette = col_scheme, direction = 1) +
  # scale_color_discrete(c("blue",  "green", "red")) +
  p
}



#' Trend facet plot for each case type and each country
#' 
#' grid plot Confirmed / Death for each country
#'
#' @inheritParams ggts_trend_facet 
#' @inherit ggts_trend_facet return
#' @param vars_2 Unquoted `column name` of the data frame's countries
#' @export 
#' @seealso [ggts_trend_facet]
#' @examples 
#' # Corona data of "Germany", "Italy", "United States of America")
#' ggts_conf_deaths_facet(corona_data_sel)
ggts_conf_deaths_facet <- function(data, x = Date, y = Cases, vars_1 = Case_Type,
                                   vars_2 = Country) {
  # col_scheme <- "Set1" # "RdYlGn" #"YlOrRd" #"Oranges" # "YlGnBu" # 
  ggplot2::ggplot(data, aes({{ x }}, {{ y }}, col = {{ vars_1 }})) +
    facet_grid(vars({{ vars_2 }}, {{ vars_1 }}), scales = "free_y") +
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

### log scale #############################

#' plot countries on log10scale
#'
#' @inheritParams ggts_conf_deaths_facet
#' @inherit ggts_trend_facet return 
#' @export
#' @examples 
#' # Corona data of "Germany", "Italy", "United States of America")
#' ggts_logscale(corona_data_sel)
ggts_logscale <- function(data, x = Date, y = Cases, vars_1 = Country, 
                          vars_2 = Case_Type) {
  # gg_plot <-  
  ggplot2::ggplot(data, aes({{ x }}, y= log10({{ y }}), col = {{ vars_1 }})) +
    labs(x = "Date", y = substitute(y),
         title = 
           "Virus Spread (with log10 scale) - World and selected Countries") + 
    geom_point() +
    # geom_line(aes(col = {{ vars_1 }}), size = 1) +
    # geom_smooth(method="loess", aes(col = {{ vars_1 }}), lty = "dashed", se=FALSE) +
    theme(legend.position = "bottom") +
    facet_wrap(vars({{ vars_2 }}), ncol = 2, scales = "free_y",
               strip.position = "left") 
}


### highchart #############################

#' highcharter World Map plot
#'
#' @param data tbd
#' @param value tbd
#' @param title tbd
#'
#' @return highchart plot
#' @export
#' @import highcharter
#' 
#' @examples
#' # so far no example
#' @references 
#' **Highcharts** *CONFIGURATION OPTIONS:* 
#'  <https://api.highcharts.com/highcharts/title>
#' 
#' **Manual pages for highcharter**
#'  <https://rdrr.io/cran/highcharter/man/hc_xAxis.html>
#' 
hc_world_map_plot <- function(data, value, title = "Text") {
  highchart() %>%
    hc_add_series_map(worldgeojson, 
                      data, 
                      value = value, 
                      joinBy = c('name', 'Country'))  %>% 
    #hc_colors(c("darkorange", "darkgray")) %>% 
    hc_colorAxis(stops = color_stops()) %>% 
    hc_title(text = title) %>% 
    hc_yAxis(title = list(text = ("cumulative Cases")))  
}

### bar chart #############################


#' Bar Chart plot with highchart()
#' 
#' Visualization with top x country bar chart
#'
#' @param data tbd
#' @param y tbd
#' @param title tbd
#' @param n tbd
#'
#' @examples
#' # example to get ordering plus World => via bind_rows(), currently not used:  
#' #  data <- bind_rows(
#' #    data %>% dplyr::filter(Country != "World") %>%
#' #      arrange(desc(Cases_100k)) %>% head(14),   
#' #    data %>%  
#' #      dplyr::filter(Country == "World"))
#' @inherit hc_world_map_plot references return
#' @export
hc_bar_chart_country <- function(data, y, title = "Text", n = 15) {
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


#' dygraph plot for Daily Confirmed and Death Cases
#' 
#' dygraph interactive plot for xts time series objects:
#' input must be a named list or data frame, where the first element/column
#' provides x-axis values and all subsequent elements/columns provide one or 
#' more series of y-values.
#'
#' @param data_xts tbd
#' @param country_select tbd
#' @param last_date tbd
#' @param span tbd
#' @param weeks tbd
#'
#' @return tbd
#' @export
#' @import dygraphs
#' 
#' @references <https://rstudio.github.io/dygraphs/gallery-series-highlighting.html>
#'
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

#' dygraph plot for Daily Confirmed Cases and Reproduction Number w/ CI
#'
#' @inheritParams plot_dygraph_daily
#'
#' @inherit plot_dygraph_daily references return
#' @export
#'
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
