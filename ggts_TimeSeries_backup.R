# general ggplot theme settings
require(tidyverse)
require(magrittr)
library(RColorBrewer) # ColorBrewer palettes

col_scheme <- "YlOrRd"
col_scheme_precip <- "YlGnBu"

theme_replace(plot.title = element_text(hjust = 0.5, face = "bold.italic", 
                                        color = "darkcyan", size = 12),
              plot.subtitle = element_text(hjust = 0.5, face = "italic", 
                                           color = "darkcyan"))
x_axis_theme <- element_text(size = 14)

## replace y-axes theme: Grad Celsius by 
##        \textdegree C  rsp.   $^\circ$C 
# ylab <- "Temperature (°C) / Precipitation (mm/month)" # whyever A-hat before °C 
# ylab <- expression(paste("Temperature ", (degree~C), 
#                            " / Precipitation (mm/month)")) 
#                       # degree~C | degree*C  => blank before C  w/o blank
ylab <- expression(paste("Precipitation / mm/Year   rsp.   Temperature / ",
                         degree*C))
ylab_m <- expression(paste("Precipitation / mm/Month   rsp.   Temperature / ",
                           degree*C))




#' @title Generate ggplot (Time-)Series Line Object with Rolling mean 
#' and Smooth line
#'
#' @description Calculates rolling mean with stats::filter(y, filter = rep(1/span, span)
#' and adding smooth line with geom_smooth(method = "loess"). 
#' @param data data.frame with x (e.g. time line) and y data to be displayed, 
#' e.g. ggplot_w_rol_mean(weather_long, Year_Month, Temperature, span = span).
#'   x : numeric string (also of data and Time classes) as index for 
#'   x column name.
#'   y : numeric string as index for y column name.
#'   span : integer width of the rolling window. 
#' @details The data input must be a valid data.frame 
#'   Year_Month ...   count
#'        <mth> ...   <dbl>
#' 1   1890 Jan ...     2.8
#' 2   1890 Feb ...    -1.7
#' :
#' n   2019 Dec ...     1.4
#' @return ggplot object
#' @seealso  \code{\link[stats]{filter}} and \code{\link[ggplot2]{ggplot}}
#' @examples
#' economics # {ggplot2}
#' ggplot_w_rol_mean(economics, "date", "uempmed", span = 7)
#' @keywords misc
#' @import checkmate
#' @importFrom stats filter
#' @encoding UTF-8
#' @md
ggts_w_rol_mean <- function(data, time = Year_Month, y = count,  span) {
# plot with running/rolling mean - attention! use aes_(aes with underscore)
  # column <- enquo(y)  ## !!enquo(y) 
                       ## new alternative: embrace the arg  {{arg}}
  data %<>% mutate(rol_mean = 
                     stats::filter({{y}}, filter = rep(1/span, span)))
                ##   stats::filter(!!(column), filter = rep(1/span, span)))
  # for getting "count": !!sym(y) or !!as.name(y)
  # print(tail(data))
  gg_plot <- ggplot(data, aes_(x = substitute(time), y = substitute(y))) +
    geom_smooth(aes(col = "Linear Reg"), method = "lm", 
                size = 1, na.rm = TRUE) +
    geom_smooth(aes(col = "Loess Reg"), method = "loess", 
                size = 1, na.rm = TRUE) +
    geom_line(aes(y = rol_mean, col = "Rolling Mean"), size = 1, na.rm = TRUE) +
    geom_line(aes(col = "Measured Values")) +
    # values: set colour values according ordering
    scale_colour_manual(limits = 
                          c("Measured Values", "Rolling Mean", "Linear Reg",
                            "Loess Reg"),
                        values = c("blue", "red", "green", "black")) +
    theme(legend.position = "bottom") +
    labs(x = "Year", col = "") +
    ggtitle("Data over Time",
            subtitle = paste("with Rolling Mean over", span, "measured values"))
  return(gg_plot)
}


#' @title Generate Time Series Decompression ggplot facet
#'  
#' @description Generates from stlplus() output list components $Data and $time 
#' a ggplot facet_wrap output over time for 
#'  "raw" (=original data), "seasonal", "trend" and "remainder" components.
#' @param data : stlplus() output list
#' e.g. ggplot_w_rol_mean(weather_long, "Year_Month", "Temperature", span = span).
#'   x : numeric string (also of data and Time classes) as index for 
#'   x column name.
#'   y : numeric string as index for y column name.
#'   span : integer width of the rolling window. 
#' @details ensure
#' @return ggplot object
#' @seealso  \code{\link[stats]{filter}} and \code{\link[ggplot2]{ggplot}}
#' @examples
#' economics # {ggplot2}
#' economics_tsbl <- economics %>% mutate(Year_Month = yearmonth(date)) 
#' economics_tsbl <- as_tsibble(economics_tsbl, index = Year_Month, .full = TRUE)
#' economics_tsbl %>% has_gaps() # check for time series gaps, fill with fill_gaps()
#' start_ts <- c(year(min(as.Date(economics_tsbl$Year_Month))), month(min(as.Date(economics_tsbl$Year_Month))))
#' economics_ts <- ts(economics_tsbl$uempmed, start = start_ts, frequency = 12)
#' economics_stlplus <- stlplus(economics_ts,  s.window=27, t.window=1201)
#' ggplot_stlplus(economics_stlplus)
#' economics_stlplus <- stlplus(economics_ts,  s.window="per", t.window=1201)
#' ggplot_stlplus(economics_stlplus)
#' @keywords misc
#' @import checkmate
#' @importFrom stats filter
#' @encoding UTF-8
#' @md
ggts_decomp <- function(data, ...){
  
  data_long <- data %>%
    mutate(trend_for_raw = Trend) %>%    # to add trend line in facet Raw only
    rename("Data with Trend" = Raw) %>% 
    pivot_longer(cols = c("Data with Trend", "Seasonal", "Trend", "Remainder"),
                 names_to = "Decompressed",
                 values_to = "value") %>% 
    mutate(Decompressed = factor(Decompressed,
                                 levels = c("Data with Trend", "Seasonal", 
                                            "Trend", "Remainder")))
  
  # 'trend_for_raw' and 'NA_replace' only to be plotted in 'Data with Trend' facet
  data_long$trend_for_raw[data_long$Decompressed != "Data with Trend"] <- NA
  data_long$NA_replace[data_long$Decompressed != "Data with Trend"] <- NA
  
  # provide mean values for horizontal line
  data_long %<>% mutate(horiz_line = 
                          case_when(Decompressed == "Data with Trend" ~ 
                                      mean(data$Raw, na.rm = TRUE),
                                    Decompressed == "Seasonal" ~ 
                                      mean(data$Seasonal, na.rm = TRUE),
                                    Decompressed == "Trend" ~ 
                                      mean(data$Trend, na.rm = TRUE),
                                    Decompressed == "Remainder" ~ 
                                      mean(data$Remainder, na.rm = TRUE)))
  
  x_segment <-data$Year_Month[1] - (data$Year_Month[6] -data$Year_Month[1])
  y_rem_min <- min(data$Remainder, na.rm = TRUE)
  y_rem_max <- max(data$Remainder, na.rm = TRUE)
  
  data_long %<>% mutate(y_seg_min = horiz_line - abs(y_rem_min),
                        y_seg_max = horiz_line + abs(y_rem_max))
  
  
  gg_plot <-  ggplot(data_long, aes(Year_Month, value, col = Decompressed)) +
    facet_wrap( ~ Decompressed, ncol = 1, scales = "free", strip.position = "right") +
    geom_line(aes(y = horiz_line), na.rm = TRUE,  linetype = "dashed", size = 1) +
    geom_line(aes(y = trend_for_raw),na.rm = TRUE,  col = "cyan", 
              linetype = "solid", size =0.8) +
    geom_point(aes(y = NA_replace), na.rm = TRUE, col = "green", size = 0.5) +
    geom_line() +  # TRUE rows w/ NA are silently removed, no warning
          # FALSE (default), warning indicates # of missing values = NA values, 
          # Raw data, replaced "only" in count column
    geom_segment(aes(x = x_segment, y = y_seg_min, xend = x_segment, yend = y_seg_max), 
                     colour = "darkgrey", size = 2) +
    theme(legend.position = "none")  +
    labs(x = "Year", col = "") +
    ggtitle("Time Series Decompression w/ Mean Horiz. Lines",
            subtitle = "with Function stlplus()")
  
  return(gg_plot)
}


# 
# 
# ggplot_stlplus <- function(data, ...){
#   require(tidyverse)
#   require(magrittr)
#   require(stlplus)   # required for example
#   require(tsibble)   # required for example
#   require(lubridate) # required for example
#   tbl_data <- 
#     as_tibble(rownames_to_column(as.data.frame(data$data), var = "Index"))
#   tbl_data %<>% mutate(time = data$time) %>% 
#     rename(Raw = raw, Seasonal = seasonal, Trend = trend,
#            Remainder = remainder) %>% 
#     dplyr::select(1, time, 2:ncol(.)) # select all with reording column time 
#   # print(tbl_data )
#   
#   tbl_data_long <- tbl_data %>%
#     mutate(trend_for_raw = Trend) %>%    # to add trend line in facet Raw only
#     rename("Data with Trend" = Raw) %>% 
#     pivot_longer(cols = "Data with Trend":"Remainder",
#                  names_to = c("Decompressed"),
#                  values_to = "value") %>% 
#     mutate(Decompressed = factor(Decompressed,
#                                  levels = c("Data with Trend", "Seasonal", 
#                                             "Trend", "Remainder")))
#   tbl_data_long$trend_for_raw[tbl_data_long$Decompressed != "Data with Trend"] <- NA
#   
#   # provide mean values for horizontal line
#   tbl_data_long %<>% mutate(horiz_line = 
#                               case_when(Decompressed == "Data with Trend" ~ 
#                                           mean(tbl_data$Raw, na.rm = TRUE),
#                                         Decompressed == "Seasonal" ~ 
#                                           mean(tbl_data$Seasonal, na.rm = TRUE),
#                                         Decompressed == "Trend" ~ 
#                                           mean(tbl_data$Trend, na.rm = TRUE),
#                                         Decompressed == "Remainder" ~ 
#                                           mean(tbl_data$Remainder, na.rm = TRUE)))
#   
#   gg_plot <-  ggplot(tbl_data_long, aes(time, value, col = Decompressed)) +
#     facet_wrap( ~ Decompressed, ncol = 1, scales = "free" ) +
#     geom_line(aes(y = horiz_line),  linetype = "dashed", size = 1) +
#     geom_line(aes(y = trend_for_raw),  col = "cyan", linetype = "solid", size =0.8) +
#     geom_line(na.rm = TRUE) +  # TRUE rows w/ NA are silently removed, no warning
#     theme(legend.position = "none")  +
#     labs(x = "Year", col = "") +
#     ggtitle("Time Series Decompression w/ Mean Horiz. Lines",
#             subtitle = "with Function stlplus()")
#   
#   return(gg_plot)
#   # return(list(gg_plot = gg_plot,
#   #             tbl_data = tbl_data))
# }
# 
# 
# ## alternartive with Raw & Trend line in one facet and each facet with mean line
# ggplot_stlplus_w_data <- function(data, ...){
#   
#   tbl_data <- 
#     as_tibble(rownames_to_column(as.data.frame(data$data), var = "Index"))
#   tbl_data %<>% mutate(time = data$time) %>% 
#     rename(Raw = raw, Seasonal = seasonal, Trend = trend,
#            Remainder = remainder) %>% 
#     dplyr::select(1, time, 2:ncol(.))
#   
#   tbl_data_long <- tbl_data %>%
#     rename("Raw & Trend Line" = Raw) %>% 
#     pivot_longer(cols =  c("Raw & Trend Line", "Seasonal", "Remainder"),
#                  names_to = c("Decompressed"),
#                  values_to = "value") %>% 
#     mutate(Decompressed = factor(Decompressed,
#                                  levels = c("Raw & Trend Line", "Seasonal", "Remainder")))
#   tbl_data_long$Trend[tbl_data_long$Decompressed != "Raw & Trend Line"] <- NA
#   
#   # pride mean values for horizontal line
#   tbl_data_long %<>% mutate(horiz_line = 
#                               case_when(Decompressed == "Raw & Trend Line" ~ 
#                                           mean(tbl_data$Raw, na.rm = TRUE),
#                                         Decompressed == "Seasonal" ~ 
#                                           mean(tbl_data$Seasonal, na.rm = TRUE),
#                                         Decompressed == "Remainder" ~ 
#                                           mean(tbl_data$Remainder, na.rm = TRUE)))
#   
#   
#   # see also
#   # https://stackoverflow.com/questions/34241890/ggplot-renaming-facet-labels-in-facet-wrap
#   # w/ facet_wrap( ~ Decompressed, labeller=label_parsed) +
#   
#   
#   gg_plot <- ggplot(tbl_data_long, aes(time, value, col = Decompressed)) +
#     facet_wrap( ~ Decompressed, ncol = 1, scales = "free" ) +
#     geom_line(aes(y = horiz_line),  linetype = "dashed", size =0.8) +
#     geom_line(aes(y = Trend),  col = "red", linetype = "solid", size =0.8) +
#     geom_line(na.rm = TRUE) +  # # TRUE rows w/ NA are silently removed, no warning
#     theme(legend.position = "none")  +
#     labs(x = "Year", col = "") +
#     ggtitle("Time Series Decompression w/ Mean Horiz. Lines",
#             subtitle = "with Function stlplus()")
#   
#   return(list(gg_plot = gg_plot,
#               tbl_data = tbl_data))
# }


# Function provides Monthly data plot (e.g. Temperature, Precipitation, ...)
# Return: plot object graph
## w/ smooth, w/o facet_wrap 
ggts_season_w_smooth <- 
  function(data, x = Year_Month, y = count, season = Month) {
    
    graph <- ggplot(data, aes_(substitute(x), substitute(y),
                               col = substitute(season))) +
      geom_point(na.rm = TRUE) +
      geom_smooth(method = "loess", size = 0.5, na.rm = TRUE) +
      labs(x = "Year") +
      # scale_colour_hue(): to get "cold" colours in winter, "warm" in summer
      scale_colour_hue(h.start = -140, l = 60, c = 200) +
      ggtitle("Monthly Data with Local Polynomial Regression Fitting") +
      theme(axis.title.x = x_axis_theme)
    
    return(graph)
  }

# Function provides Season (Wintr, Spring, Summer, Fall) line and smooth plot in
# one commmon diagram
ggts_season <- function(data, span = 1)
{
  data_season <- data %>%
    rename(Winter = Winter_avg, Spring = Spring_avg,
           Summer = Summer_avg, Fall = Fall_avg) %>% 
    pivot_longer(cols = c(Winter:Fall),
                 names_to = c("Season"),
                 values_to = "count")
  
  graph <- 
    ggplot(data_season, aes(x = Year)) +
    # geom_point(col = "blue", shape = 20, na.rm = TRUE) +
    geom_line(aes(y = count, col = Season), linetype = "solid", na.rm = TRUE) +
    geom_smooth(aes(y = count, col = Season), method = "loess",  na.rm = TRUE) +
    # limits: ordering of colours, values: set colour values according ordering
    scale_colour_manual(limits = c("Winter", "Spring", "Summer", "Fall"),
                        values = c("blue", "green",  "orange", "brown")) +
    theme(legend.position = "bottom") +
    labs(x = "Year", y = ylab_m) +
    ggtitle("Average Season Data (w/ Loess Regression Lines", subtitle = 
              "Winter (DJF, DecJanFeb), Spring (MAM), Summer (JJA), Fall (SON)") +
    theme(axis.title.x = x_axis_theme)
  
  
  return(graph)
}

ggts_year_over_month <- function(data, x = Month, y = count, Period = Period) {
  col_scheme <- "YlOrRd"
  
  gg_plot <- ggplot(data, aes_(substitute(x), substitute(y),
                               group = substitute(Period), 
                               col = substitute(Period))) +
    geom_point(shape = 5, na.rm = TRUE) +
    geom_line(na.rm = TRUE) +
    labs(x = "Month") +
    ggtitle(paste0("Monthly Variations of ", substitute(Period),
                   "-counts over Month"),
            subtitle = paste(min(data$Period), 
                             " - ", max(data$Period))) +
    theme(axis.title.x = x_axis_theme) +
    scale_colour_distiller(palette = col_scheme, direction = 1) 
  # scheme for Temp: "YlOrRd"; for Precip: "YlGnBu"
  return(gg_plot)
}

# Function provides yearly/ span*yearly Temperature and Precipitation
# Return: plot object graph


ggts_histo_forecast_resid <- function(test)
{
  # make a histogram of the forecast errors:
  mybinwidth <- IQR(test$.resid)/4     # set to 1/4 of quartile range
  mysd   <- sd(test$.resid)
  mymin  <- min(test$.resid) - mysd*3 # beforehand *5
  mymax  <- max(test$.resid) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  # and with same data length as test$.resid 
  n <- nrow(test)-1
  x <- seq(mymin, mymax, by = (mymax - mymin)/n)
  number <- length(x)
  normal_distr <- tibble(x = x, PDF  = dnorm(x, mean = 0, sd = mysd))
  
  gg_hist <- ggplot(test, aes(.resid, ..density..)) + 
      ggtitle("Histogram of Forecast Residuals",
                subtitle = "with overlaid normal curve") + 
      xlab("Forecast Residuals") + 
      geom_histogram(binwidth = mybinwidth,  
                     col = "black", fill = "red", alpha = 0.7) +
      geom_line(aes(x=normal_distr$x, y=normal_distr$PDF), col = "blue")

  
  # hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  # myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  # points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
  
return(gg_hist)
}


############################### to be deleted ##################################
# only for test purposes:  Forecasting with ETS models
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
