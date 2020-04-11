
# see https://en.wikipedia.org/wiki/Exponential_growth

library(tidyverse)

infected_0 <- 1000
doubling_time <- c(2,9,4)
days <- seq(0, 32, by = 1)

(infected_tbl <- tibble(
  days = days, 
  infected_T2 = infected_0*2^(days/(doubling_time[1])),
  infected_T3 = infected_0*2^(days/(doubling_time[2])),
  infected_T4 = infected_0*2^(days/(doubling_time[3])),
  log_T2 = log10(infected_T2),
  log_T3 = log10(infected_T3),
  log_T4 = log10(infected_T4)))

log10(infected_0) + (days/doubling_time)*log10(2)
log2(infected_0) + (days/doubling_time)*log2(2)


ggts_counts_test <- function(data, x = Date, y = Count, col = count_type) {
  col_scheme <- "Set1" # "RdYlGn" #"YlOrRd" #"Oranges" # "YlGnBu" # 
  ggplot(data, aes_(substitute(x), substitute(y), col = substitute(col))) +
    # facet_wrap(vars(count_type), ncol = 1, scales = "free",
    #            strip.position = "left") +
    geom_point(size = 1.5, na.rm = TRUE) +
    geom_line(size = 1, na.rm = TRUE) +  
  #  theme(legend.position = "none")  +
    labs(y = "") + 
    ggtitle(paste("Doubling Time T =", doubling_time, "days"))
}  

infected_tbl_long <-  infected_tbl %>% 
  pivot_longer(cols = starts_with("infected"), 
             names_to = "count_type",
             values_to = "count")

plot_lin_scale <- ggts_counts_test(infected_tbl_long, days, count, count_type) +
  labs(subtitle = "linear y-scale")

plot_log10_scale <- ggts_counts_test(infected_tbl_long, days, count, count_type) +
  scale_y_log10() +
  labs(subtitle = "log10 y-scale")

gridExtra::grid.arrange(plot_lin_scale, plot_log10_scale, ncol = 1)


# ts_corona <- infected_tbl %>% 
#   as_tsibble(index = days)
# forc_countries <- c("Germany")
reg_range <- 10

ts_filt_corona <- ts_corona %>% ungroup() %>% group_by(count_type) %>% 
  filter(Country %in% forc_countries & Date >= Sys.Date() - reg_range &
           count_type != "Recovered") %>% 
  select(Country, count_type, Date, count)

# linear regression of log data

ts_filt_corona %>% 
  ggplot(aes(x=Date, y=log(count))) +
  labs(x = "Date",
       title = paste("Comparison selected countries - Cumulated Cases with Linear Regression (log10 scale)")) +
  geom_point() +
  geom_line(aes(col = Country), size = 1) +
  geom_smooth(method="lm", aes(col = Country), lty = "dashed", se=FALSE) +
  facet_wrap(vars(count_type), ncol = 2, scales = "free",
             strip.position = "left") 


fit_corona <- ts_filt_corona %>% ungroup() %>% 
  group_by(count_type) %>% 
  model(TSLM(log(count) ~ Date)) 
fc_corona <- fit_corona %>% fabletools::forecast(h = "14 days") 

i <- "Germany"
fc_corona %>%  filter(Country == i) %>% 
  autoplot(ts_filt_corona, level = 95) +
  ggtitle(paste(i, "- Cases and 14-days Forecast on linear y-scale"),
            subtitle = "Excample to show the extreme fast increase on linear scale") +
  labs(x = "Days") +
  # scale_y_log10() +
  scale_x_date(date_labels = "%b %d", date_breaks = "7 days") +
  facet_wrap(~ count_type, ncol = 2,  scales = "free",
             strip.position = "left")


