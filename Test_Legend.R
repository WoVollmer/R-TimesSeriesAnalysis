
data_long <- data %>%
   pivot_longer(cols = c(rol_mean:rol_gauss),
               names_to = c("line_data"),
               values_to = "count")

annual_plot <- ggplot(data, aes(x = Year, y = Year_avg)) +
  facet_wrap( ~ Temp_Precip, ncol = 1, scales = "free" ) +
  geom_line(aes(colour = "Yearly Data"), linetype = "solid", na.rm = TRUE) +
  geom_point(col = "blue", shape = 20, na.rm = TRUE) +
  geom_line(aes(y = rol_mean, col = "Rolling Mean"), linetype = "solid", 
            size = 1.3, na.rm = TRUE) +
  geom_line(aes(y = rol_gauss, color = "Rolling Gauss"), linetype = "solid", 
            size = 1.3, na.rm = TRUE) +
  geom_smooth(aes(colour = "Loess Reg"), 
              method = "loess",  size = 1, na.rm = TRUE) +
  geom_smooth(aes(colour = "Linear Reg"),
              method = "lm", size = 1, na.rm = TRUE) +
  labs(colour = "Lines") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.position = "bottom") +
  geom_segment(aes(x=min.year,xend=max.year,y=ref_period, yend=ref_period, 
                   col = "Ref Period"), size = 1,
               linetype = "dashed", show.legend = TRUE) +
  xlim(min.year, max.year) +
  labs(x = "Year", y = ylab) +
  ggtitle(paste("Yearly and ", span, 
                "-years Rolling Mean Data 
                  with Linear and Loess Regression lines"),
          subtitle = paste(city, min.year, " - ", max.year)) +
  theme(axis.title.x = x_axis_theme)

cols <- c("Yearly Data" = "blue", "Rolling Mean" = "red", 
          "Rolling Gauss" = "orange",  "Ref Period" = "black",
          "Loess Reg" = "darkblue", "Linear Reg" = "green")
cols <- c("blue", "red", "orange", "black", "darkblue", "green")
annual_plot + 
  scale_colour_manual(values = c("blue", "red", 
                                 "orange", "black", 
                                 "darkblue", "green"), 
                      limits = c("Yearly Data", "Rolling Mean", 
                                 "Rolling Gauss", "Ref Period", 
                                 "Linear Reg", "Loess Reg"))
annual_plot + scale_colour_manual(values = cols)

# * Blue:    Annual Mean Temperature / Precipitation
# * Red:     Rolling mean with span of `r span` years
# * Orange:  Rolling gaussian filtering with span of `r span` years
# * Green:   Linear Regression line
# * Darkblue: Local Polynomial Regression Fitting (Loess)
# * Black:    Reference Period `r period_years["ref"]`: 
#                    Annual Mean Temperature / Precipitation
#                    

ggplot(data, aes(x = Year)) +
  facet_wrap( ~ Temp_Precip, ncol = 1, scales = "free" ) +
  # geom_point(col = "blue", shape = 20, na.rm = TRUE) +
  geom_line(aes(y = Winter_avg), col = "blue", linetype = "solid", na.rm = TRUE) +
  geom_smooth(aes(y = Winter_avg), method = "loess", col = "blue", size = 1, na.rm = TRUE) +
  geom_smooth(aes(y = Spring_avg), method = "loess", col = "yellow", size = 1, na.rm = TRUE) +
  geom_smooth(aes(y = Summer_avg), method = "loess", col = "orange", size = 1, na.rm = TRUE) +
  geom_smooth(aes(y = Fall_avg), method = "loess", col = "brown", size = 1, na.rm = TRUE) +
  geom_line(aes(y = Spring_avg), col = "yellow", linetype = "solid", na.rm = TRUE) +
  geom_line(aes(y = Summer_avg), col = "orange", linetype = "solid", na.rm = TRUE) +
  geom_line(aes(y = Fall_avg), col = "brown", linetype = "solid", na.rm = TRUE) +
  labs(x = "Year", y = ylab_m) +
  ggtitle("Seasonal Yearly Data",
          subtitle = 
            "Winter (DJF, blue), Spring (MAM, yellow), Summer (JJA, orange), Fall (SON, Brown)")  +
  theme(axis.title.x = x_axis_theme)


data_season <- data %>%
  rename(Winter = Winter_avg, Spring = Spring_avg,
         Summer = Summer_avg, Fall = Fall_avg) %>% 
  pivot_longer(cols = c(Winter:Fall),
               names_to = c("Season"),
               values_to = "count")


ggplot(data_season, aes(x = Year)) +
  facet_wrap( ~ Temp_Precip, ncol = 1, scales = "free" ) +
  # geom_point(col = "blue", shape = 20, na.rm = TRUE) +
  geom_line(aes(y = count, col = Season), linetype = "solid", na.rm = TRUE) +
  geom_smooth(aes(y = count, col = Season), method = "loess",  na.rm = TRUE) +
  # limits: ordering of colours, values: set colour values according ordering
  scale_colour_manual(limits = c("Winter", "Spring", "Summer", "Fall"),
                      values = c("blue", "yellow",  "orange", "brown")) +
  theme(legend.position = "bottom") +
  labs(x = "Year", y = ylab_m) +
  ggtitle("Seasonal Yearly Data", subtitle = 
            "Winter (DJF, DecJanFeb), Spring (MAM), Summer (JJA), Fall (SON)") +
  theme(axis.title.x = x_axis_theme)








