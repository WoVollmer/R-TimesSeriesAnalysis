library(zoo)
library(tseries) # Finance data can be easily queried using the get.hist.quote

suppressWarnings(RNGversion("3.5.0"))
set.seed(1)

x.Date <- as.Date(paste(2004, rep(1:4, 4:1), sample(1:28, 10), sep = "-"))
x <- zoo(rnorm(12), x.Date)

## rolling operations for univariate series
rollmean(x, 3)
rollmax(x, 3)
rollmedian(x, 3)
rollsum(x, 3)

## rolling operations for multivariate series
xm <- zoo(matrix(1:12, 4, 3), x.Date[1:4])
rollmean(xm, 3)
rollmax(xm, 3)
rollmedian(xm, 3)
rollsum(xm, 3)

## rollapply vs. dedicated rollmean
rollapply(xm, 3, mean) # uses rollmean
rollapply(xm, 3, function(x) mean(x)) # does not use rollmean

yearly_plot_data_zoo <- yearly_plot_data %>% 
  group_by(Temp_Precip) %>%  
  mutate(rol_mean_zoo = rollmean(Year_avg, span, fill = NA),
       rol_apply = rollapply(Year_avg, span, function(x) mean(x, na.rm = TRUE), fill = NA), 
          rol_apply_zoo = rollapply(Year_avg, span, mean, fill = NA)) %>% 
  ungroup()

View(yearly_plot_data_zoo)

######################## zoo-faq ##################
# A "zoo" series with duplicated indexes
(zoo(1:8, c(1, 2, 2, 2, 3, 4, 5, 5)))
z <- suppressWarnings(zoo(1:8, c(1, 2, 2, 2, 3, 4, 5, 5)))
z

# Fix it up by averaging duplicates:
aggregate(z, identity, mean)


# Or, ﬁx it up by taking last in each set of duplicates:
aggregate(z, identity, tail, 1)


# Fix it up via interpolation of duplicate times
time(z) <- na.approx(ifelse(duplicated(time(z)), NA, time(z)), na.rm = FALSE)
time(z)

# If there is a run of equal times at end they wind up as NAs
# and we cannot have NA times.
z[!is.na(time(z))]


## 4. 
DF <- data.frame(time = 2001:2006, x = 11:16,
                 f = factor(letters[c(1, 1, 2, 2, 1, 3)]))
zx <- zoo(DF$x, DF$time)
zf <- zoo(DF$f, DF$time)
str(zx)
str(zf)


z <- zoo(data.matrix(DF[-1]), DF$time)
z


