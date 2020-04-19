require(tidyverse)
require(magrittr)
require(tsibble)

#' @title Utilities for time series analysis and data reforming
#'
#' @description calculate for Jan:Dec counts yearly and seasonal mean data/month
#'  note for e.g. Precipitation unit: mm/month => for mm/year multiply by 12.
#'  1)  seasonal values are calculated from the monthly data by averaging the 
#'  three monthly values
#'  2)  Seasons are Dec-Feb (DJF), Mar-May (MAM), June-Aug (JJA), Sept-Nov (SON)
#'  3)  Winter: year refers to January (realized by lag()).
#' @param data frame 
#' @details The data input nust be ...
#'
#' @return data frame  
#' Year "unchanged columns" Jan ... Dec Year_avg Winter_avg Spring_avg Summer_avg Fall_avg
#' 1659                      4.0 6.0 7.0 11.0  13.0  16.0  16.0  13.0  10.0 5.0   2.0 
#' :
#' 1889                      -3.1 -2.09 ...-1.01      8.28    -1.27      8.97       18.4
#' @encoding UTF-8
#' @md
#' 
uts_gen_yearly_seasonal_avg <- function(data, season = TRUE) {
  # calculate for Temperature/Precipitation yearly and seasonal mean data/month 
  #         note for Precipitation: to be multiplied by 12 if mm/year is required
  # 1)  seasonal values are calculated from the monthly data by averaging the 
  #     three monthly values
  # 2)  Seasons are Dec-Feb (DJF), Mar-May (MAM), June-Aug (JJA), Sept-Nov (SON).
  # 3)  Winter: year refers to January (realized by lag())  

# mean of year for all years grouped by (City, Measure, Year)
  data_yearly <- data %>%
    index_by(Year = ~ year(.)) %>%
  # data_yearly <- as_tsibble(data, index =  Year, key = Measure) %>%
    as_tibble() %>% 
    group_by(City, Measure, Year) %>% 
    summarise(Year_avg = mean(count))  # na.rm = TRUE not feasible for mean(year)
  # slice(data_yearly, 1:6)
  
  if (season) {
    # mean of season for all years grouped by (City, Measure, Year_Season)
    mean_season <- data %>%  
      group_by(City, Measure) %>%  
      # important: grouping before call lag(), otherwise rolling over Measure
      mutate(count_lag = lag(count)) %>% 
      index_by(Year_Season = ~ yearquarter(.)) %>%  
      as_tibble() %>% 
      group_by(City, Measure, Year_Season) %>% 
      summarise(Season_avg = mean(count_lag)) %>% 
      mutate(Year = year(Year_Season),
             Season = quarter(Year_Season))
    # slice(mean_season, 1:6)
    
    mean_season_wide <- mean_season %>%
      pivot_wider(id_cols = c(City, Measure, Year),
                  names_from = Season,
                  values_from = Season_avg) %>%
      rename(Winter_avg = '1', Spring_avg = '2', Summer_avg = '3', Fall_avg = '4')
    # slice(mean_season_wide, 1:6)
    
    data_yearly <- inner_join(mean_season_wide, data_yearly)
  }     
  return(data_yearly)
}

#' check and fill missing data with NAs w/ {tsibble} functions
#' - ts objects don't allow missing years or months
#' and provide output in wide Month format (Year Temp_Precip Jan Feb .... Dec)
#'  
#' data input format (Month in long format)
#'    Year Month Temp_Precip   count
#'   <dbl> <dbl> <chr>         <dbl>
#' 1  1887     1 Temperature    NA  
#' 2  1887     1 Precipitation   4  
#' 3  1887     2 Temperature    NA
#' :
#' 5  2019    12 Temperature    4.55
#' 6  2019    12 Precipitation 30.4 
#' 
#' 
#' data output format (Month in wide format)
#'    Year Temp_Precip   Jan   Feb  ... Dec 
#'   <dbl> <fct>       <dbl> <dbl> ... <dbl>
#' 1  1887 Temperature NA    NA  ... NA       
#' :
#' 3  1889 Temperature -3.1 -2.09 ...-1.01 
#' 
#'  test w/ tsibble - ts objects don't allow missing years
uts_data_check_and_fill_w_na <- function(data, add_precip = FALSE, key = NULL) {
    if (add_precip) {
      # generate "Precipitation" NA data (e.g. England w/o Precip data)
      new_precip <- bind_rows(data[1,], 
                            data[nrow(data),]) %>% 
      mutate(Measure = "Precipitation", 
             count = NA)
    data <- bind_rows(data, new_precip)
  }
  data %<>% 
    unite("Year_Month", Year, Month, sep = "-", remove = FALSE) %>% 
    mutate(Year_Month = yearmonth(Year_Month))
  # yearmonth("1997-1") = yearmonth("1997-Jan") = "1997 Jan"
  # date(yearmonth("1997-Jan")) = "1997-01-01"
  
  # fill_gaps with NAs since (since ts objects don't allow missing years)
  data_tsbl <- 
    as_tsibble(data, index = Year_Month, key = key) 
  (data_tsbl %>% has_gaps())   # Check for gaps
  (data_tsbl %>% scan_gaps())  # Reveal
  (data_tsbl %>% count_gaps()) # Summarise - number of gaps from - to
  data_tsbl %<>% fill_gaps() # Fill in time gaps
  (data_tsbl %>% has_gaps())   # Check for gaps
  
  return(data_tsbl)
}


#' Function converts stlplus() output to tibble w/ interpolated NA replacements 
uts_stlplus_as_tibble <- function(stlplus_data) {
  
  stlplus_new <- as_tibble(
    rownames_to_column(as.data.frame(stlplus_data$data), var = "Index")) %>%
    rename(Raw = raw, Seasonal = seasonal, Trend = trend, 
           Remainder = remainder) %>% 
    mutate(Year_Month = yearmonth(stlplus_data$time),
           Year = year(Year_Month),
           Month = factor(month(Year_Month)),
           Interpolated = Seasonal + Trend,           
           Remainder = case_when(is.na(Remainder) ~ 0, 
             # since count repl. by interpolated => new remainder for ex NA: = 0
                                 TRUE ~ Remainder),
           Seasonal_Adjust = Trend + Remainder, # = count - seasonal) 
           NA_replace = case_when(is.na(Raw) ~ Interpolated, 
              # to allow plots line(Raw) + line/points(NA_replace) = count line
                                  TRUE ~ NA_real_),
           count = case_when(is.na(Raw) ~ Interpolated,
                             TRUE ~ Raw)) %>% 
    dplyr::select(Year_Month, Year, Month, count, Raw, Interpolated,  Trend, 
                  Seasonal, Remainder, Seasonal_Adjust, NA_replace)
  levels(stlplus_new$Month) <- month.abb
  
  return(stlplus_new)
}

#' are there any NAs in data_monthly$count
#' => replace NA by interpolation separate for each time series
uts_interpolate_na <- function(data, freq) {
  
  n_na <- as_tibble(data) %>% summarise(nr_na = sum(is.na(count))) %>% sum()
  
  if (n_na == 0)  return(data)  # nothing to be done
                               # check needed for uts_missls()
  
  # ensure that first and last yearl data are not NAs
  first_year_w_Jan_count <- data %>% 
    filter(Month == "Jan" & !is.na(count)) %$% 
    min(Year)
  last_year_w_Dec_count <- data %>% 
    filter(Month == "Dec" & !is.na(count)) %$% 
    max(Year)
  
  data <- data %>% 
    filter(Year >= first_year_w_Jan_count & Year <= last_year_w_Dec_count)
  n_row <- 
  n_na  <- as_tibble(data) %>% summarise(nr_na = sum(is.na(count)))
  if (n_na == 0)  return(data)  # nothing more to be done
                                # check needed for uts_missls()
  
  data_ts <- data %$% ts(count, start=c(first_year_w_Jan_count, 1), frequency = freq)
  data_ts <- uts_missls(data_ts, p=3, 1) 
  
  # to allow plots line(Raw) + line/points(NA_replace) = count line 
  # and easy check between 
  #    previous NAs in RAW (old count) and 
  #    replaced NAs in updated count
  #    column Raw, NA_replace and Interpolated (return of uts_missls) added
  data %<>% mutate(Raw = count,
                   Interpolated = as.vector(data_ts),
                   NA_replace = case_when(is.na(Raw) ~ Interpolated, 
                                          # to allow plots line(Raw) + line/points(NA_replace) = count line
                                          TRUE ~ NA_real_),
                   count = case_when(is.na(Raw) ~ Interpolated,
                                     TRUE ~ Raw))
}




#   missls         : filling in missing values in time series            
#   ldrec          : Levinson-Durbin recursion 
#   interpol       : auxiliary function for missls
 
##  uts_missls
##
##  Purpose : Minimum Mean Square Error Interpolator to fill missings
##            using LS approach
##
##  Format  :  y = missls(x,p=0,tol=0.001,theo=0)
##
##  Input   :  x = (n,1)-vector, time series with missings
##             p = scalar, 0, or prespecified order of AR modell
##  Output  :  y = (n,1) vector, completed time series
##  Remarks :  first and last observation mut not be missing
##             tolerance can be set through variable tol 
##             it enters via tol*sd(x,na.rm=TRUE) 
##             prespecified  iacf can be incorporated trough variable   
##             theo = (k,1)-vector, prespecified iacf (starting at lag 1) 
##  Reference : Source: Rainer Schlittgen (01.10.2014) tsutil.r missls()
##  Brubacker, S. and Wilson, G. (1976): Interpolating time series
##            with applications to the estimation of holiday effects   
##            on electricity demand  
##            Journal of the Royal Statistical Society, C, 25, 107-116
##



uts_missls <- function(x,p=0,tol=0.001,theo=0){ 
  
  n <- length(x)   
  if(length(x[!is.na(x)]) == n){ stop("no missing observation")  } 
  if(is.na(x[1])|is.na(x[n])){ stop("First and last observation must not be missing")  }    
  
  mu <- mean(x, na.rm = TRUE)   
  xcent <- x-mu 
  tol <- tol*sd(x, na.rm = TRUE) 
  
  if(theo==0){                    # fitting of an AR[p] model    
    if(p==0) { p <- trunc(n/10) } 
    # estimation of ACF
    indexf <- c(1:n)
    indexf <- indexf[is.na(x)]  
    y <- xcent
    y[indexf] <- 0           
    g <- 1*(!is.na(xcent))     
    ycov <- acf(y,p,type="covariance",demean=FALSE,plot=FALSE) 
    ycov <- ycov$acf  
    gcov <- acf(g,p,type="covariance",demean=FALSE,plot=FALSE)  
    gcov <- gcov$acf  
    xcov <- ycov/gcov 
    xcor <- xcov/xcov[1]  
    mat <- uts_ldrec(xcor)               # Compute Levinson-Durbin recursion   
    a <- mat[p,1:p]                      # select AR coefficients    
    rho <- as.vector(ARMAacf(ma = -a, lag.max = p))     #  iacf   
    rho <- rho[-1] 
  }else{ 
    rho <- theo  
    p <- length(rho) 
  }  
  wieder <- 0 
  abbruch <- 0  
  while(abbruch == 0){  
    z <- interpol(rho,xcent)   
    if(theo == 0){      
      aneu <- ar(z, aic = FALSE, order.max = p, method= "yule-walker")  
      aneu <- aneu$ar
      if (max(abs(a-aneu)) < tol) { abbruch <- 1  }          
      else{
        a <- aneu  
        rho <- as.vector(ARMAacf(ma = -a, lag.max = p)) # new iacf 
        rho <- rho[-1]   
      }
    }else{ abbruch <- 1 } 
    wieder <- wieder+1 
    if(wieder > 20)  {abbruch <- 1}
  }  
  out <- z+mu 
  return(out) 
} 


##  uts_ldrec
##
##   Levinson-Durbin recursion for determing all coefficients a(i,j),
##   mat <- ldrec(a)
##   1<=i<=j<=maxlag (=p)
##   input  : a is (p+1,1)-vector of acf of a time series: acov(0),...,acov(p)
##                              or  1,acor(1),..,acor(p)
##   output : (p,p+2)-matrix with coefficients in lower triangular,
##            pacf in colum p+2 and Q(p) in colum p+1
##   Reference : Source: Rainer Schlittgen (01.10.2014) tsutil.r ldrec()          


uts_ldrec <- function(a){ 
  p <- length(a)-1 
  mat <- matrix(0,p,p+2) 
  acor <- a/a[1]
  acor <- acor[-1] 
  mat[1,1] <- acor[1];  mat[1,p+1] <- 1-acor[1]^2;   mat[1,p+2] <- acor[1] 
  i <- 1 
  while(i < p){
    mat[i+1,i+1] <- (acor[i+1] - sum(mat[i,1:i]*acor[i:1]))/mat[i,p+1] 
    mat[i+1,1:i] <- mat[i,1:i]-mat[i+1,i+1]*mat[i,i:1]   
    mat[i+1,p+2] <- mat[i+1,i+1] 
    mat[i+1,p+1] <- mat[i,p+1]*(1-mat[i+1,p+2]^2) 
    i <- i+1;
  }
  mat
}

#   interpol       : auxiliary function for missls
interpol <- function(rho,xcent){ 
  
  n <- length(xcent)                     
  p <- length(rho)  
  fehl <- c(1:n)
  fehl <- fehl[is.na(xcent)]      
  m <- length(fehl)  
  z <- xcent
  z[fehl] <- 0  
  zt <- rep(0,m)                     # \tilde{z}  
  s <- fehl[1]  
  k <- 1 
  while( k <= m ){
    i <- fehl[k]-s 
    bis1 <- min(c(n-i-s,p))  
    bis2 <- min(c(s+i-1,p))  
    zt[k] <- -( sum(rho[1:bis1]*z[(s+i+1):(bis1+s+i)])
                + sum(rho[1:bis2]*z[(s+i-1):(s+i-bis2)]) )                
    k <- k+1 
  } 
  mat <- diag(rep(1,m)) 
  k <- 1 
  while( k < m ){
    dist <- fehl[(k+1):m]-fehl[k] 
    if( min(dist) <= p ){
      lp <- c(1:length(dist))
      lp <- lp[(dist > 0)&(dist <= p)] 
      mat[k,k+lp] <- t(rho[dist[lp]])  
      mat[k+lp,k] <- t(mat[k,k+lp])  
    } 
    k <- k+1 
  }    
  neu.lm <- lm.fit(mat,zt)
  z[fehl] <- neu.lm$coefficients 
  return(z)
} 