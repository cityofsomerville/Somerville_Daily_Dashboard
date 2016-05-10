# For all of these the date_var should be formatted as a month, day, year
# It goes into the function as the name wrapped in quotes, e.g., "CrimeDate"



## Adds new time variables to your data
add_date_vars <- function(my_data, date_var){
  
  ## Takes the date from data and adds time variables ##
  
  today <- Sys.Date()
  
  my_data$date <- as.Date(my_data[,date_var])
  my_data$year_month <- format(my_data$date, '%Y-%m')
  my_data$month <- format(my_data$date, '%m')
  my_data$year <- format(my_data$date, '%Y')
  my_data$days_ago <- difftime(my_data$date, today, units = "days")
  
  return(my_data)
}




## x-day time series
# Decide the time increments 7, 365, whatever
# group and summarize based on x
make_x_day_ts <- function(my_data, date_var, x_days){
  
  ## Turns the data into a complete time series ##
  ## The time series starts at the end, and groups by periods of X ##
  ## Good for when you have data missing from a ts ##
  ## And for when you want to compare current periods with the past ##
  
  # First we get the daily
  my_data$date <- as.Date(my_data[,date_var])
  
  days <- my_data %>%
    group_by(date) %>% 
    summarise(n = n())
  
  first_day <- min(days$date)
  last_day <- max(days$date)
  
  all_days <- seq.Date(from=first_day, to = last_day, b='days')
  all_days <- all_days  %>%  as.data.frame() 
  colnames(all_days)[1] = "date"
  
  # After this we will have a time series df with every date and how many of the variable
  daily_ts = merge(days, all_days, by='date', all=TRUE)
  daily_ts[is.na(daily_ts)] <- 0
  
  
  
  ## Now x-ly
  # First we get the day number of the last day in our daily time series
  # Because otherwise it ends on Sunday
  # stackoverflow.com/questions/8030812/how-can-i-group-days-into-weeks/8031372#8031372
  period_ending_num <- as.numeric(max(daily_ts$date))
  
  # Ok now we can group by x-day periods that end on the last day of the daily ts
  x_ts <- daily_ts %>% 
    mutate(period = (period_ending_num - as.numeric(date)) %/% x_days) %>% 
    group_by(period) %>%
    summarise_each(funs(sum, max)) %>% #max to get the last date
    arrange(-period)
  
  x_ts <- x_ts[-1,] # Drop first row because it's likely an incomplete period
  
  # Clean up
  x_ts <- x_ts %>% 
    select(date_sum:date_max) %>% 
    select(-date_sum)
  
  names(x_ts) <- gsub("_sum", "", names(x_ts))
  names(x_ts)[2] <- "period_ending"
  
  return(x_ts)
  
}




## x-day time series
# Decide the time increments 7, 365, whatever
# group and summarize based on x
make_x_day_ts_multiple_v <- function(my_data, date_var, x_days, var_of_interest){
  
  ## Turns the data into a complete time series ##
  ## The time series starts at the end, and groups by periods of X ##
  ## Good for when you have data missing from a ts ##
  ## And for when you want to compare current periods with the past ##
  ## This one takes multiple observations from a variable of interest 
  ## and makes TS for each unique observation
  
  # First we get the daily
  my_data$date <- as.Date(my_data[,date_var])
  my_data$v <- my_data[,var_of_interest]
  
  # Blanks seemed to F up everything
  my_data$v <- gsub("^$|^ $", "my_weird_blank", my_data$v)
  
  days <- my_data %>%
    filter(v != "my_weird_blank") %>% 
    group_by(date, v) %>%
    summarise(n = n()) %>% 
    ungroup() %>% 
    spread(v, n)
  
  first_day <- min(days$date)
  last_day <- max(days$date)
  
  all_days <- seq.Date(from=first_day, to = last_day, b='days')
  all_days <- all_days  %>%  as.data.frame() 
  colnames(all_days)[1] = "date"
  
  # After this we will have a time series df with every date and how many of the variable
  daily_ts = merge(days, all_days, by='date', all=TRUE)
  daily_ts[is.na(daily_ts)] <- 0
  
  
  ## Now x-ly
  # First we get the day number of the last day in our daily time series
  # Because otherwise it ends on Sunday
  # stackoverflow.com/questions/8030812/how-can-i-group-days-into-weeks/8031372#8031372
  period_ending_num <- as.numeric(max(daily_ts$date))
  
  # Ok now we can group by x-day periods that end on the last day of the daily ts
  x_ts <- daily_ts %>% 
    mutate(period = (period_ending_num - as.numeric(date)) %/% x_days) %>% 
    group_by(period) %>%
    summarise_each(funs(sum, max)) %>% #max to get the last date
    arrange(-period)
  
  x_ts <- x_ts[-1,] # Drop first row because it's likely an incomplete period
  
  # Clean up
  x_ts <- x_ts %>% 
    select(date_sum:date_max) %>% 
    select(-date_sum)
  
  names(x_ts) <- gsub("_sum", "", names(x_ts))
  names(x_ts)[ncol(x_ts)] <- "period_ending"
  
  return(x_ts)
  
}




### Time series statistical comparisons ###
comp_last_day_avg <- function(my_data, date_var){
  
  ## Compare last day to average apples to apples
  # Turns the data into a complete time series #
  # Filters by week day, or week end, depending on the last day
  # Then compares the last day to the rest to see if it is 
  # significantly above average or not
  
  # First we get the daily
  daily_ts <- make_x_day_ts(my_data, date_var, 1)
  
  # Now find the type
  daily_ts <- daily_ts %>% 
    mutate(day_type = ifelse(wday(period_ending) == 1, "weekend",
                             ifelse(wday(period_ending) == 7, "weekend",
                                    "weekday")))
  
  last_day_type <- daily_ts$day_type[which.max(daily_ts$period_ending)]
  
  # Filter for that type
  daily_ts <- filter(daily_ts, day_type == last_day_type)
  
  # Now find the means and sd
  avg_n <- mean(daily_ts$n)
  stdev <- sd(daily_ts$n)
  
  last_day_n <- daily_ts$n[which.max(daily_ts$period_ending)]
  delta <- last_day_n - avg_n
  
  comparison <- ifelse(delta > 0 & delta > stdev, "significantly above average",
                       ifelse(delta > 0 & delta < stdev, 
                              "slightly above average",
                              ifelse(delta < 0 & abs(delta) > stdev, 
                                     "significantly below average",
                                     ifelse(delta < 0 & abs(delta) < stdev, 
                                            "slightly below average",
                                            "average"))))
  
  return(paste(comparison, "for a", last_day_type))
  
}




### sort your time series by growth in the x time period ###
sort_by_ts_statistical_growth <- function(my_data, date_var, x_days, var_of_interest, n_threshold){
  
  ## if you have time-series data with lots of different observations from a certain variable 
  ## this will compare them to see which observations grew the most, statistically speaking, 
  ## in a time period of x,
  ## with some threshold of what you consider to be a small n 
  ## It's basically a way of spotting time-series anomalies in the present period of x ##
  
  ### First we get a time series
  ts <- make_x_day_ts_multiple_v(my_data, date_var, x_days, var_of_interest)
  # to test ts <- make_x_day_ts_multiple_v(my_data, "CrimeDate", 10, "District")
  
  
  
  #### Now make a few datasets and combine them
  ts_for_names <- ts %>% 
    select(-period_ending) 
  
  # Pause to get the row names
  new_row_names <- colnames(ts_for_names)
  
  
  
  ### the last time period 
  ts_final <- ts %>% 
    select(-period_ending) %>% 
    tail(n = 1) %>% 
    t() %>% 
    data.frame() %>% 
    mutate(v_names = new_row_names)
  
  colnames(ts_final)[1] <- "final"
  
  
  
  ### the last time period expressed as a Z-Score
  ts_z <- ts %>% 
    select(-period_ending) %>% 
    scale() %>% 
    data.frame() %>% 
    tail(n = 1) %>% 
    t() %>% 
    data.frame() %>% 
    mutate(v_names = new_row_names)
  
  colnames(ts_z)[1] <- "final_z_score"
  
  
  
  ### the average for all time periods
  ts_mean <- ts %>% 
    select(-period_ending) %>% 
    colMeans()
  
  ts_mean <- data.frame(ts_mean, new_row_names)
  ts_mean <- rename(ts_mean, v_names = new_row_names)
  
  
  
  ## Now combine
  all_together <- merge(ts_final, ts_mean, by = "v_names")
  all_together <- merge(all_together, ts_z, by = "v_names")
  
  
  # Increase in final over the average
  all_together$per_increase_final <- (all_together$final - all_together$ts_mean) / all_together$ts_mean
  
  
  # Sort and take out small n, because otherwise the largest increases will be with the lame ones
  all_together <- all_together %>% 
    filter(final > n_threshold) %>% 
    arrange(-final_z_score)
  
  
  return(all_together)
  
}
