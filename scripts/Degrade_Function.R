# function to shorten and degrade time series
degrade_ts_fn <- function(all_pops_index, c, mlength=10, numobs=5) {
  
  # create a Poisson distribution around the mean length of time series
  length_dist <- rpois(1000, mlength)
  
  # create a Poisson distribution around the mean number of observations of t.s.
  numobs_dist <- rpois(1000, numobs)
  
  # loop through time series
  for (i in 1:nrow(all_pops_index)) {
    
    # randomly set length of time series from Poisson distribution
    # restricted between 2 and the starting length of the t.s.
    length_ts <- sample(length_dist[(length_dist>=2) & (length_dist<=c)], 1)
    
    # randomly set number of observations of t.s. from Poisson distribution
    # restricted between 2 and the new mean length of t.s.
    numobs_ts <- sample(numobs_dist[(numobs_dist>=2) & (numobs_dist<=length_ts)], 1)
    
    #randomly set start year for the t.s. based on length
    start_ts_year <- sample(1:(c-length_ts+1), 1)
    
    #calculate end year for the t.s. based on start year and length
    end_ts_year <- start_ts_year + length_ts - 1
    
    # cut the time series to the correct length
    cut_ts <- all_pops_index[i,start_ts_year:end_ts_year]
    
    # calculate how many observations to replace with NA
    remove_obs <- length_ts - numobs_ts
    
    # degrade time series to the randomly selected number of observations
    cut_ts[which(cut_ts %in% sample(cut_ts, remove_obs))] <- NA
    
    # fill time series with NAs
    all_pops_index[i,1:c] <- NA
    
    # put cut ts back in
    all_pops_index[i,start_ts_year:end_ts_year] <- cut_ts
    
  }
  
  return(all_pops_index)
  
}
