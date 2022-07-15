# function to introduce observation error to time series
error_intr_fn2 <- function(all_pops_index, m_colnames, mean_cv, cv_sd) {
  
  # remove zeros and convert to log
  #all_pops_index2 <- log(all_pops_index[,1:length(m_colnames)] + 0.0000001)
  
  # create matrix to hold new index values
  all_pops_error <- matrix(data = NA, nrow = nrow(all_pops_index), ncol= length(m_colnames))
  
  # find the difference between the mean and minimum
  #mean_min_diff <- mean_cv - min_cv
  
  # find the difference between the maximum and the mean
  #mean_max_diff <- max_cv - mean_cv
  
  # the standard deviation for the cv distribution is calculated as one third of
  # the difference between the max and mean or mean and min, whichever is smaller
  # because a normal distribution of 1000 values should give max and min values 
  # of roughly 3x the standard deviation (this is subject to chance)
  #cv_sd <- min(c(mean_min_diff, mean_max_diff)) / 3
  
  # generate distribution of cv values
  cv_dist <- rnorm(1000, mean = mean_cv, sd = cv_sd)
  
  # remove negative values
  cv_dist <- cv_dist[cv_dist >= 0]
  
  # sample cv value for each row of the dataset, with replacement
  row_cv <- sample(cv_dist, size = nrow(all_pops_index), replace = TRUE)
  
  # loop through rows
  for (i in 1:nrow(all_pops_index)) {
    
    # loop through observations
    for (j in 1:(length(m_colnames))) {
      
      # create a normal distribution of index values with actual index value as mean and std_val as st. dev.
      # note that this uses the absolute value of the normal distribution to avoid negative values
      distribution <- rnorm(1000, mean = all_pops_index[i,j], sd = all_pops_index[i,j] * row_cv[i])
      
      distribution_nz <- distribution[distribution > 0] # remove zeros
      
      new_ival <- sample(distribution_nz, 1) # sample from distribution to get new index value with error
      
      # new_ival <- sample(distribution, 1) # sample from distribution to get new index value with error
      
      all_pops_error[i,j] <- new_ival # put new index value into matrix
      
    }
    
  }
  
  # back convert from log
  #all_pops_error <- exp(all_pops_error)
  
  # adjust all indices to begin at a base value of 100
  all_pops_error <- all_pops_error / all_pops_error[,1] * 100
  
  all_pops_error <- as.data.frame(all_pops_error) # convert to data frame
  
  names(all_pops_error) <- m_colnames # get column names from original data frame
  
  all_pops_error$SpecID <- all_pops_index$SpecID # get species ID values from original data frame
  
  all_pops_error$PopID <- all_pops_index$PopID # get population ID values from original data frame
  
  all_pops_error$GrpID <- all_pops_index$GrpID # get group ID values from original data frame
  
  return(all_pops_error)
}
