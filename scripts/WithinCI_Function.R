# test how much of the real trend is within the confidence intervals of the sampled trend
real_trend_within_ci_fn <- function(msi_ci, true) {
  
  # check if confidence intervals are valid
  if ((length(which(msi_ci[1,]>0)) < 2) & (length(which(msi_ci[2,]>0)) < 2)) {
    
    return(NA)
    
  } else if ((length(which(!is.na(msi_ci[1,]))) <2) & (length(which(!is.na(msi_ci[2,]))) <2)) {
    
    return(NA)
    
  }
  
  # create a vector to count how many times the true trend falls outside of the confidence intervals
  false.count <- 0
  
  # loop over each time point
  for (i in 1:length(msi_ci)) {
    
    # put upper confidence interval into a vector
    test.upper <- msi_ci[2,][i]
    
    # put lower confidence interval into a vector
    test.lower <- msi_ci[1,][i]
    
    # put true trend into a vector
    test <- true[i]
    
    # check for NAs in confidence intervals
    if (!is.na(test.upper) & !is.na(test.lower)) {
      
      # check if the true trend is above the upper c.i.
      if (test > test.upper) {
        
        # if so, increase the count
        false.count <- false.count + 1
        
        # in this case we do not need to test the lower c.i., so move to next time point
        next
        
        # check if the true trend is below the lower c.i.
      } else if (test.lower > test) {
        
        # if so, increase the count
        false.count <- false.count + 1
        
      }
      
      # if there is an NA value in the upper or lower confidence interval...  
    } else {
      
      # move to the next time point
      next
      
    }
    
  }
  
  # calculate the percentage of non-NA time points where the true trend was within the c.i.'s
  # we remove 1 from the length because the first time point has no confidence intervals
  p.cent <- 100 - (false.count / (length(!is.na(msi_ci[1,]))-1) * 100)
  
  return(p.cent)
  
}
