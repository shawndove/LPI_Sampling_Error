# test how much of the real trend is within the confidence intervals of the sampled trend
real_finaltp_within_ci_fn <- function(msi_ci, true) {
  
  # get number of time points 
  maxtp <- length(true)
  
  # check if confidence intervals are valid
  if ((length(which(msi_ci[1,]>0)) < 2) & (length(which(msi_ci[2,]>0)) < 2)) {
    
    return(NA)
    
  } else if ((length(which(!is.na(msi_ci[1,]))) <2) & (length(which(!is.na(msi_ci[2,]))) <2)) {
    
    return(NA)
    
  }
  
  # create a vector to check if the final time point of the true trend false outside of the sample confidence intervals
  false.count <- NA
  
  # put upper confidence interval into a vector
  test.upper <- msi_ci[2,][maxtp]
    
  # put lower confidence interval into a vector
  test.lower <- msi_ci[1,][maxtp]
    
  # put true trend into a vector
  test <- true[maxtp]
    
  # check for NAs in confidence intervals
  if (!is.na(test.upper) & !is.na(test.lower)) {
      
    # check if the true trend is above the upper c.i.
    if (test > test.upper) {
        
      # if so, count as false
      false.count <- 1
        
      # check if the true trend is below the lower c.i.
    } else if (test.lower > test) {
        
      # if so, count as false
      false.count <- 1
        
    } else {
      
      false.count <- 0
      
    }
      
    # if there is an NA value in the upper or lower confidence interval...  
  } else {
     
    false.count <- NA 

  }

return(false.count)
  
}
