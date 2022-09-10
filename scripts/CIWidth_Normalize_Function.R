# find the average width of the confidence intervals
ci_width_normalize_fn <- function(msi_ci, msi, c) {
  
  # subtract lower c.i. from upper c.i. for all time points and take the mean
  width <- mean(as.matrix(msi_ci[2,2:c] - msi_ci[1,2:c]), na.rm=TRUE)

  if (is.nan(width)) {
    
    width <- NA
    
  }
  
  # calculate mean index value
  mean_index <- mean(as.matrix(as.vector(msi[1:c])), na.rm=TRUE)
  
  if (is.nan(mean_index)) {
    
    mean_index <- NA
    
  }
  
  # normalize confidence interval width by dividing by the mean index value
  norm_width <- width / mean_index
  
  if (is.nan(norm_width)) {
    
    norm_width <- NA
    
  }
  
  return(norm_width)
  
}
