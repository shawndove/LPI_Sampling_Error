# find the average width of the confidence intervals
ci_width_fn <- function(msi_ci, c) {
  
  # subtract lower c.i. from upper c.i. for all time points and take the mean
  width <- mean(as.matrix(msi_ci[2,2:c] - msi_ci[1,2:c]), na.rm=TRUE)
  
  if (is.nan(width)) {
    
    width <- NA
    
  }
  
  return(width)
  
}