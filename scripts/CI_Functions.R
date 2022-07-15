## confidence interval functions

ci_fn <- function(index, c, m_colnames, boots=100, n=NA, weights=NA, lambda=FALSE, savedata=FALSE) {
  
  # check whether data is in list format
  if (inherits(index, "list")) {
    
    # if it is, convert the list to a data frame
    index.matrix <- do.call(rbind, index)
    
  } else {
    
    # if it is not a list, copy the data frame directly
    index.matrix <- index
    
  }
  
  # select the whole dataset
  grp_specdata <- index.matrix
  
  # check if these are already lambda values
  # if they are, there will be one less column than in m_colnames
  # so we test if the final element of m_colnames is missing from the column names
  if (!(tail(m_colnames, 1) %in% colnames(grp_specdata))) {
    
    # if they are already lambda values, remove ID tags
    grp_specdata1.4 <- grp_specdata[,1:(c-1)]
    
    # if they are not already lambda values
  } else {
    
    # remove ID tags
    grp_specdata1.1 <- grp_specdata[,1:c]
    
    # check if the lambda flag is set to TRUE (LPI mode)
    if (lambda==TRUE) {
      
      # if so, convert to lambda values
      grp_specdata1.2_pt1 <- grp_specdata1.1[,1:(ncol(grp_specdata1.1)-1)]
      grp_specdata1.2_pt2 <- grp_specdata1.1[,2:ncol(grp_specdata1.1)]
      grp_specdata1.3 <- grp_specdata1.2_pt2 / (grp_specdata1.2_pt1)
      
      # if lambda flag is set to FALSE
    } else {
      
      # update variable name
      grp_specdata1.3 <- grp_specdata1.1
      
    }
    
    # convert to log10
    grp_specdata1.4 <- log10(grp_specdata1.3)
    
  }
  
  # create matrix to hold sampled species lambdas/indices
  grp_samp_list <- list()
  
  # loop to bootstrap sampling process
  for (i in 1:boots) {
    
    # create matrix to hold bootstrapped sample
    grp_samp_temp <- matrix(NA, nrow=nrow(grp_specdata1.4), ncol=ncol(grp_specdata1.4))
    
    # loop to sample from each interval
    for (j in 1:ncol(grp_specdata1.4)) {
      
      # get observed values from interval i
      temp <- grp_specdata1.4[!is.na(grp_specdata1.4[,j]),j]
      
      #if there are no observed values in the column
      if(length(temp)==0) {
        
        # put NA into temp2 to avoid an empty vector
        temp2 <- NA
        
        # if there are observed values
      } else {
        
        # sample n observed values with replacement
        temp2 <- sample(temp, replace=TRUE)
        
      }
      
      # put the sample into the sample matrix
      grp_samp_temp[(1:length(temp2)),j] <- temp2
      
    }
    
    # if there are no weights
    if (is.na(weights)) {
      
      # take the mean of the natural log of the species lambdas
      grp_samp_temp1.4 <- colMeans(grp_samp_temp, na.rm=TRUE)
      
    }
    
    # if there are weights...
    # convert NaN values to NA
    grp_samp_temp1.4[is.nan(grp_samp_temp1.4)] <- NA
    
    # if savedata flag is set to TRUE
    if (savedata==TRUE) {
      
      # add bootstrapped sample to list
      grp_samp_list[[i]] <- grp_samp_temp1.4
      
      # if savedata flag is set to FALSE
    } else {
      
      # back convert from log10
      grp_samp_temp1.5 <- 10^(grp_samp_temp1.4)
      
      # if these were lambda values
      if (!(tail(m_colnames, 1) %in% colnames(grp_specdata))) {
        
        # convert to index values
        grp_samp_temp1.7 <- append(grp_samp_temp1.5, 100, after = 0)
        grp_samp_temp1.8 <- cumprod(grp_samp_temp1.7)
        
        # if they were not lambda values
      } else {
        
        # set each index to start at 100
        grp_samp_temp1.8 <- (grp_samp_temp1.5 / grp_samp_temp1.5[1] * 100)
        
      }
      
      # add bootstrapped sample to list
      grp_samp_list[[i]] <- grp_samp_temp1.8
      
    }
    
  }
  
  # convert list to a data frame
  grp_samp <- do.call(rbind, grp_samp_list)
  
  # if savedata flag is set to TRUE
  if (savedata==TRUE) {
    
    # add years as column names
    colnames(grp_samp) <- m_colnames
    
    # restructure as a data frame
    grp_samp <- as.data.frame(grp_samp)
    
    # rename as confidence interval list
    grp_ci.list <- grp_samp
    
    # exit function
    return(grp_ci.list)
    
  }
  
  # if savedata is FALSE...
  
  # create matrix to hold confidence intervals
  grp_samp_ci <- matrix(NA, nrow=2, ncol=ncol(grp_samp))
  
  # loop to get confidence intervals for each year
  for (i in 1:ncol(grp_samp)) {
    
    # order the index values for year i
    temp <- sort(as.vector(grp_samp[,i]))
    
    # get the lower bound for year i
    lower_bound <- quantile(temp, 0.025, names=FALSE)
    
    # get the upper bound for year i
    upper_bound <- quantile(temp, 0.975, names=FALSE)
    
    # put the lower bounding confidence interval into the matrix
    grp_samp_ci[1,i] <- lower_bound
    
    # put the upper bounding confidence interval into the matrix
    grp_samp_ci[2,i] <- upper_bound
    
  }
  
  # add years as column names
  colnames(grp_samp_ci) <- m_colnames
  
  # restructure as a data frame
  grp_samp_ci <- as.data.frame(grp_samp_ci)
  
  # rename as confidence interval list
  grp_ci.list <- grp_samp_ci
  
  # print information to show that the msi has been completed
  print(paste("completed confidence intervals for msi"))
  
  # exit function
  return(grp_ci.list)
  
}

# get confidence intervals for resampling method
ci_resample <- function(index, m_colnames) {
  
  # load GET package
  library(GET)
  
  # check whether data is in list format
  if (inherits(index, "list")) {
    
    # if it is, convert the list to a data frame
    index.matrix <- do.call(rbind, index)
    
  } else {
    
    # if it is not a list, copy the data frame directly
    index.matrix <- index
    
  }
  
  # replace NAs with 0
  index.matrix[is.na(index.matrix)] <- 0
  
  # check if these are lambda values
  # if they are, there will be one less column than in m_colnames
  # so we test if the final element of m_colnames is missing from the column names
  if (!(tail(m_colnames, 1) %in% colnames(index.matrix))) {
    
    # if they are lambda values, back convert from log10
    index.matrix1.1 <- 10^(index.matrix)
    
    # create a single-column matrix of 100s for conversion to index values
    first_col <- matrix(100, nrow = n_boot, ncol = 1)
    
    # convert to index values
    index.matrix1.2 <- cbind(first_col, index.matrix1.1)
    index.matrix2 <- rowCumprods(as.matrix(index.matrix1.2[,1:length(m_colnames)]))
  
  } else {

    # if they are not lambda values, remove species id column
    index.matrix2 <- index.matrix[,1:length(m_colnames)]
  }
  
  # create matrix to hold confidence intervals
  grp_ci <- matrix(NA, nrow=2, ncol=ncol(index.matrix2))
  
  # select the whole dataset, omitting NAs
  grp.data <- na.omit(index.matrix2)

  # calculate final msi from bootstraps
  grp.final <- colMeans(grp.data, na.rm = TRUE)
  
  ## rank envelope method
  
  # convert msi into a vector
  grp.final.vec <- as.vector(grp.final)
  
  # transpose matrix of msi bootstraps
  grp.data.t <- t(grp.data)
  
  # create a list for the create_curve_set function
  c1 <- list(as.numeric(m_colnames), grp.final.vec, grp.data.t)
  
  # name the list elements appropriately for the function  
  names(c1) <- c("r", "obs", "sim_m")
  
  # create curve set for the rank envelope function
  c2 <- create_curve_set(c1)
  
  # create confidence intervals from the curve set
  res <- rank_envelope(c2)
  
  # extract the lower bounding confidence interval
  grp_ci[1,] <- res$lo
  
  # extract the upper bounding confidence interval
  grp_ci[2,] <- res$hi
  
  # restructure as data frame for export
  grp_ci <- as.data.frame(grp_ci)
  
  # restore column names
  colnames(grp_ci) <- m_colnames
  
  # rename as confidence interval list
  grp_ci.list <- grp_ci
  
  # print information to show that the msi has been completed
  print(paste("completed confidence intervals for msi"))
  
  # export the confidence intervals
  return(grp_ci.list)
  
}
