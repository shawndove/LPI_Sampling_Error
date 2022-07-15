### functions ###

# remove time series which have fewer than the number of counts specified in 'count_thres' and which are shorter than
# the length specified in 'min_ts_length'
cull_fn <- function(grp_data, count_thres, min_ts_length, c) {
  
  # create a vector to hold row names of time series that meet the thresholds
  notna <- vector()
  
  # set counter to record number of time series that meet the thresholds
  counter <- 1
  
  # loop over each row
  for (i in 1:nrow(grp_data)) {
    
    # check whether the number of observations meets the threshold
    if (length(which(!is.na(grp_data[i,1:c]))) >= count_thres) {
      
      # check whether the length of the time series (from first to last observation) meets the threshold
      if ((max(as.numeric(colnames(grp_data[i,1:c][which(!is.na(grp_data[i,1:c]))]))) - 
           min(as.numeric(colnames(grp_data[i,1:c][which(!is.na(grp_data[i,1:c]))])))) >= (min_ts_length - 1)) {
        
        # if the time series passes both tests, add its row number to the vector notna
        notna[counter] <- rownames(grp_data[i,1:c])
        
        # increase the counter
        counter <- counter + 1
        
      }
      
    }
    
  }
  
  # create a new data frame, including only time series that meet the thresholds
  grp_data_culled <- grp_data[rownames(grp_data) %in% notna,]
  
  return(grp_data_culled)
  
}


## function to interpolate time series
complete_time_series <- function(grp_data_culled, c, m_colnames, sample_pop_ids=NA, lambda=TRUE, gam_all=FALSE, calcsd=FALSE) {
  
  # set forecast to TRUE if you want to project the time series backwards and forewards
  
  if (!is.na(sample_pop_ids)) {
    
    # select all copies of the populations listed in the sample
    grp_data_culled <- grp_data_culled[grp_data_culled$PopID %in% sample_pop_ids,]
    
  }
  
  # check for GAM quality status column
  if ("gqfail" %in% colnames(grp_data_culled)) {
    
    POSTGAM <- TRUE
    
  } else {
    
    POSTGAM <- FALSE
    
  }
  
  if(nrow(grp_data_culled)==0) {
    
    return(grp_data_culled)
    
  }
  
  ## INTERPOLATION
    
  # create a matrix to hold the interpolated time series
  new.grp_data <- as.data.frame(matrix(NA, nrow = nrow(grp_data_culled[,1:c]), ncol = ncol(grp_data_culled[,1:c])))
  
  # create a vector to hold the rows
  rowsmat <- vector()
  
  # create a vector to record whether time series are copied or interpolated
  copied <- vector()
  
  # create a counter to track how many populations have been interpolated
  counter1 <- 1

  # begin the interpolation loop
  for (i in 1:nrow(grp_data_culled)) {
    
    # put a single time series (row) into a vector
    new_pop_counts <- as.matrix(grp_data_culled[i,m_colnames])
    
    # put the row name of the time series into a vector
    rownum <- rownames(grp_data_culled[i,])
    
    # check if GAMs have already been performed
    if (POSTGAM==TRUE) {
      
      # if so, check if this population passed quality check
      if (grp_data_culled$gqfail[i]==0) {
        
        # copy the time series into the new matrix
        new.grp_data[i,] <- new_pop_counts
        
        # put the row number of the time series into the row numbers vector
        rowsmat[i] <- rownum
        
        print(paste("copied population ", counter1, sep=""))
        
        # increase the counter
        counter1 <- counter1 + 1
        
        next
        
      }
      
    }
    
    # check if there are any zeros
    if (length(which(new_pop_counts==0))>0 & calcsd==FALSE) {
      
      # check if all non-NA observations are zero
      if (mean(new_pop_counts[which(!is.na(new_pop_counts))])==0) {
        
        # if so, set zero adjust to a very small value
        zero_adjust <- 1e-17
        
      } else {
        
        # otherwise, calculate 1% of the mean of the observed values (excluding zeros)
        # if there are any zeros, this will be added to every observation to avoid issues with log of zero
        zero_adjust <- 0.01 * mean(new_pop_counts[which(new_pop_counts>0)], na.rm=TRUE)
        
      }
      
      # add the zero_adjust value
      new_pop_counts = new_pop_counts + zero_adjust
      
    }
    
    # get the missing counts from the time series
    pop_count_blanks <- new_pop_counts[,which(is.na(new_pop_counts))]
    
    # the lines below are a fix for a problem that occurs when there is only a single NA value
    new_pop_count_names <- as.integer(colnames(new_pop_counts))
    names(pop_count_blanks) <- new_pop_count_names[which(new_pop_counts %in% pop_count_blanks)]
    
    # if there are no missing counts, copy the time series directly and move on
    if (length(pop_count_blanks) == 0) {
      
      # copy the time series into the new matrix
      new.grp_data[i,] <- new_pop_counts
      
      # put the row number of the time series into the row numbers vector
      rowsmat[i] <- rownum
      
      # put 1 into the copied vector to record that this time series was not interpolated
      copied[i] <- 1
      
      print(paste("copied population ", counter1, sep=""))
      
      # increase the counter
      counter1 <- counter1 + 1
      
      next
      
    }
    
    # if there are 6 or more non-missing counts...
    if (length(new_pop_counts[,which(!is.na(new_pop_counts))]) >= 6) {
      
      # check if GAMs have already been performed
      if (POSTGAM==FALSE) {
        
        # if not, copy the time series into the new matrix
        new.grp_data[i,] <- new_pop_counts
        
        # put the row number of the time series into the row numbers vector
        rowsmat[i] <- rownum
        
        # put 1 into the copied vector to record that this time series was not interpolated
        copied[i] <- 1
        
        print(paste("copied population ", counter1, sep=""))
        
        # increase the counter
        counter1 <- counter1 + 1
        
        next
        
      }
      
    }
    
    # get the non-missing counts
    pop_count_filled <- new_pop_counts[,which(!is.na(new_pop_counts))]
    
    # get the column numbers of the missing counts
    pop_count_blank_cols <- which(new_pop_counts %in% pop_count_blanks)
    
    # get the column numbers of the non-missing counts
    pop_count_filled_cols <- which(new_pop_counts %in% pop_count_filled)
    
    # get the years of the missing counts
    pop_count_blank_years <- as.integer(names(pop_count_blanks))
    
    # get the years of the non-missing counts
    pop_count_filled_years <- as.integer(names(pop_count_filled))
    
    # get the year of the first non-missing count
    first_filled_year <- min(pop_count_filled_years)
    
    # get the year of the last non-missing count
    last_filled_year <- max(pop_count_filled_years)
    
    # get the column number of the first non-missing count
    first_filled_col <- min(pop_count_filled_cols)
    
    # get the column number of the last non-missing count
    last_filled_col <- max(pop_count_filled_cols)
    
    # make a vector of all columns between and including the first to the last non-missing counts of the time series
    # this will be used to determine years which must be interpolated. Projection will be done later.
    actual_ts_cols <- first_filled_col:last_filled_col
    
    # make a vector of years that match the columns
    actual_ts_years <- first_filled_year:last_filled_year
    
    # make a vector of columns to be interpolated by matching the above columns vector with the non-missing counts
    actual_ts_blank_cols <- pop_count_blank_cols[pop_count_blank_cols %in% actual_ts_cols]
    
    # get the years of the missing counts within the time series period
    actual_ts_blank_years <- actual_ts_years[actual_ts_years %in% pop_count_blank_years]
    
    # get column numbers of existing counts within the time series period
    actual_ts_filled_cols <- actual_ts_cols[actual_ts_cols %in% pop_count_filled_cols]
    
    # get existing counts for use in interpolation
    actual_ts_filled_vals <- new_pop_counts[,actual_ts_filled_cols]
    
    # get the years of the non-missing counts within the period of the time series
    actual_ts_filled_years <- actual_ts_years[actual_ts_years %in% pop_count_filled_years]
    
    #interpolate to fill in missing values within the time series period (not projected), using log-linear interpolation
    pop_interp_temp <- approx(actual_ts_filled_years, log(actual_ts_filled_vals), actual_ts_blank_years)
    
    # back convert from log
    pop_interp <- exp(pop_interp_temp$y)
    
    # create a counter for use when adding the interpolated values into the time series
    counter2 <- 1
    
    # loop for adding the interpolated values into the time series
    for (j in actual_ts_blank_cols) {
      
      # add interpolated values. counter is used for pop_interp because it has fewer values than new_pop_counts
      new_pop_counts[,j] <- pop_interp[counter2]
      
      # increase the counter each time a value is added to new_pop_counts
      counter2 <- counter2 + 1
      
    }
    
    # put the interpolated time series into the new matrix
    new.grp_data[i,] <- as.matrix(new_pop_counts)
    
    # put the row number of the time series into the row numbers vector
    rowsmat[i] <- rownum
    
    # put 0 into the copied vector to record that this time series was interpolated
    copied[i] <- 0
    
    print(paste("completed interpolation of population ", counter1, sep=""))
    
    counter1 <- counter1 + 1
    
  }
  
  if(length(grp_data_culled)>c) {
    
    # add id tags back
    new.grp_data[,(c+1):length(grp_data_culled)] <- grp_data_culled[,(c+1):length(grp_data_culled)]
    
  }

  # put column names into the new matrix
  colnames(new.grp_data) <- colnames(grp_data_culled)
  
  # put row names into the new matrix
  rownames(new.grp_data) <- rowsmat

  # check if GAMs have already been performed
  if (POSTGAM==FALSE) {
    
    # if not, add "copied" vector as a column to show the gam function which time series to ignore
    new.grp_data$copied <- copied
    
  }

  return(new.grp_data)

}
  

pop_gam_fn <- function(new.grp_data, c, m_colnames, n=NA, lambda=FALSE, resample=FALSE, forecast=FALSE, quality=FALSE) {
  
  # create a list to put resampled populations into
  gam_poplist <- list()
  
  # create a vector of population IDs
  pop_ids <- new.grp_data$PopID
  
  # create a vector of row numbers
  rows <- 1:nrow(new.grp_data)
  
  # create vector to record populations which fail GAM quality check
  gqfail <- vector()
  
  # reorganize the data into a long format that works with the population resampling code
  trim.mat <- matrix(NA, nrow = nrow(new.grp_data) * c, ncol = 3)
  
  # name columns
  colnames(trim.mat) <- c("population", "year", "count")
  
  # convert to data frame
  trim.mat <- as.data.frame(trim.mat)
  
  # fill columns with data
  trim.mat[,1] <- as.factor(rep(pop_ids, each = c)) # population ID
  
  trim.mat[,2] <- as.numeric(rep(m_colnames, times = nrow(new.grp_data))) # year
  
  trim.mat[,3] <- as.numeric(as.vector(t(new.grp_data[m_colnames])))  # count
  
  # if resampling flag is turned off...
  if (resample==FALSE) {
    
    # create copied vector to check which populations have already been interpolated
    copied <- new.grp_data$copied
    
    # create vector of rows which have been interpolated
    copied_rows <- which(copied[rows]==0)
    
    # create vector of rows which have not been interpolated
    gam_rows <- which(copied[rows]==1)
    
    for (row in copied_rows) {
      
      # copy population into completed list, without the "copied" column
      gam_poplist[[row]] <- new.grp_data[row,1:c]
      
      # flag the GAM quality status as pass (because pop was not GAM'd)
      gqfail[row] <- 0
      
      print(paste("copied population ", row, sep=""))
      
    } 
    
  } else {
    
    # otherwise, GAM everything
    gam_rows <- rows
    
  }
  

  for (row in gam_rows) {
    # Get pop data
    pop_data = subset(trim.mat, population == pop_ids[row])
    
    # check if there are any zeros
    if (length(which(pop_data$count==0))>0) {
      
      # check if all non-NA observations are zero
      if (mean(pop_data$count[which(!is.na(pop_data$count))])==0) {

        # if so, set zero adjust to a very small value
        zero_adjust <- 1e-17
        
      } else {
        
        # otherwise, calculate 1% of the mean of the observed values (excluding zeros)
        # if there are any zeros, this will be added to every observation to avoid issues with log of zero
        zero_adjust <- 0.01 * mean(pop_data$count[which(pop_data$count>0)], na.rm=TRUE)
        
      }
      
      # add the zero_adjust value
      pop_data$log_popvalue = log(pop_data$count + zero_adjust)
      
    } else {
      
      # add log column
      pop_data$log_popvalue = log(pop_data$count)
      
    }
    
    # K is half of the number of non-NA values in pop_data$count (if it is an odd number, K will be rounded down)
    K = round(length(which(!is.na(pop_data$count)))/2)

    # Make GAM
    #b <- gam(log_popvalue~s(year,k = K, bs = "gp"),data=pop_data)
    b <- gam(log_popvalue ~ s(year, k = K), data = pop_data)
    
    # if we are reproducing the LPI method...
    if (quality==TRUE) {
      
      # check the model fit
      # first, get the residuals of the GAM model
      resid <- residuals(b)
      
      # change K to the full number of non-NA values in pop_data$count 
      K = length(which(!is.na(pop_data$count)))

      # set years for residuals
      resid.years <- pop_data$year[which(!is.na(pop_data$count))]
      
      # then GAM the residuals (using same GAM settings as LPI)
      resid.gam <- gam(resid ~ s(resid.years, k = K, bs = "cs"), gamma = 1.4)
      
      # finally, check whether the sum of the estimated degrees of freedom is close to 1
      if ((abs(sum(resid.gam$edf) - 1)) < 0.01) {
        
        # flag the GAM quality status as a pass
        gqfail[row] <- 0
        
      } else {
        
        # copy the original data for using the chain method later
        gam_poplist[[row]] <- new.grp_data[row,1:c]
        
        # flag the GAM quality status as a fail
        gqfail[row] <- 1
        
        print(paste0("GAM of population ", row, " failed quality check.", sep=""))
        
        next
        
      }
      
      
    }
    
    # create matrix to hold GAM'd population
    pred.a <- matrix(NA, nrow=1, ncol=c)
      
    # add column names
    colnames(pred.a) = paste(m_colnames)
    
    if (resample==TRUE) {
      
      if (lambda==TRUE) {
        
        # predict all values between the first and last missing values using GAM
        # in this case we are interpolating using the GAM, as in the LPI
        startGAM <- min(which(!is.na(pop_data$count)))
        
        endGAM <- max(which(!is.na(pop_data$count)))
        
        # map coefs to fitted curves
        Xp <- predict(b, pop_data[startGAM:endGAM,], type="lpmatrix")
        
      } else if (lambda==FALSE) {
        
        # map coefs to fitted curves
        Xp <- predict(b, pop_data, type="lpmatrix")
        
      }
      
      # posterior mean and cov of coefs - (capture error of the model)
      beta <- coef(b);Vb <- vcov(b)
      
      # Samples from a multivariate normal distribution, where beta is the means of the variables (coefficients)
      # and Vb is the variance-covariance matrix of the coefficients
      br <- MASS::mvrnorm(n,beta,Vb) ## simulate n rep coef vectors from post.
      
      # convert to data frame
      pred.a <- as.data.frame(pred.a)
      
      # loop to get trough to peak diff for each sim
      for (i in 1:n) {
      
        if (lambda==TRUE) {
        
          # curve for this replicate
          pred.a[i,startGAM:endGAM] <- as.data.frame(t(Xp%*%br[i,]))
          
        } else if (lambda==FALSE) {
        
          # curve for this replicate
          pred.a[i,] <- as.data.frame(t(Xp%*%br[i,]))
          
        }
        
      }
      
    } else if (resample==FALSE) {
      
      if (lambda==TRUE) {
        
        # predict non-missing values using GAM
        # this is only to be used after log-linear interpolation has been done
        #pred.a[,which(!is.na(pop_data$count))] <- t(predict(b, pop_data[which(!is.na(pop_data$count)),]))
        
        # predict all values between the first and last missing values using GAM
        # in this case we are interpolating using the GAM, as in the LPI
        startGAM <- min(which(!is.na(pop_data$count)))
        
        endGAM <- max(which(!is.na(pop_data$count)))
        
        # predict the missing values from the GAM
        pred.a[,startGAM:endGAM] <- t(predict(b, pop_data[startGAM:endGAM,]))
        
      } else if (lambda==FALSE & forecast==TRUE) {
        
        # predict all values between the first and last missing values using GAM
        # in this case we are interpolating using the GAM, as in the LPI
        startGAM <- min(which(is.na(pop_data$count)))-1
        
        endGAM <- max(which(is.na(pop_data$count)))+1
        
        pred.a <- log(new.grp_data[row,1:c])
        
        # predict the missing values from the GAM
        pred.a[,startGAM:endGAM] <- t(predict(b, pop_data[startGAM:endGAM,]))
        
      } else if (lambda==FALSE & forecast==FALSE) {
        
        # if not using lambda method, GAM the whole time series
        pred.a <- t(predict(b, pop_data))
      
      }
      
    }
    
    # convert to matrix
    pred.a <- as.matrix(pred.a)
    
    # convert back to index values
    pred.a <- exp(pred.a)
    
    # convert any negative values to 0s
    pred.a <- ifelse(pred.a < 0, 0, pred.a)
    
    # convert to data frame
    pred.a <- as.data.frame(pred.a)
    
    # add column names
    colnames(pred.a) <- colnames(new.grp_data[,m_colnames])
    
    if (resample==TRUE) {
      
      # add resample ID
      #pred.a$ResID <- 1:nrow(pred.a)
      
      # add other IDs back in
      pred.a[,(c+1):(length(new.grp_data))] <- new.grp_data[row,(c+1):(length(new.grp_data))]
      
    }
    
    # add GAM'd population to list
    gam_poplist[[row]] <- pred.a
    
    print(paste("completed GAM of population ", row, sep=""))
    
  }
  
  # convert from list to data frame
  gam_popmat <- do.call(rbind, gam_poplist)
  
  if (is.null(gam_popmat)) {
    
    gam_popmat <- as.data.frame(matrix(NA, nrow=1, ncol=(ncol(new.grp_data)-1)))
    
    gqfail <- 0
    
  }
  
  if (resample==FALSE) {
    
    # add extra columns back in from the original data frame
    gam_popmat[,(c+1):(length(new.grp_data)-1)] <- new.grp_data[,(c+1):(length(new.grp_data)-1)]
    
    # add column names back in
    colnames(gam_popmat) <- colnames(new.grp_data[,1:(length(new.grp_data)-1)])
    
  } else {
    
    # add column names back in
    colnames(gam_popmat) <- colnames(new.grp_data[,1:(length(new.grp_data))])
    
  }
  

  
  # if we are reproducing the LPI method...
  if (quality==TRUE) {
    
    # add the GAM quality fail status to the data frame
    gam_popmat$gqfail <- gqfail
    
  }

  return(gam_popmat)
  
}


# find the average width of the confidence intervals
ci_width_fn <- function(msi_ci, c) {
  
  # subtract lower c.i. from upper c.i. for all time points and take the mean
  width <- mean(as.matrix(msi_ci[2,2:c] - msi_ci[1,2:c]), na.rm=TRUE)
  
  if (is.nan(width)) {
    
    width <- NA
    
  }
  
  return(width)
  
}


# test how much of the real trend is within the confidence intervals of the sampled trend
real_trend_within_ci_fn <- function(msi_ci, true) {
  
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


# function to randomly remove observations from synthetic dataset
remove_vals_fn <- function(all_pops_index, c, rmin=0.65, rmax=0.95) {
  
  # minimum number of observations to remove
  a <- round(c * rmin)
  
  # maximum number of observations to remove
  b <- round(c * rmax)
  
  # sequence with all possible numbers of observations that can be removed
  num <- seq(a,b)
  
  # loop for each population
  for (i in 1:nrow(all_pops_index)) {
    
    # sample from the sequence to choose how many observations to remove
    d <- sample(num, 1)
    
    # replace d randomly chosen observations with NA
    all_pops_index[i,][sample(ncol(all_pops_index[1:(ncol(all_pops_index)-3)]), d)] <- NA 
    
  }
  
  return(all_pops_index)
  
}

# function to shorten and degrade time series
degrade_ts_fn <- function(all_pops_index, c, mlength=10, numobs=5) {
  
  # create a Poisson distribution around the mean length of time series
  length_dist <- rpois(1000, mlength)
  
  # create a Poisson distribution around the mean number of observations of t.s.
  numobs_dist <- rpois(1000, numobs)
  
  # loop through time series
  for (i in 1:nrow(all_pops_index)) {
    
    # randomly set length of time series from Poisson distribution
    # restricted between 3 and the starting length of the t.s.
    length_ts <- sample(length_dist[(length_dist>=3) & (length_dist<=c)], 1)
    
    # randomly set number of observations of t.s. from Poisson distribution
    # restricted between 3 and the new mean length of t.s.
    numobs_ts <- sample(numobs_dist[(numobs_dist>=3) & (numobs_dist<=length_ts)], 1)
    
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

# calculate species mean growth rates
growth_rate_calc_fn3 <- function(merged.matrix, c2=50, model=FALSE) {
  
  # create empty vectors to store max, min, and mean growth rates
  maxr2 <- vector()
  geomeanr2 <- vector()
  armeanr2 <- vector()
  minr2 <- vector()
  sd.mr2 <- vector()
  specgr <- vector()
  specsd <- vector()
  gratelist2 <- list()
  popgeogrlist2 <- list()
  popargrlist2 <- list()
  popsdlist2 <- list()
  
  counter <- 1

  # loop to get max and mean growth rates
  for (i in unique(merged.matrix$SpecID)) {
    
    # create empty vectors to store max, min, and mean growth rates
    maxr <- vector()
    geomeanr <- vector()
    armeanr <- vector()
    minr <- vector()
    sd.mr <- vector()
    gratelist <- list()

    tempa <- merged.matrix[merged.matrix$SpecID==i,1:c2] # get non-na values
    
    for (j in 1:nrow(tempa)) {
      
      temp <- tempa[j,]
      
      if (model==TRUE) {
        
        temp <- temp[!is.na(temp)] # exclude NAs
        
        temp <- temp[temp > 0] # exclude values of 0 or less
        
      }
      
      if (length(temp) < 2) {
        
        temp <- c(1,1,1)
        
      }
      
      # create a vector of initial values for growth rate calculation. correct for ID column at end of data.
      start.vals <- temp[1:(length(temp) - 1)]
      
      # create a vector of final values for growth rate calculation. correct for ID column at end of data.
      final.vals <- temp[2:(length(temp))]
      
      # calculate the growth rates
      g.rate <- log(final.vals / start.vals)
      
      g.rate[g.rate >= 2.302585] <- 2.302585
      
      g.rate[g.rate <= -2.302585] <- -2.302585
      
      # convert to a vector
      g.rate.vec <- as.vector(as.matrix(g.rate))
      
      # find the maximum growth rate from the given row.
      maxr[j] <- max(g.rate.vec)
      
      # find the mean growth rate from the given row (geometric mean).
      geomeanr[j] <- exp(1) ^ mean(log(g.rate.vec))
      armeanr[j] <- mean(g.rate.vec)
      
      # find the mimimum growth rate from the given row.
      minr[j] <- min(g.rate.vec)
      
      # find the standard deviation of growth rates for a given row
      sd.mr[j] <- sd(g.rate.vec)
      
      gratelist[[j]] <- g.rate.vec
      
    }
    
    maxr2[counter] <- max(maxr)
    
    # arithmetic mean of population mean growth rates for the species
    geomeanr2[counter] <- mean(geomeanr, na.rm=TRUE)
    armeanr2[counter] <- mean(armeanr, na.rm=TRUE)
    
    minr2[counter] <- min(minr)
    
    # standard deviation of population mean growth rates for the species
    sd.mr2[counter] <- sd(geomeanr, na.rm=TRUE)
    
    gratelist.temp <- unlist(gratelist)
    
    specgr[counter] <- mean(gratelist.temp)
    
    specsd[counter] <- sd(gratelist.temp)
    
    gratelist2[[counter]] <- gratelist.temp
    
    # list of population mean growth rates by species
    popgeogrlist2[[counter]] <- geomeanr[!is.na(geomeanr) & is.finite(geomeanr)]
    popargrlist2[[counter]] <- armeanr[!is.na(armeanr) & is.finite(armeanr)]
    
    # list of population growth rate standard deviations by species
    popsdlist2[[counter]] <- sd.mr[!is.na(sd.mr) & is.finite(sd.mr)]
    
    counter <- counter + 1
    
  }
  
  allgrates <- unlist(gratelist2)
  
  sd.all <- sd(allgrates)
  
  popargrates <- unlist(popargrlist2)
  popargrates <- popargrates[!is.na(popargrates) & is.finite(popargrates)]
  
  popgeogrates <- unlist(popgeogrlist2)
  popgeogrates <- popgeogrates[!is.na(popgeogrates) & is.finite(popgeogrates)]
  
  # arithmetic mean of population mean growth rates
  armeanpopargrates <- mean(popargrates)
  armeanpopgeogrates <- mean(popgeogrates)
  
  # geometric mean of population mean growth rates
  geomeanpopargrates <- exp(1) ^ mean(log(popargrates))
  geomeanpopgeogrates <- exp(1) ^ mean(log(popgeogrates))
  
  # population standard deviations of growth rates
  popsds <- unlist(popsdlist2)
  
  # arithmetic mean of population standard deviations of growth rates
  meanpopsds <- mean(popsds, na.rm=TRUE)
  # standard deviation of population standard deviations of growth rates
  sdpopsds <- sd(popsds, na.rm=TRUE)
  
  # standard deviation of population mean growth rates
  sd.geopop <- sd(popgeogrates)
  sd.arpop <- sd(popargrates)
  
  maxr2 <- maxr2[!is.na(maxr2) & is.finite(maxr2)]
  
  # arithmetic means of population geometric mean growth rates for each species
  geomeanr2 <- geomeanr2[!is.na(geomeanr2) & is.finite(geomeanr2)]
  
  minr2 <- minr2[!is.na(minr2) & is.finite(minr2)]
  
  # standard deviations of population geometric mean growth rates for each species
  #sd.mr2 <- sd.mr2[!is.na(sd.mr2) & is.finite(sd.mr2)]
  
  mean.sdmr2 <- mean(sd.mr2, na.rm=TRUE)
  
  maxr.mean <- mean(maxr2, na.rm=TRUE)
  
  maxr.sd <- sd(maxr2, na.rm=TRUE)
  
  maxr.var <- maxr.sd / sqrt(length(maxr2))
  
  maxr.max <- max(maxr2)
  
  maxr.min <- min(maxr2)
  
  maxr.geomean <- exp(1) ^ mean(log(maxr2))
  
  minr.mean <- mean(minr2, na.rm=TRUE)
  
  minr.sd <- sd(minr2, na.rm=TRUE)
  
  minr.var <- minr.sd / sqrt(length(minr2))
  
  minr.max <- max(minr2)
  
  minr.min <- min(minr2)
  
  minr.geomean <- exp(1) ^ mean(log(minr2))
  
  meanr.mean <- mean(geomeanr2)
  
  meanr.sd <- sd(geomeanr2)
  
  meanr.var <- meanr.sd / sqrt(length(geomeanr2))
  
  meanr.max <- max(geomeanr2)
  
  meanr.min <- min(geomeanr2)
  
  meanr.geomean <- exp(1) ^ mean(log(geomeanr2))
  
#  return.vals <- c(meanr.geomean, sd.all, sd.pop, meanr.sd, meanr.max, meanr.min, meanr.var, maxr.max, minr.min, meanr.mean)
  
#  names(return.vals) <- c("mean.geomean.gr", "sd.all.gr", "sd.pop.gr", "sd.mean.gr", "max.mean.gr", "min.mean.gr", "var.mean.gr", "max.max.gr", "min.min.gr", "mean.mean.gr")
 
  return.vals <- list(armeanpopargrates, sd.arpop, meanpopsds, sdpopsds, popsds, popargrlist2)#, geomeanr2, sd.mr2, popgeogrlist2, popsdlist2)
  
  names(return.vals) <- c("Mean of Population Mean Log Growth Rates",
                          "Standard Deviation of Population Mean Log Growth Rates",
                          "Mean of Population Growth Rate Standard Deviations",
                          "Standard Deviation of Population Growth Rate Standard Deviations",
                          "Population Standard Deviations of Growth Rates",
                          "Population Mean Growth Rates by Species")#,
                          #"Species Arithmetic Means of Population Geometric Mean Growth Rates",
                          #"Species Standard Deviations of Population Geometric Mean Growth Rates",
                          #"Pop Geo Mean Growth Rates by Species",
                          #"Pop Growth Rate St Dev by Species")
   
  return(return.vals)
  
}

# create a database of artificial populations
# randomized change points ensure trends are not linear
# this version runs the creation process separately for each species
pgrowth4.4 <- function(tpops, tmax, gr_mean, gr_sd_vec, popspec) {
  
  all_pops <- matrix(NA, nrow = tpops, ncol = tmax+1) # create matrix to put all the population counts
  
  all_pops[,tmax+1] <- 1:tpops # fill the final column of the matrix with the species IDs
  
  for (i in 1:nrow(all_pops)) { # loop over each species
    
    popmean <- sample(rnorm(1000, mean=gr_mean, sd=gr_sd_vec), 1)
    
      temp_pops <- rlnorm(tmax, 
                            meanlog = popmean, sdlog = gr_sd_vec) # create a growth rate distr. for this species
        
      all_pops[i,1:tmax] <- temp_pops # assign growth rates to each population in this species
      
  }
  
  for (i in 1:tpops) {
    
    ts <- cumprod(all_pops[i,1:tmax])
    
    ts <- (ts/ts[1])*100
    
    all_pops[i,1:tmax] <- ts
    
  }
  
  colnames(all_pops) <- c(1:(ncol(all_pops)-1), "SpecID")
  
  all_pops <- as.data.frame(all_pops)
  
  all_pops <- all_pops[complete.cases(all_pops),]
  
  return(all_pops)
  
}

# create a database of artificial populations
pgrowth4.5 <- function(tpops, tmax, gr_mean, gr_sd_vec, popspec, sd_mean) {
  
  all_pops <- matrix(NA, nrow = tpops, ncol = tmax+1) # create matrix to put all the population counts
  
  spec_id <- sample(1:(tpops/popspec), tpops, replace = TRUE) # assign each population to a species
  
  all_pops[,tmax+1] <- t(spec_id) # fill the final column of the matrix with the species IDs
  
  # create normal distribution of species mean growth rates
  specmeangrdist <- rnorm(length(unique(spec_id)), mean=gr_mean, sd=gr_sd_vec)
  
  # create exponential distribution of population standard deviations of growth rates
  popsddist <- rexp(nrow(all_pops), rate=1/sd_mean)

  for (i in unique(spec_id)) { # loop over each species
    
    if (is.null(nrow(all_pops[all_pops[,tmax+1]==i,]))) { # check if there are no populations assigned to this species
      
      next
      
    }
    
    # create temporary matrix to fill with species growth rates
    spec_data <- all_pops[all_pops[,tmax+1]==i,1:tmax]
    
    # randomly select species mean growth rate from normal distribution
    specmean <- sample(specmeangrdist, 1)
    
    # randomly select standard deviation of mean growth rate from a sequence between 0 and dataset st.dev
    specsd <- sample(seq(0, gr_sd_vec, by=0.01), 1)
    
    # create normal distribution of population mean growth rates
    popmeangrdist <- rnorm(nrow(spec_data), mean=specmean, sd=specsd)
    
    for (j in 1:nrow(spec_data)) { # loop over all populations in species i

      # randomly select a mean growth rate for this population from normal distribution
      popmean <- sample(popmeangrdist, 1) 
      
      # randomly select a standard deviation of growth rates for this population from exponential dist
      popsd <- sample(popsddist, 1)

      # assign growth rates to each population in this species
      spec_data[j,] <- rlnorm(tmax, meanlog=popmean, sdlog=popsd)
      
    }
    
    # put the species growth rates into the population matrix
    all_pops[all_pops[,tmax+1]==i,1:tmax] <- spec_data
    
  }
  
  # loop over each population
  for (i in 1:tpops) {
    
    # calculate cumulative product of growth rates
    ts <- cumprod(all_pops[i,1:tmax])
    
    # convert to index, starting at value of 100. year 1 is the base year
    ts <- (ts/ts[1])*100
    
    # put the index values into the matrix
    all_pops[i,1:tmax] <- ts
    
  }
  
  # name the columns
  colnames(all_pops) <- c(1:(ncol(all_pops)-1), "SpecID")
  
  # convert to data frame
  all_pops <- as.data.frame(all_pops)
  
  # remove any NA values
  all_pops <- all_pops[complete.cases(all_pops),]
  
  return(all_pops)
  
}

