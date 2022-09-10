group_index_fn2 <- function(grp_index.list, c, m_colnames, n=NA, n_boot=NA, weights=NA, stay_lambda=FALSE) {
  
  # check if a value is provided for n
  if (!is.na(n)) {
    
    # check if the number of rows for each group index is equal to n_boot
    if (nrow(grp_index.list[[1]]) == n_boot) {
      
      # if so, set the resample flag to TRUE
      resample<-TRUE
      
    } else {
      
      # otherwise something is wrong, so stop with an error message
      stop("function parameters are not set correctly")
      
    }
    
  } else {
    
    # if no value is provided for n, set the resample flag to FALSE
    resample<-FALSE
    
  }
  
  # check whether data is in list format
  if (inherits(grp_index.list, "list")) {
    
    # if so, convert group index list to data frame
    grp_index <- do.call(rbind, grp_index.list)
    
  } else {
    
    # otherwise, copy group index directly
    grp_index <- grp_index.list
    
  }
  
  # check whether the final column from the original dataset is missing
  # if so, turn the lambda FLAG on. As lambdas are year-to-year change values,
  # there should one fewer lambda value than the number of observation years
  if (!(tail(m_colnames, 1) %in% colnames(grp_index))) {
    
    lambda<-TRUE
    
    # check whether there are weightings provided
    # if so, turn the lambda flag on
  } else if (!is.na(weights)) {
    
    lambda<-TRUE 
    
  } else {
    
    lambda<-FALSE
    
  }
  
  if (nrow(grp_index)==1 & !any(!is.na(grp_index))) {
    
    # if in lambda format...
    if (!(tail(m_colnames, 1) %in% colnames(grp_index))) {
      
      # remove id tags
      grp_index1.1 <- grp_index[,1:(c-1)]
      
      # if NOT tagged to stay in lambda format
      if (stay_lambda==FALSE) {
        
        # convert to index values
        grp_index1.5 <- append(grp_index1.1, 100, after = 0)
        grp_index2 <- cumprod(grp_index1.5)
        
        # if tagged to stay in lambda format
      } else if (stay_lambda==TRUE) {
        
        # update variable name
        grp_index2 <- grp_index1.1
        
      }
      
      
      # if tagged to stay in lambda format
    } else if (stay_lambda==TRUE) {
      
      # remove id tags
      grp_index2 <- grp_index[,1:(c-1)]
      
    } else {
      
      # remove id tags
      grp_index2 <- grp_index[,1:c]
      
    }
    
    print(paste("completed msi"))
    
    grp.list <- grp_index2
    
    return(grp.list)
    
  }
  
  if (lambda==TRUE & resample==TRUE) {
    
    # if the lambda flag and the resample flag are both set
    # create a single-column matrix of 100s for later conversion to index values
    first_col <- matrix(100, nrow = n_boot, ncol = 1)
    
  }
  
  if (resample==TRUE) {
    
    ### bootstrapping the species index variants, with replacement ###
    
    # make a vector of all unique species IDs
    spec_ids <- unique(grp_index$SpecID)
    
    # create a counter to keep track of the number of species sampled
    p_counter <- 1
    
    # create a vector to hold row numbers
    ms_grpdata <- data.frame()

    for (spec in spec_ids) {
      
      # select all resampled species indices that belong to a particular population
      specdata <- subset(grp_index, SpecID==spec)
      
      # randomly sample row numbers from all resampled species indices x times, where x is specified by n_boot
      sample_nums <- sample(1:nrow(specdata), n_boot, replace = TRUE)
      
      # get data from specdata by matching to row numbers in sample_nums
      ms_grpdata[(1+(p_counter*n_boot)-n_boot):(p_counter*n_boot),1:ncol(specdata)] <- specdata[match(sample_nums, row(specdata)),]
      
      # advance the population counter
      p_counter <- p_counter + 1
    }
    
  } else {
    
    # otherwise, copy all data
    ms_grpdata <- grp_index
    
  }

  
  if (lambda==TRUE) {
    
    # remove ID tags
    ms_grpdata1.4 <- ms_grpdata[,1:(c-1)]
    
  } else {
    
    # remove ID tags
    ms_grpdata1.1 <- ms_grpdata[,1:c]
    
    # convert to log10
    ms_grpdata1.4 <- log10(ms_grpdata1.1)
    
  }
  
  
  if (resample==FALSE) {
    
    # if weights are provided
    if (!is.na(weights)) {
      
      # create a list to hold weighted values
      w_grpdata <- list()
      
      # apply weighting to the group lambdas
      for (i in 1:length(weights)) {
        
        temp <- ms_grpdata1.4[i,] * weights[i]
        
        w_grpdata[[i]] <- temp
        
      }
      
      # convert to data frame
      ms_grpdata1.4 <- as.data.frame(do.call(rbind, w_grpdata))
      
      # take the sum of the group lambdas
      ms_grpdata1.5 <- colSums(ms_grpdata1.4, na.rm=TRUE)
      
    } else {
      
      if (lambda==TRUE) {
        
        # take the mean of the group lambdas
        ms_grpdata1.5 <- colMeans(ms_grpdata1.4, na.rm=TRUE)
        
      } else {
        
        # take the mean of the group lambdas
        ms_grpdata1.5 <- colMeans(ms_grpdata1.4, na.rm=TRUE)
        
      }
      
    }
    
    if (lambda==TRUE & stay_lambda==FALSE) {
      
      # get rid of NANs
      ms_grpdata1.5[is.nan(ms_grpdata1.5)] <- 0
      
      # back convert from log10
      ms_grpdata1.6 <- 10^(ms_grpdata1.5)
      
      # convert to index values
      ms_grpdata1.7 <- append(ms_grpdata1.6, 100, after = 0)
      ms_grpdata1.8 <- cumprod(ms_grpdata1.7)
      
    } else if (lambda==FALSE & stay_lambda==FALSE) {
      
      # back convert from log10
      ms_grpdata1.6 <- 10^(ms_grpdata1.5)
      
      # ensure index starts at 100
      ms_grpdata1.8 <- (ms_grpdata1.6 / ms_grpdata1.6[1] * 100)
      
    } else if (lambda==TRUE & stay_lambda==TRUE) {
      
      # update variable name
      ms_grpdata1.8 <- ms_grpdata1.5
      
    } else if (lambda==FALSE & stay_lambda==TRUE) {
      
      # update variable name
      ms_grpdata1.8 <- ms_grpdata1.5[1:(length(ms_grpdata1.5)-1)]
      
    }
    
    # transpose group index as a matrix
    ms_grpdata2.1 <- t(as.matrix(ms_grpdata1.8))
    
    # convert NaN values to NA
    ms_grpdata2.1[is.nan(ms_grpdata2.1)] <- NA
    
    if (stay_lambda==FALSE) {
      
      # add column names
      colnames(ms_grpdata2.1) <- m_colnames
      
    } else {
      
      if (any(!is.na(ms_grpdata2.1))) {
        
        # add column names
        colnames(ms_grpdata2.1) <- m_colnames[1:(length(m_colnames)-1)]
        
      }
      
    }
    
    # restructure as data frame
    ms_grpdata2.1 <- as.data.frame(ms_grpdata2.1)
    
    print(paste("completed msi"))
    
    grp.list <- ms_grpdata2.1
    
  } else if (resample==TRUE) {
    
    # transpose data and convert to vector
    ms_grpdata1.5 <- as.vector(t(ms_grpdata1.4))
    
    # create a new matrix in wide format, so column means can be used to create group indices
    ms_grpdata1.6 <- matrix(ms_grpdata1.5, nrow=length(unique(grp_index$SpecID)), 
                            ncol=(n_boot*ncol(ms_grpdata1.4)), byrow=TRUE)
    
    # take the column means. the .colMeans function requires extra information but is very fast
    ms_grpdata1.7 <- .colMeans(ms_grpdata1.6, nrow(ms_grpdata1.6), ncol(ms_grpdata1.6), na.rm=TRUE)
    
    # convert back to long format, with a species index in each row
    ms_grpdata1.8 <- matrix(ms_grpdata1.7, nrow = n_boot, ncol = ncol(ms_grpdata1.4), byrow=TRUE)
    
    # convert NaN values to 0
    ms_grpdata1.8[is.nan(ms_grpdata1.8)] <- 0
    
    if (lambda==TRUE & stay_lambda==FALSE) {
      
      # back convert from log10
      ms_grpdata1.9 <- 10^(ms_grpdata1.8)
      
      # convert to index values
      ms_grpdata2 <- cbind(first_col, ms_grpdata1.9)
      ms_grpdata2.1 <- rowCumprods(ms_grpdata2)
      
    } else if (lambda==FALSE & stay_lambda==FALSE) {
      
      # back convert from log10
      ms_grpdata1.9 <- 10^(ms_grpdata1.8)
      
      # set each index to start at 100
      ms_grpdata2.1 <- (ms_grpdata1.9 / ms_grpdata1.9[,1] * 100)
      
    } else if (lambda==TRUE & stay_lambda==TRUE) {
      
      # update variable name
      ms_grpdata2.1 <- ms_grpdata1.8
      
    } else if (lambda==FALSE & stay_lambda==TRUE) {
      
      # update variable name
      ms_grpdata2.1 <- ms_grpdata1.8[,(1:length(ms_grpdata1.8)-1)]
      
    }
    
    # convert NaN values to NA
    ms_grpdata2.1[is.nan(ms_grpdata2.1)] <- NA
    
    # convert to a data frame
    ms_grpdata2.1 <- as.data.frame(ms_grpdata2.1)
    
    if (stay_lambda==FALSE) {
      
      
      # add years as column names
      colnames(ms_grpdata2.1) <- m_colnames
      
    } else {
      
      if (any(!is.na(ms_grpdata2.1))) {
        
        # add years as column names
        colnames(ms_grpdata2.1) <- m_colnames[1:(length(m_colnames)-1)]
        
      }
      
    }
    
    print(paste("completed msi"))
    
    grp.list <- ms_grpdata2.1
    
    
  }
  
  return(grp.list)
  
}
