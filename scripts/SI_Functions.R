species_index_fn <- function(resamp_popmat, c, n=NA, n_boot=NA, lambda=FALSE, random_pops=TRUE, resample=FALSE) {
  
  # make a vector of all unique species IDs in the sample
  spec_ids <- unique(resamp_popmat$SpecID)
  
  # create list to hold the species indices
  spec_index.list <- list()
  
  # start a counter to iterate species
  counter1 <- 1
  
  if(!any(!is.na(resamp_popmat[,1:c])) & nrow(resamp_popmat)==1) {
    
    # get species data
    sample_mat5 <- as.data.frame(resamp_popmat[,1:c])
    
    # add a species ID column
    sample_mat5$SpecID <- spec_ids[1]
    
    # add a group ID column
    sample_mat5$GrpID <- resamp_popmat$GrpID[1]
    
    # add all copies of the species index to the list of species indices
    spec_index.list[[counter1]] <- sample_mat5
    
    # print information to show that a species has been completed
    print(paste("completed index for species ", counter1, sep=""))
    
    return(spec_index.list)
    
  }
  
  # loop to create species indices
  for (spec in spec_ids) {
    
    # select all populations that belong to a particular species
    spec_popdata <- subset(resamp_popmat, SpecID==spec)
    
    if (resample==FALSE) {
      
      # get species data
      sample_mat1.2 <- as.matrix(spec_popdata[,1:c])
      
      if (nrow(spec_popdata)==1) {
        
        if (lambda==FALSE) {
          
          # adjust all indices to begin at a base value of 100
          sample_mat1.5 <- (sample_mat1.2 / sample_mat1.2[,1] * 100)
          
          # restructure as data frame
          sample_mat5 <- as.data.frame(sample_mat1.5)
          
        } else {
          
          # if using lambda method, convert to lambda values
          sample_mat1.3_pt1 <- sample_mat1.2[,1:(ncol(sample_mat1.2)-1)]
          sample_mat1.3_pt2 <- sample_mat1.2[,2:ncol(sample_mat1.2)]
          sample_mat1.5 <- sample_mat1.3_pt2 / (sample_mat1.3_pt1)
          
          # restructure as data frame
          sample_mat1.7 <- as.data.frame(t(sample_mat1.5))
          
          # convert time series values to log10
          sample_mat1.8 <- log10(sample_mat1.7)
          
          # remove all change values with natural log values outside of -2.3:2.3 (log10 -1:1)
          #sample_mat5 <- as.data.frame(t(apply(sample_mat1.8, 2, function(i) {ifelse(i > -1, ifelse(i < 1, i, NA), NA)})))
          sample_mat5 <- as.data.frame(t(apply(sample_mat1.8, 2, function(i) {ifelse(i > -1, ifelse(i < 1, i, 1), -1)})))
          
        }
        
        # add years as column names
        colnames(sample_mat5) <- colnames(spec_popdata[,1:ncol(sample_mat5)])
        
        # add a species ID column
        sample_mat5$SpecID <- spec
        
        # add a group ID column
        sample_mat5$GrpID <- spec_popdata$GrpID[1]
        
      } else {
        
        # adjust all time series to begin at a base value of 100
        #sample_mat1.2 <- sample_mat / sample_mat[,1] * 100
        
        if (lambda==TRUE) {
          
          # if using lambda method, convert to lambda values
          sample_mat1.3_pt1 <- sample_mat1.2[,1:(ncol(sample_mat1.2)-1)]
          sample_mat1.3_pt2 <- sample_mat1.2[,2:ncol(sample_mat1.2)]
          sample_mat1.5 <- sample_mat1.3_pt2 / (sample_mat1.3_pt1)
          
        } 
        else {
          
          # if not using lambda method, update the variable name
          sample_mat1.5 <- sample_mat1.2
          
        }
        
        # convert time series values to log10
        sample_mat1.7 <- log10(sample_mat1.5)
        
        if (lambda==TRUE) {
          
          # remove all change values with natural log values outside of -2.3:2.3 (log10 -1:1)
          #sample_mat1.8 <- apply(sample_mat1.7, 2, function(i) {ifelse(i > -1, ifelse(i < 1, i, NA), NA)})
          sample_mat1.8 <- apply(sample_mat1.7, 2, function(i) {ifelse(i > -1, ifelse(i < 1, i, 1), -1)})
          
        } else {
          
          # update variable name
          sample_mat1.8 <- sample_mat1.7
          
        }
        
        # take mean of population index values at each time point
        sample_mat2 <- colMeans(sample_mat1.8, na.rm=TRUE)
        
        if (lambda==FALSE) {
          
          # convert back from log10
          sample_mat3 <- 10^(sample_mat2)
          
          # if not using lambda method, set species index to start at 100
          sample_mat4 <- sample_mat3 / sample_mat3[1] * 100
          
        } else {
          
          # convert NaN values to NA
          sample_mat2[is.nan(sample_mat2)] <- NA
          
          # update variable name
          sample_mat4 <- sample_mat2
          
        }
        
        # transpose species index as a matrix
        sample_mat5 <- t(as.matrix(sample_mat4))
        
        # add column names
        colnames(sample_mat5) <- colnames(spec_popdata[,1:ncol(sample_mat5)])
        
        # add rownames
        rownames(sample_mat5) <- rownames(sample_mat2)
        
        # restructure as data frame
        sample_mat5 <- as.data.frame(sample_mat5)
        
        # add a species ID column
        sample_mat5$SpecID <- spec
        
        # add a group ID column
        sample_mat5$GrpID <- spec_popdata$GrpID[1]
        
      }
      
    } 
    else if (resample==TRUE) {
      
      # if there is only one population in the species
      if (nrow(spec_popdata) <= n) {
        
        # randomly sample row numbers from all resampled time series x times, where x is specified by n_boot
        sample_nums <- sample(1:nrow(spec_popdata), n_boot, replace = TRUE)
        
        sample_mat <- spec_popdata[match(sample_nums, row(spec_popdata)),1:c]
        
        if (lambda==TRUE) {
          
          # if using lambda method, convert to lambda values
          sample_mat1.1_pt1 <- sample_mat[,1:(ncol(sample_mat)-1)]
          sample_mat1.1_pt2 <- sample_mat[,2:ncol(sample_mat)]
          sample_mat1.2 <- sample_mat1.1_pt2 / (sample_mat1.1_pt1)
          
          # restructure as data frame
          sample_mat1.5 <- as.data.frame(sample_mat1.2)
          
          # convert time series values to log10
          sample_mat1.7 <- log10(sample_mat1.5)
          
          # remove all change values with log10 values outside of -1:1
          #sample_mat5 <- apply(sample_mat1.7, 2, function(i) {ifelse(i > -1, ifelse(i < 1, i, NA), NA)})
          sample_mat5 <- apply(sample_mat1.7, 2, function(i) {ifelse(i > -1, ifelse(i < 1, i, 1), -1)})
          
        } else {
          
          # adjust all indices to begin at a base value of 100
          sample_mat1.2 <- (sample_mat / sample_mat[,1] * 100)
          
          # restructure as data frame
          sample_mat5 <- as.data.frame(sample_mat1.2)
          
        }
        
        # add years as column names
        colnames(sample_mat5) <- colnames(spec_popdata[,1:ncol(sample_mat5)])
        
        # restructure as data frame
        sample_mat5 <- as.data.frame(sample_mat5)
        
        # add a species ID column
        sample_mat5$SpecID <- spec
        
        # add a group ID column
        sample_mat5$GrpID <- spec_popdata$GrpID[1]
        
      } else {
        
        if (random_pops==TRUE) {
          
          ### bootstrapping the species index, with replacement ###
          
          # randomly sample from all resampled populations x times, where x is specified by n_boot times the number
          # of populations that belong to the species
          sample_nums <- sample(1:nrow(spec_popdata), n_boot*nrow(spec_popdata) / n, replace = TRUE)
          
          # get data from spec_popdata by matching to row numbers in sample_nums
          sample_mat <- spec_popdata[match(sample_nums, row(spec_popdata)),]
          
        } else {
          
          ### bootstrapping the species index, with replacement ###
          
          # make a vector of all unique population IDs in the species
          pop_ids <- unique(spec_popdata$PopID)
          
          # create a counter to keep track of the number of populations sampled
          p_counter <- 1
          
          # create a vector to hold time series row numbers
          sample_mat <- data.frame()
          
          for (pop in pop_ids) {
            
            # select all resampled time series that belong to a particular population
            popdata <- subset(spec_popdata, PopID==pop)
            
            # randomly sample row numbers from all resampled time series x times, where x is specified by n_boot
            sample_nums <- sample(1:nrow(popdata), n_boot, replace = TRUE)
            
            # get data from popdata by matching to row numbers in sample_nums
            sample_mat[(1+(p_counter*n_boot)-n_boot):(p_counter*n_boot),1:ncol(popdata)] <- popdata[match(sample_nums, row(popdata)),]
            
            # advance the population counter
            p_counter <- p_counter + 1
          }
          
        }
        
        
        # remove the id, popid and specid columns from sample_mat
        sample_mat1.2 <- sample_mat[,1:c]
        
        # adjust all indices to begin at a base value of 100
        #sample_mat1.2 <- (sample_mat1.1 / sample_mat1.1[,1] * 100)
        
        if (lambda==TRUE) {
          
          # if using lambda method, convert to lambda values
          sample_mat1.3_pt1 <- sample_mat1.2[,1:(ncol(sample_mat1.2)-1)]
          sample_mat1.3_pt2 <- sample_mat1.2[,2:ncol(sample_mat1.2)]
          sample_mat1.5 <- sample_mat1.3_pt2 / (sample_mat1.3_pt1)
          
        } else {
          
          # if not using lambda method, update the variable name
          sample_mat1.5 <- sample_mat1.2
          
        }
        
        # convert to log10 and convert to matrix format for faster processing
        sample_mat1.7 <- log10(as.matrix(sample_mat1.5))
        
        if (lambda==TRUE) {
          
          # remove all change values with log10 values outide of -1:1
          #sample_mat1.75 <- apply(sample_mat1.7, 2, function(i) {ifelse(i > -1, ifelse(i < 1, i, NA), NA)})
          sample_mat1.75 <- apply(sample_mat1.7, 2, function(i) {ifelse(i > -1, ifelse(i < 1, i, 1), -1)})
          
        } else {
          
          sample_mat1.75 <- sample_mat1.7
          
        }
        
        # transpose data and convert to vector
        sample_mat1.8 <- as.vector(t(sample_mat1.75))
        
        # create a new matrix in wide format, so column means can be used to create species indices
        sample_mat1.9 <- matrix(sample_mat1.8, nrow=length(unique(spec_popdata$PopID)), 
                                ncol=(n_boot*ncol(sample_mat1.7)), byrow=TRUE)
        
        # take the column means. the .colMeans function requires extra information but is very fast
        sample_mat2 <- .colMeans(sample_mat1.9, nrow(sample_mat1.9), ncol(sample_mat1.9), na.rm=TRUE)
        
        # convert back to long format, with a species index in each row
        sample_mat3 <- matrix(sample_mat2, nrow = n_boot, ncol = ncol(sample_mat1.7), byrow=TRUE)
        
        if (lambda==TRUE) {
          
          # convert NaN values to NA
          sample_mat3[is.nan(sample_mat3)] <- NA
          
          # update variable name
          sample_mat4 <- sample_mat3
          
        } else {
          
          # convert back from log10 to index values
          sample_mat3 <- 10^(sample_mat3)
          
          # set each species index to start at 100
          sample_mat4 <- (sample_mat3 / sample_mat3[,1] * 100)
          
        }
        
        # convert to a data frame
        sample_mat5 <- as.data.frame(sample_mat4)
        
        # add years as column names
        colnames(sample_mat5) <- colnames(spec_popdata[,1:ncol(sample_mat5)])
        
        # add a species id column
        sample_mat5$SpecID <- rep(spec, times=nrow(sample_mat5))
        
        # add a group id column
        sample_mat5$GrpID <- rep(spec_popdata$GrpID[1], times=nrow(sample_mat5))
        
      }
      
    }
    
    # add all resampled copies of the species index to the list of species indices
    spec_index.list[[counter1]] <- sample_mat5
    
    # print information to show that a species has been completed
    print(paste("completed index for species ", counter1, sep=""))
    
    # increase counter value to keep track of the number of species completed
    counter1 <- counter1 + 1
    
  }
  
  return(spec_index.list)
  
}
