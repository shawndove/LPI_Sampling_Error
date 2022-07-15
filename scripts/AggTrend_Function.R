aggregate_index_fn <- function(ms_index.list, c, m_colnames, n=NA, n_boot=NA, stay_lambda=FALSE) {
  
  # check whether data is in list format
  if (inherits(ms_index.list, "list")) {
    
    # if so, convert group index list to data frame
    ms_grpdata <- do.call(rbind, ms_index.list)
    
  } else {
    
    # otherwise, copy group index directly
    ms_grpdata <- ms_index.list
    
  }
  
  # create a single-column matrix of 100s for later conversion to index values
  first_col <- matrix(100, nrow = n_boot, ncol = 1)
  
  # remove ID tags
  ms_grpdata1.4 <- ms_grpdata[,1:(c-1)]
  
  # convert to log10
  #ms_grpdata1.4 <- log10(ms_grpdata1.1)
  
  # transpose data and convert to vector
  ms_grpdata1.5 <- as.vector(t(ms_grpdata1.4))
  
  # create a new matrix in wide format, so column means can be used to create group indices
  ms_grpdata1.6 <- matrix(ms_grpdata1.5, nrow=length(unique(ms_grpdata$GrpID)), 
                          ncol=(n_boot*ncol(ms_grpdata1.4)), byrow=TRUE)
  
  # take the column sums. the .colSums function requires extra information but is very fast
  ms_grpdata1.7 <- .colSums(ms_grpdata1.6, nrow(ms_grpdata1.6), ncol(ms_grpdata1.6), na.rm=TRUE)
  
  # convert back to long format, with a species index in each row
  ms_grpdata1.8 <- matrix(ms_grpdata1.7, nrow = n_boot, ncol = ncol(ms_grpdata1.4), byrow=TRUE)
  
  # convert NaN values to 0
  ms_grpdata1.8[is.nan(ms_grpdata1.8)] <- 0
  
  if (stay_lambda==TRUE) {
    
    # convert to a data frame
    ms_grpdata2.1 <- as.data.frame(ms_grpdata1.8)
    
    # add years as column names
    colnames(ms_grpdata2.1) <- m_colnames[1:(c-1)]
    
    # return indices
    return(ms_grpdata2.1)
    
  }
  
  # back convert from log10
  ms_grpdata1.9 <- 10^(ms_grpdata1.8)
  
  # convert to index values
  ms_grpdata2 <- cbind(first_col, ms_grpdata1.9)
  ms_grpdata2.1 <- rowCumprods(ms_grpdata2)
  
  # convert NaN values to NA
  ms_grpdata2.1[is.nan(ms_grpdata2.1)] <- NA
  
  # convert to a data frame
  ms_grpdata2.1 <- as.data.frame(ms_grpdata2.1)
  
  # add years as column names
  colnames(ms_grpdata2.1) <- m_colnames
  
  # return indices
  return(ms_grpdata2.1)
  
}
