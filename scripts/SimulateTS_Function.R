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

