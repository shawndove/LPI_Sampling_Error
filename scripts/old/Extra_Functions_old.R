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

