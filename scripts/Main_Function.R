## Main Function ----

all_fn <- function(popvar, 
                   popmean, 
                   sdmean,
                   pgrowthx, 
                   iter_num, 
                   tmax, 
                   tpops, 
                   popspec, 
                   n, 
                   n_boot, 
                   ngrps,
                   count_thres, 
                   min_ts_length, 
                   c, 
                   samp_size, 
                   m_colnames, 
                   mlength, 
                   numobs, 
                   bootstrap_size, 
                   error=FALSE,
                   mean_cv,
                   cv_sd) {
  
  ## load external functions ----
  
  source("scripts/SimulateTS_Function.R")
  source("scripts/CI_Functions.R")
  source("scripts/GI_Functions.R")
  source("scripts/SI_Functions.R")
  source("scripts/GAM_Functions.R")
  source("scripts/ChainMethod_Function.R")
  source("scripts/GRCalc_Function.R")
  source("scripts/ObsError_Function.R")
  source("scripts/Degrade_Function.R")
  source("scripts/Cull_Function.R")
  source("scripts/WithinCI_Function.R")
  source("scripts/CIWidth_Function.R")
  source("scripts/Methods_Function.R")
  
  # create directory to store files
  if(!dir.exists("TestData/")) {dir.create("TestData/")}
  if(!dir.exists(paste("TestData/", iter_num, sep=""))) {dir.create(paste("TestData/", iter_num, sep=""))}
  
  # create synthetic populations, assigned to different species
  if (pgrowthx == 7) {
    
    # create synthetic populations, assigned to different species
    all_pops_index <- pgrowth4.5(tpops, tmax, popmean, popvar, popspec, sdmean)
    
  } else("Check your synthetic data generator function input setting.")
  
  # save raw synthetic dataset
  saveRDS(all_pops_index, file=paste("TestData/", iter_num, "/saved_synth_", iter_num, "_raw.RData", sep=""))
  
  cat(paste("Constructing dataset:", "\nPopulations:", tpops, "\nSpecies:", length(unique(all_pops_index$SpecID)), "\n\n"))
  
  cat(paste0("Plotting geometric mean of the dataset.\n"))
  
  # create geometric mean and plot it
  index_geomean <- exp(colMeans(log(all_pops_index[,1:c])))
  
  png(file=paste("TestData/", iter_num, "/saved_synth_", iter_num, "_geometric_mean_raw.png", sep=""),
      width = 1280, height = 720, units = "px", pointsize = 12, bg = "white")
  
  par(mar=c(6,6,6,2) + 0.1)
  plot(m_colnames, index_geomean[1:c], type="l", lty=1, lwd=2, xlab="", ylab="", cex.axis=1.5)
  grid(col="grey85", lty=2)
  mtext(text = "Year", side = 1, line = 4, cex = 2)
  mtext(text = "Index", side = 2, line = 3.5, cex = 2)
  
  dev.off()
  
  cat(paste0("Plotting a GAM of the geometric mean.\n"))
  
  # GAM the geometric mean and plot it
  g <- gam(index_geomean[1:c]~s(m_colnames,k = round(c/2)))$fitted.values
  g.gam <- g / g[1] * 100
  y.low <- min(g.gam) - 5
  y.high <- max(g.gam) + 5
  
  png(file=paste("TestData/", iter_num, "/saved_synth_", iter_num, "_raw_gam_geometric_mean.png", sep=""),
      width = 1280, height = 720, units = "px", pointsize = 12, bg = "white")
  
  plot(m_colnames, g.gam, xlab="", ylab="", type="l", lty=1, ylim=c(y.low,y.high), lwd=2, frame.plot=TRUE,
       cex.axis=1.5)
  grid(col="grey85", lty=2)
  mtext(text = "Year", side = 1, line = 4, cex = 2)
  mtext(text = "Index", side = 2, line = 3.5, cex = 2)
  
  dev.off()
  
  #all_pops_index <- readRDS(file=paste("TestData/", iter_num, "/saved_synth_", iter_num, "_raw.RData", sep=""))
  
  # add a population ID column
  all_pops_index$PopID <- rownames(all_pops_index)
  
  if (error==TRUE) {
    
    cat(paste("Adding random sampling error to dataset.\n"))
    
    # add sampling error to dataset
    all_pops_error <- error_intr_fn2(all_pops_index, m_colnames, mean_cv, cv_sd)
    saveRDS(all_pops_error, file=paste("TestData/", iter_num, "/saved_synth_", iter_num, "_error.RData", sep=""))
    
    cat(paste0("Degrading dataset.\n"))
    
    # degrade dataset
    all_pops_degraded <- degrade_ts_fn(all_pops_error, c, mlength, numobs)
    
    saveRDS(all_pops_degraded, file=paste("TestData/", iter_num, "/saved_synth_", iter_num, "_degraded.RData", sep=""))
    
  } else {
    
    cat(paste0("Degrading dataset.\n"))
    
    # degrade dataset
    all_pops_degraded <- degrade_ts_fn(all_pops_index, c, mlength, numobs)
    
    saveRDS(all_pops_degraded, file=paste("TestData/", iter_num, "/saved_synth_", iter_num, "_degraded.RData", sep=""))
    
  }
  
  cat(paste0("Removing time series with less than ", count_thres, " total observations and/or with a length of less than ", min_ts_length, ".\n"))
  
  # remove time series which are too short or have too few counts
  grp_data_culled <- cull_fn(all_pops_degraded, count_thres, min_ts_length, c)
  saveRDS(grp_data_culled, file=paste("TestData/", iter_num, "/saved_synth_", iter_num, "_culled.RData", sep=""))
  
  cat(paste0("Creating ", bootstrap_size, " randomly sampled subsets of ", samp_size, " populations each.\n"))
  
  # if sample size is too large, set it to the total number of populations - 1
  if (nrow(grp_data_culled) < samp_size) {
    samp_size <- (nrow(grp_data_culled)-1)
  }
  
  # create list of x population ID matrices at a given sample size, where x is the number of sample bootstraps
  sample_pop_id_list <- list()
  for (i in 1:bootstrap_size) {
    sample_pop_id_list[[i]] <- sample(grp_data_culled$PopID, samp_size)
  }
  #sample_pop_id_list <- readRDS(file=paste("TestData/", iter_num-40, "/saved_synth_", iter_num-40, "_sample_pop_id_list.RData", sep=""))
  saveRDS(sample_pop_id_list, file=paste("TestData/", iter_num, "/saved_synth_", iter_num, "_sample_pop_id_list.RData", sep=""))
  
  
  ## TRUE TREND ##
  
  cat(paste0("Creating ", length(unique(all_pops_index$SpecID)), " species indices for true trend.\n"))
  
  # create species indices from the population indices
  full_spec_real <- species_index_fn(all_pops_index, c)
  saveRDS(full_spec_real, file=paste("TestData/", iter_num, "/saved_synth_", iter_num, "_species_indices_TrueTrend.RData", sep=""))
  
  cat(paste0("Creating msi for true trend.\n"))
  
  # create multi species indices from the group indices
  msi_real <- group_index_fn(full_spec_real, c, m_colnames)
  saveRDS(msi_real, file=paste("TestData/", iter_num, "/saved_synth_", iter_num, "_msi_TrueTrend.RData", sep=""))
  
  cat(paste0("Creating msi confidence intervals for true trend.\n"))
  
  # create confidence intervals for the multi species index
  #msi_ci_real <- ci_fn(full_spec_real, c, m_colnames)
  #saveRDS(msi_ci_real, file=paste("TestData/", iter_num, "/saved_synth_", iter_num, "_msi_ci_TrueTrend.RData", sep=""))
  
  cat(paste0("Plotting true trend.\n"))
  
  # plot trend
  plot_msi <- msi_real
  #plot_ci <- msi_ci_real
  
  png(file=paste("TestData/", iter_num, "/saved_synth_", iter_num, "_msi_TrueTrend.png", sep=""),
      width = 1280, height = 720, units = "px", pointsize = 12, bg = "white")
  
  par(mar=c(6,6,6,2) + 0.1)
  plot(m_colnames, plot_msi, xlab="", ylab="", type="l", lty=1, lwd=3, frame.plot=TRUE,
       cex.lab=1.5, cex.axis=1.5)
  lines(m_colnames, g.gam, lty=1, lwd=3, col="red")
  grid(col="grey85", lty=2)
  mtext(text = "Year", side = 1, line = 4, cex = 1.5)
  mtext(text = "Index", side = 2, line = 3.5, cex = 1.5)
  
  dev.off()
  
  
  #temp <- method_fn(grp_data_culled, sample_pop_id_list, msi_real, c, m_colnames, n=NA, n_boot=NA, iter_num, samp_size, bootstrap_size, method="nores")
  
  #temp <- method_fn(grp_data_culled, sample_pop_id_list, msi_real, c, m_colnames, n=NA, n_boot=NA, iter_num, samp_size, bootstrap_size, method="lambda")
  
  temp <- method_fn(grp_data_culled, sample_pop_id_list, msi_real, c, m_colnames, n=n, n_boot=n_boot, iter_num, samp_size, bootstrap_size, method="lr")
  
  #temp <- method_fn(grp_data_culled, sample_pop_id_list, msi_real, c, m_colnames, n=n, n_boot=n_boot, iter_num, samp_size, bootstrap_size, method="my")
  
  temp <- method_fn(grp_data_culled, sample_pop_id_list, msi_real, c, m_colnames, n=NA, n_boot=NA, iter_num, samp_size, bootstrap_size, method="lambda2")
  
  
  ## DATA GATHERING ##
  
  # get growth rate and other info
  gr.stats.raw <- growth_rate_calc_fn3(all_pops_index)
  
  all_pops_completed <- complete_time_series(all_pops_degraded, c, m_colnames, calcsd=TRUE)
  
  gr.stats.degraded <- growth_rate_calc_fn3(all_pops_completed, model=TRUE)
  
  # create csv file with important info about the dataset
  info.dat <- data.frame(row.names = "1")
  info.dat$ID <- iter_num
  info.dat$synth_version <- pgrowthx
  info.dat$num_pops <- tpops
  info.dat$num_years <- tmax
  info.dat$num_resamps <- n
  info.dat$num_bootstraps <- n_boot
  info.dat$min_ts_length <- min_ts_length + 1
  info.dat$mean_gr_raw <- gr.stats.raw[[1]]
  info.dat$gr_sd_raw <- gr.stats.raw[[2]]
  info.dat$mean_sd_raw <- gr.stats.raw[[3]]
  info.dat$mean_gr_degraded <- gr.stats.degraded[[1]]
  info.dat$gr_sd_degraded <- gr.stats.degraded[[2]]
  info.dat$mean_sd_degraded <- gr.stats.degraded[[3]]
  info.dat$samp_size <- samp_size
  info.dat$pops_per_species <- popspec
  info.dat$mean_ts_length <- sum(!is.na(as.vector(all_pops_completed[,1:50]))) / nrow(all_pops_completed)
  info.dat$mean_num_obs <- sum(!is.na(as.vector(all_pops_degraded[,1:50]))) / nrow(all_pops_degraded)
  #info.dat$degrade_type <- degrade
  info.dat$error <- error
  info.dat$mean_cv <- mean_cv
  info.dat$cv_sd <- cv_sd
  
  write.csv(info.dat, file=paste("TestData/", iter_num, "/saved_synth_", iter_num, "_info.csv", sep=""))
  
  # get growth rate and other info from samples
  gr.stats.samples <- list()
  tslength.samples <- vector()
  numobs.samples <- vector()
  for (i in 1:length(sample_pop_id_list)) {
    
    temp <- all_pops_degraded[all_pops_degraded$PopID %in% sample_pop_id_list[[i]],]
    temp2 <- complete_time_series(temp, c, m_colnames, calcsd=TRUE)
    gr.stats.samples[[i]] <- growth_rate_calc_fn3(temp2, model=TRUE)
    tslength.samples[i] <- sum(!is.na(as.vector(temp2[,1:50]))) / nrow(temp2)
    numobs.samples[i] <- sum(!is.na(as.vector(temp[,1:50]))) / nrow(temp)
    
  }
  
  info.samples.meangr <- list()
  info.samples.grsd <- list()
  info.samples.meansd <- list()
  info.samples.meantslength <- vector()
  info.samples.meannumobs <- vector()
  for (i in 1:length(sample_pop_id_list)) {
    
    info.samples.meangr[[i]] <- gr.stats.samples[[i]][[1]]
    info.samples.grsd[[i]] <- gr.stats.samples[[i]][[2]]
    info.samples.meansd[[i]] <- gr.stats.samples[[i]][[3]]
    info.samples.meantslength[i] <- tslength.samples[i]
    info.samples.meannumobs[i] <- numobs.samples[i]
    
  }
  
  info.samples <- data.frame(mean_gr_sample = do.call(rbind, info.samples.meangr),
                             gr_sd_sample = do.call(rbind, info.samples.grsd),
                             mean_sd_sample = do.call(rbind, info.samples.meansd),
                             mean_ts_length = info.samples.meantslength,
                             mean_num_obs = info.samples.meannumobs)
  write.csv(info.samples, file=paste("TestData/", iter_num, "/saved_synth_", iter_num, "_gr_samples.csv", sep=""))
  
}
