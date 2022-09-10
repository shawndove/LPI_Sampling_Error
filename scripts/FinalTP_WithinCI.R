dir_name <- "TestData/Testing2/"
dir_name2 <- "TestData/Testing2/fixed2/"

dir_names <- list.dirs(path="TestData", 
                       full.names = TRUE, 
                       recursive = FALSE)

# remove unwanted directories
dir_names <- dir_names[-length(dir_names)]
dir_names <- paste(dir_names, "/", sep="")
dir_names <- dir_names[1255:1350]

source("scripts/FinalTP_WithinCI_Function.R")

msi_ci_files_lambda2 <- vector()
msi_ci_files_lr <- vector()
msi_ci_files_ga <- vector()
msi_truetrend_files <- vector()
ID_files <- vector()
for (i in 1:length(dir_names)) {
  
  info.list <- list.files(paste(dir_names[i], sep=""), pattern="_info")
  
  ID_files[i] <- read.csv(file=paste(dir_names[i], info.list, sep=""))$ID
  
  msi_ci_files_lambda2[i] <- list.files(paste(dir_names[i], 
                                              sep=""), 
                                        pattern = paste("msi_sampled_list_ci_lambda2", sep=""))
  
  msi_ci_files_lr[i] <- list.files(paste(dir_names[i], 
                                              sep=""), 
                                        pattern = paste("msi_sampled_list_ci_lr", sep=""))
  
  msi_ci_files_ga[i] <- list.files(paste(dir_names[i], 
                                         sep=""), 
                                   pattern = paste("msi_sampled_list_ci_gamall", sep=""))
  
  msi_truetrend_files[i] <- list.files(paste(dir_names[i], 
                                             sep=""), 
                                       pattern = "msi_TrueTrend.RData")
  
}

for (z in 1:length(dir_names)) {
  
  msi_ci_list_lambda2 <- readRDS(file=paste(dir_names[z], msi_ci_files_lambda2[z], sep=""))
  msi_ci_list_lr <- readRDS(file=paste(dir_names[z], msi_ci_files_lr[z], sep=""))
  msi_ci_list_ga <- readRDS(file=paste(dir_names[z], msi_ci_files_ga[z], sep=""))
  msi_real <- readRDS(file=paste(dir_names[z], msi_truetrend_files[z], sep=""))
  ID <- ID_files[z]

  # test how much of the real trend is within the confidence intervals of the sampled trends
  within.ci.tp.list <- lapply(msi_ci_list_lambda2, real_finaltp_within_ci_fn, msi_real)
  within.ci.tp.matrix <- do.call(rbind, within.ci.tp.list)
  colnames(within.ci.tp.matrix) <- "MSI"
  write.csv(within.ci.tp.matrix, file=paste(dir_names[z], "saved_synth_", ID, "_within_ci_finaltp_sampled_list_lambda2.csv", sep=""))
  
  mean.within.ci.tp <- mean(within.ci.tp.matrix, na.rm=TRUE)
  write.csv(mean.within.ci.tp, file=paste(dir_names[z], "saved_synth_", ID, "_within_ci_finaltp_sampled_mean_lambda2.csv", sep=""))
  
  # test how much of the real trend is within the confidence intervals of the sampled trends
  within.ci.tp.list <- lapply(msi_ci_list_lr, real_finaltp_within_ci_fn, msi_real)
  within.ci.tp.matrix <- do.call(rbind, within.ci.tp.list)
  colnames(within.ci.tp.matrix) <- "MSI"
  write.csv(within.ci.tp.matrix, file=paste(dir_names[z], "saved_synth_", ID, "_within_ci_finaltp_sampled_list_lr.csv", sep=""))
  
  mean.within.ci.tp <- mean(within.ci.tp.matrix, na.rm=TRUE)
  write.csv(mean.within.ci.tp, file=paste(dir_names[z], "saved_synth_", ID, "_within_ci_finaltp_sampled_mean_lr.csv", sep=""))
  
  # test how much of the real trend is within the confidence intervals of the sampled trends
  within.ci.tp.list <- lapply(msi_ci_list_ga, real_finaltp_within_ci_fn, msi_real)
  within.ci.tp.matrix <- do.call(rbind, within.ci.tp.list)
  colnames(within.ci.tp.matrix) <- "MSI"
  write.csv(within.ci.tp.matrix, file=paste(dir_names[z], "saved_synth_", ID, "_within_ci_finaltp_sampled_list_gamall.csv", sep=""))
  
  mean.within.ci.tp <- mean(within.ci.tp.matrix, na.rm=TRUE)
  write.csv(mean.within.ci.tp, file=paste(dir_names[z], "saved_synth_", ID, "_within_ci_finaltp_sampled_mean_gamall.csv", sep=""))
  
}
