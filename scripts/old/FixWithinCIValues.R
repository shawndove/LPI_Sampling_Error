# recalculate withinci values

source("scripts/WithinCI_Function.R")


dir_name <- "TestData/"

dir_names <- list.dirs(path="TestData", 
                           full.names = TRUE, 
                           recursive = FALSE)

dir_names <- dir_names[-length(dir_names)]
dir_names <- paste(dir_names, "/", sep="")

msi_truetrend_files <- vector()
msi_lambda_files <- vector()
msi_lr_files <- vector()
lambda_wci_list_files <- vector()
lambda_wci_mean_files <- vector()
lr_wci_list_files <- vector()
lr_wci_mean_files <- vector()

for (i in 1:length(dir_names)) {
  
  msi_truetrend_files[i] <- list.files(paste(dir_names[i], 
                                          sep=""), 
                                    pattern = "msi_TrueTrend.RData")

  msi_lambda_files[i] <- list.files(paste(dir_names[i], 
                                       sep=""), 
                                 pattern = "msi_sampled_list_ci.*.lambda2.RData")
  
  lambda_wci_list_files[i] <- list.files(paste(dir_names[i], 
                                               sep=""), 
                                         pattern = "within_ci_sampled_list_lambda2.csv")
  
  lambda_wci_mean_files[i] <- list.files(paste(dir_names[i], 
                                               sep=""), 
                                         pattern = "within_ci_sampled_mean_lambda2.csv")
  
  lr_wci_list_files[i] <- list.files(paste(dir_names[i], 
                                               sep=""), 
                                         pattern = "within_ci_sampled_list_lr.csv")
  
  lr_wci_mean_files[i] <- list.files(paste(dir_names[i], 
                                               sep=""), 
                                         pattern = "within_ci_sampled_mean_lr.csv")

  msi_lr_files[i] <- list.files(paste(dir_names[i], 
                                          sep=""), 
                                    pattern = "msi_sampled_list_ci.*.lr.RData")

}

for (z in 1:length(dir_names)) {
  
  msi_truetrend <- readRDS(file=paste(dir_names[z], msi_truetrend_files[z], sep=""))
  msi_lambda_list <- readRDS(file=paste(dir_names[z], msi_lambda_files[z], sep=""))
  msi_lr_list <- readRDS(file=paste(dir_names[z], msi_lr_files[z], sep=""))
  
  # test how much the sampled trends deviate from the real trend
  within.ci.list <- lapply(msi_lambda_list, real_trend_within_ci_fn, msi_truetrend)
  within.ci.matrix <- do.call(rbind, within.ci.list)
  colnames(within.ci.matrix) <- "MSI"
  write.csv(within.ci.matrix, file=paste(dir_names[z], lambda_wci_list_files[z], sep=""))
  
  mean.within.ci <- mean(within.ci.matrix, na.rm=TRUE)
  write.csv(mean.within.ci, file=paste(dir_names[z], lambda_wci_mean_files[z], sep=""))
  
  within.ci.list <- lapply(msi_lr_list, real_trend_within_ci_fn, msi_truetrend)
  within.ci.matrix <- do.call(rbind, within.ci.list)
  colnames(within.ci.matrix) <- "MSI"
  write.csv(within.ci.matrix, file=paste(dir_names[z], lr_wci_list_files[z], sep=""))
  
  mean.within.ci <- mean(within.ci.matrix, na.rm=TRUE)
  write.csv(mean.within.ci, file=paste(dir_names[z], lr_wci_mean_files[z], sep=""))
}

