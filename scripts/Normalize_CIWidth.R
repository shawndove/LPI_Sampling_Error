dir_name <- "TestData/Testing2/"
dir_name2 <- "TestData/Testing2/fixed2/"

dir_names <- list.dirs(path="TestData", 
                       full.names = TRUE, 
                       recursive = FALSE)

# remove unwanted directories
dir_names <- dir_names[-length(dir_names)]
dir_names <- paste(dir_names, "/", sep="")
dir_names <- dir_names[1351:1510]

source("scripts/CIWidth_Normalize_Function.R")

msi_ci_files_lambda2 <- vector()
msi_ci_files_lr <- vector()
msi_ci_files_ga <- vector()
msi_files_lambda2 <- vector()
msi_files_lr <- vector()
msi_files_ga <- vector()
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
  
  msi_files_lambda2[i] <- list.files(paste(dir_names[i], 
                                              sep=""), 
                                        pattern = paste("msi_final_sampled_list_lambda2", sep=""))
  
  msi_files_lr[i] <- list.files(paste(dir_names[i], 
                                         sep=""), 
                                   pattern = paste("msi_final_sampled_list_lr", sep=""))
  
  msi_files_ga[i] <- list.files(paste(dir_names[i], 
                                         sep=""), 
                                   pattern = paste("msi_final_sampled_list_gamall", sep=""))
}

for (z in 1:length(dir_names)) {
  
  msi_ci_list_lambda2 <- readRDS(file=paste(dir_names[z], msi_ci_files_lambda2[z], sep=""))
  msi_ci_list_lr <- readRDS(file=paste(dir_names[z], msi_ci_files_lr[z], sep=""))
  msi_ci_list_ga <- readRDS(file=paste(dir_names[z], msi_ci_files_ga[z], sep=""))
  msi_list_lambda2 <- readRDS(file=paste(dir_names[z], msi_files_lambda2[z], sep=""))
  msi_list_lr <- readRDS(file=paste(dir_names[z], msi_files_lr[z], sep=""))
  msi_list_ga <- readRDS(file=paste(dir_names[z], msi_files_ga[z], sep=""))
  ID <- ID_files[z]

  # test how much of the real trend is within the confidence intervals of the sampled trends
  ciwidth.normalized.list <- lapply(1:length(msi_ci_list_lambda2), 
                                    function(i) ci_width_normalize_fn(msi_ci_list_lambda2[[i]], msi_list_lambda2[[i]], 50))
  ciwidth.normalized.matrix <- do.call(rbind, ciwidth.normalized.list)
  colnames(ciwidth.normalized.matrix) <- "MSI"
  write.csv(ciwidth.normalized.matrix, file=paste(dir_names[z], "saved_synth_", ID, "_ciwidth_normalized_sampled_list_lambda2.csv", sep=""))
  
  mean.within.ci.tp <- mean(ciwidth.normalized.matrix, na.rm=TRUE)
  write.csv(mean.within.ci.tp, file=paste(dir_names[z], "saved_synth_", ID, "_ciwidth_normalized_sampled_mean_lambda2.csv", sep=""))
  
  # test how much of the real trend is within the confidence intervals of the sampled trends
  ciwidth.normalized.list <- lapply(1:length(msi_ci_list_lr), 
                                    function(i) ci_width_normalize_fn(msi_ci_list_lr[[i]], msi_list_lr[[i]], 50))
  ciwidth.normalized.matrix <- do.call(rbind, ciwidth.normalized.list)
  colnames(ciwidth.normalized.matrix) <- "MSI"
  write.csv(ciwidth.normalized.matrix, file=paste(dir_names[z], "saved_synth_", ID, "_ciwidth_normalized_sampled_list_lr.csv", sep=""))
  
  mean.ciwidth.normalized <- mean(ciwidth.normalized.matrix, na.rm=TRUE)
  write.csv(mean.ciwidth.normalized, file=paste(dir_names[z], "saved_synth_", ID, "_ciwidth_normalized_sampled_mean_lr.csv", sep=""))
  
  # test how much of the real trend is within the confidence intervals of the sampled trends
  ciwidth.normalized.list <- lapply(1:length(msi_ci_list_ga), 
                                    function(i) ci_width_normalize_fn(msi_ci_list_ga[[i]], msi_list_ga[[i]], 50))
  ciwidth.normalized.matrix <- do.call(rbind, ciwidth.normalized.list)
  colnames(ciwidth.normalized.matrix) <- "MSI"
  write.csv(ciwidth.normalized.matrix, file=paste(dir_names[z], "saved_synth_", ID, "_ciwidth_normalized_sampled_list_gamall.csv", sep=""))
  
  mean.ciwidth.normalized <- mean(ciwidth.normalized.matrix, na.rm=TRUE)
  write.csv(mean.ciwidth.normalized, file=paste(dir_names[z], "saved_synth_", ID, "_ciwidth_normalized_sampled_mean_gamall.csv", sep=""))
  
}
