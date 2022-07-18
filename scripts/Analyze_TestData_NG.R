
# save the test directory name to a variable
dir_name <- "TestData/Testing2/"
dir_name2 <- "TestData/Testing2/fixed1/"

dir_names <- list.dirs(path="TestData", 
                       full.names = TRUE, 
                       recursive = FALSE)

# remove unwanted directories
dir_names <- dir_names[-length(dir_names)]
dir_names <- paste(dir_names, "/", sep="")

# get lists of csv files in the test directory
# info_list <- list.files(paste(dir_name, 
#                               sep=""), 
#                         pattern = "info",
#                         full.names = TRUE,
#                         recursive = FALSE)
# 
# gr_samples_list <- list.files(paste(dir_name, 
#                               sep=""), 
#                              pattern = "gr_samples",
#                              full.names = TRUE,
#                              recursive = FALSE)
# 
# wci_list <- list.files(paste(dir_name, 
#                              sep=""), 
#                        pattern = "within_ci_sampled_list_lambda",
#                        full.names = TRUE,
#                        recursive = FALSE)
# 
# ciw_list <- list.files(paste(dir_name, 
#                              sep=""), 
#                        pattern = "ci_width_sampled_list_lambda",
#                        full.names = TRUE,
#                        recursive = FALSE)
# 
# tsd_list <- list.files(paste(dir_name,
#                              sep=""),
#                        pattern = "culled.",
#                        full.names = TRUE,
#                        recursive = FALSE)
# 
# tscd_list <- list.files(paste(dir_name,
#                               sep=""),
#                         pattern = "completed_time_series",
#                         full.names = TRUE,
#                         recursive = FALSE)
# 
# samp_list <- list.files(paste(dir_name,
#                               sep=""),
#                         pattern = "sample_pop_id_list",
#                         full.names = TRUE,
#                         recursive = FALSE)
# 
# # copy files to new directory
# file.copy(c(info_list,
#             gr_samples_list,
#             wci_list,
#             ciw_list,
#             tsd_list,
#             tscd_list,
#             samp_list), 
#           dir_name2,
#           overwrite = TRUE)

# reset previously used vectors to empty
info_list <- vector()
gr_samples_list <- vector()
wci_list_lambda <- vector()
wcim_list_lambda <- vector()
wci_ftp_list_lambda <- vector()
wcim_ftp_list_lambda <- vector()
ciw_list_lambda <- vector()
ciwm_list_lambda <- vector()
tdm_list_lambda <- vector()
td_list_lambda <- vector()
wci_list_lr <- vector()
wcim_list_lr <- vector()
wci_ftp_list_lr <- vector()
wcim_ftp_list_lr <- vector()
ciw_list_lr <- vector()
ciwm_list_lr <- vector()
tdm_list_lr <- vector()
td_list_lr <- vector()
tsd_list <- vector()
tscd_list <- vector()
samp_list <- vector()

for (i in 1:length(dir_names)) {
  
  info_list[i] <- list.files(paste(dir_names[i], 
                                sep=""), 
                             pattern = "info",
                             full.names = TRUE,
                             recursive = FALSE)
  
  
  gr_samples_list[i] <- list.files(paste(dir_names[i], 
                                   sep=""), 
                                   pattern = "gr_samples",
                                   full.names = TRUE,
                                   recursive = FALSE)
  
  tdm_list_lambda[i] <- list.files(paste(dir_names[i], 
                               sep=""), 
                         pattern = "trend_dev_sampled_mean_lambda2",
                         full.names = TRUE,
                         recursive = FALSE)
  
  td_list_lambda[i] <- list.files(paste(dir_names[i], 
                              sep=""), 
                        pattern = "trend_dev_sampled_list_lambda2",
                        full.names = TRUE,
                        recursive = FALSE)
  
  wcim_list_lambda[i] <- list.files(paste(dir_names[i], 
                               sep=""), 
                             pattern = "within_ci_sampled_mean_lambda2",
                             full.names = TRUE,
                             recursive = FALSE)
  
  wci_list_lambda[i] <- list.files(paste(dir_names[i], 
                                  sep=""), 
                            pattern = "within_ci_sampled_list_lambda2",
                            full.names = TRUE,
                            recursive = FALSE)
  
  wcim_ftp_list_lambda[i] <- list.files(paste(dir_names[i], 
                                          sep=""), 
                                    pattern = "within_ci_finaltp_sampled_mean_lambda2",
                                    full.names = TRUE,
                                    recursive = FALSE)
  
  wci_ftp_list_lambda[i] <- list.files(paste(dir_names[i], 
                                         sep=""), 
                                   pattern = "within_ci_finaltp_sampled_list_lambda2",
                                   full.names = TRUE,
                                   recursive = FALSE)
  
  ciwm_list_lambda[i] <- list.files(paste(dir_names[i], 
                               sep=""), 
                             pattern = "ci_width_sampled_mean_lambda2",
                             full.names = TRUE,
                             recursive = FALSE)
  
  ciw_list_lambda[i] <- list.files(paste(dir_names[i], 
                                  sep=""), 
                            pattern = "ci_width_sampled_list_lambda2",
                            full.names = TRUE,
                            recursive = FALSE)
  
  tdm_list_lr[i] <- list.files(paste(dir_names[i], 
                                  sep=""), 
                            pattern = "trend_dev_sampled_mean_lr",
                            full.names = TRUE,
                            recursive = FALSE)
  
  td_list_lr[i] <- list.files(paste(dir_names[i], 
                                 sep=""), 
                           pattern = "trend_dev_sampled_list_lr",
                           full.names = TRUE,
                           recursive = FALSE)
  
  wcim_list_lr[i] <- list.files(paste(dir_names[i], 
                                   sep=""), 
                             pattern = "within_ci_sampled_mean_lr",
                             full.names = TRUE,
                             recursive = FALSE)
  
  wci_list_lr[i] <- list.files(paste(dir_names[i], 
                                  sep=""), 
                            pattern = "within_ci_sampled_list_lr",
                            full.names = TRUE,
                            recursive = FALSE)
  
  wcim_ftp_list_lr[i] <- list.files(paste(dir_names[i], 
                                          sep=""), 
                                    pattern = "within_ci_finaltp_sampled_mean_lr",
                                    full.names = TRUE,
                                    recursive = FALSE)
  
  wci_ftp_list_lr[i] <- list.files(paste(dir_names[i], 
                                         sep=""), 
                                   pattern = "within_ci_finaltp_sampled_list_lr",
                                   full.names = TRUE,
                                   recursive = FALSE)
  
  ciwm_list_lr[i] <- list.files(paste(dir_names[i], 
                                   sep=""), 
                             pattern = "ci_width_sampled_mean_lr",
                             full.names = TRUE,
                             recursive = FALSE)
  
  ciw_list_lr[i] <- list.files(paste(dir_names[i], 
                                  sep=""), 
                            pattern = "ci_width_sampled_list_lr",
                            full.names = TRUE,
                            recursive = FALSE)
  
  tsd_list[i] <- list.files(paste(dir_names[i],
                               sep=""),
                            pattern = "culled.",
                            full.names = TRUE,
                            recursive = FALSE)
  
  tscd_list[i] <- list.files(paste(dir_names[i],
                                sep=""),
                             pattern = "completed_time_series",
                             full.names = TRUE,
                             recursive = FALSE)
  
  samp_list[i] <- list.files(paste(dir_names[i],
                                sep=""),
                             pattern = "sample_pop_id_list",
                             full.names = TRUE,
                             recursive = FALSE)

}

# copy files to new directory
file.copy(c(info_list,
            gr_samples_list,
            wci_list_lambda,
            wcim_list_lambda,
            wci_ftp_list_lambda,
            wcim_ftp_list_lambda,
            ciw_list_lambda,
            ciwm_list_lambda,
            td_list_lambda,
            tdm_list_lambda,
            wci_list_lr,
            wcim_list_lr,
            wci_ftp_list_lr,
            wcim_ftp_list_lr,
            ciw_list_lr,
            ciwm_list_lr,
            td_list_lr,
            tdm_list_lr,
            tsd_list,
            tscd_list,
            samp_list), 
          dir_name2,
          overwrite = TRUE)

# reset previously used vectors to empty again
info_list <- vector()
gr_samples_list <- vector()
td_list_lambda <- vector()
tdm_list_lambda <- vector()
wci_list_lambda <- vector()
wcim_list_lambda <- vector()
wci_ftp_list_lambda <- vector()
wcim_ftp_list_lambda <- vector()
ciw_list_lambda <- vector()
ciwm_list_lambda <- vector()
td_list_lr <- vector()
tdm_list_lr <- vector()
wci_list_lr <- vector()
wcim_list_lr <- vector()
wci_ftp_list_lr <- vector()
wcim_ftp_list_lr <- vector()
ciw_list_lr <- vector()
ciwm_list_lr <- vector()
tsd_list <- vector()
tscd_list <- vector()
samp_list <- vector()

# get lists of csv files in the test directory
info_list <- list.files(paste(dir_name2, 
                              sep=""), 
                        pattern = "info",
                        full.names = TRUE,
                        recursive = FALSE)

gr_samples_list <- list.files(paste(dir_name2, 
                              sep=""), 
                              pattern = "gr_samples",
                              full.names = TRUE,
                              recursive = FALSE)

tdm_list_lambda <- list.files(paste(dir_name2, 
                            sep=""), 
                      pattern = "trend_dev_sampled_mean_lambda2",
                      full.names = TRUE,
                      recursive = FALSE)

td_list_lambda <- list.files(paste(dir_name2, 
                            sep=""), 
                      pattern = "trend_dev_sampled_list_lambda2",
                      full.names = TRUE,
                      recursive = FALSE)

wcim_list_lambda <- list.files(paste(dir_name2, 
                             sep=""), 
                       pattern = "within_ci_sampled_mean_lambda2",
                       full.names = TRUE,
                       recursive = FALSE)

wci_list_lambda <- list.files(paste(dir_name2, 
                             sep=""), 
                       pattern = "within_ci_sampled_list_lambda2",
                       full.names = TRUE,
                       recursive = FALSE)

wcim_ftp_list_lambda <- list.files(paste(dir_name2, 
                                     sep=""), 
                               pattern = "within_ci_finaltp_sampled_mean_lambda2",
                               full.names = TRUE,
                               recursive = FALSE)

wci_ftp_list_lambda <- list.files(paste(dir_name2, 
                                    sep=""), 
                              pattern = "within_ci_finaltp_sampled_list_lambda2",
                              full.names = TRUE,
                              recursive = FALSE)

ciwm_list_lambda <- list.files(paste(dir_name2, 
                             sep=""), 
                       pattern = "ci_width_sampled_mean_lambda2",
                       full.names = TRUE,
                       recursive = FALSE)

ciw_list_lambda <- list.files(paste(dir_name2, 
                             sep=""), 
                       pattern = "ci_width_sampled_list_lambda2",
                       full.names = TRUE,
                       recursive = FALSE)

tdm_list_lr <- list.files(paste(dir_name2, 
                             sep=""), 
                       pattern = "trend_dev_sampled_mean_lr",
                       full.names = TRUE,
                       recursive = FALSE)

td_list_lr <- list.files(paste(dir_name2, 
                            sep=""), 
                      pattern = "trend_dev_sampled_list_lr",
                      full.names = TRUE,
                      recursive = FALSE)

wcim_list_lr <- list.files(paste(dir_name2, 
                              sep=""), 
                        pattern = "within_ci_sampled_mean_lr",
                        full.names = TRUE,
                        recursive = FALSE)

wci_list_lr <- list.files(paste(dir_name2, 
                             sep=""), 
                       pattern = "within_ci_sampled_list_lr",
                       full.names = TRUE,
                       recursive = FALSE)

wcim_ftp_list_lr <- list.files(paste(dir_name2, 
                                 sep=""), 
                           pattern = "within_ci_finaltp_sampled_mean_lr",
                           full.names = TRUE,
                           recursive = FALSE)

wci_ftp_list_lr <- list.files(paste(dir_name2, 
                                sep=""), 
                          pattern = "within_ci_finaltp_sampled_list_lr",
                          full.names = TRUE,
                          recursive = FALSE)

ciwm_list_lr <- list.files(paste(dir_name2, 
                              sep=""), 
                        pattern = "ci_width_sampled_mean_lr",
                        full.names = TRUE,
                        recursive = FALSE)

ciw_list_lr <- list.files(paste(dir_name2, 
                             sep=""), 
                       pattern = "ci_width_sampled_list_lr",
                       full.names = TRUE,
                       recursive = FALSE)


tsd_list <- list.files(paste(dir_name2,
                             sep=""),
                       pattern = "culled",
                       full.names = TRUE,
                       recursive = FALSE)

tscd_list <- list.files(paste(dir_name2,
                             sep=""),
                       pattern = "completed_time_series",
                       full.names = TRUE,
                       recursive = FALSE)

samp_list <- list.files(paste(dir_name2,
                              sep=""),
                        pattern = "sample_pop_id_list",
                        full.names = TRUE,
                        recursive = FALSE)

# create a set of temporary lists and vectors to hold various data from saved files
trenddev_tl_lambda <- list()
trenddev_m_tl_lambda <- list()
withinci_tl_lambda <- list()
withinci_m_tl_lambda <- list()
withinci_ftp_tl_lambda <- list()
withinci_m_ftp_tl_lambda <- list()
ciwidth_tl_lambda <- list()
ciwidth_m_tl_lambda <- list()
trenddev_tl_lr <- list()
trenddev_m_tl_lr <- list()
withinci_tl_lr <- list()
withinci_m_tl_lr <- list()
withinci_ftp_tl_lr <- list()
withinci_m_ftp_tl_lr <- list()
ciwidth_tl_lr <- list()
ciwidth_m_tl_lr <- list()
samp_sdgr_tl <- list()
samp_meangr_tl <- list()
samp_meansd_tl <- list()
samp_meants_tl <- list()
samp_sdgr_m_tl <- list()
samp_meangr_m_tl <- list()
samp_meansd_m_tl <- list()
samp_meants_m_tl <- list()
sampspecsize_tl <- list()
sampspecsize_m_tl <- vector()
tyears_tl <- vector()
tpops_tl <- vector()
boots_tl <- vector()
sampsize_tl <- vector()
samppercent_tl <- vector()
popspec_tl <- vector()
sdgr_tl <- vector()
meangr_tl <- vector()
meansd_tl <- vector()
sdgrdeg_tl <- vector()
meangrdeg_tl <- vector()
meansddeg_tl <- vector()
iternum_tl <- vector()
tsgenver_tl <- vector()
tspec_tl <- vector()
meanobs_tl <- vector()
meanlength_tl <- vector()
degrade_type_tl <- vector()
meancv_tl <- vector()

# get data from files
for (i in 1:length(info_list)) {
  
  # import data from csv files
  info <- read.csv(info_list[i])
  gr_samples <- read.csv(gr_samples_list[i])
  trenddev_lambda <- read.csv(td_list_lambda[i])
  trenddev_m_lambda <- read.csv(tdm_list_lambda[i])
  withinci_lambda <- read.csv(wci_list_lambda[i])
  withinci_m_lambda <- read.csv(wcim_list_lambda[i])
  withinci_ftp_lambda <- read.csv(wci_ftp_list_lambda[i])
  withinci_m_ftp_lambda <- read.csv(wcim_ftp_list_lambda[i])
  ciwidth_lambda <- read.csv(ciw_list_lambda[i])
  ciwidth_m_lambda <- read.csv(ciwm_list_lambda[i])
  trenddev_lr <- read.csv(td_list_lr[i])
  trenddev_m_lr <- read.csv(tdm_list_lr[i])
  withinci_lr <- read.csv(wci_list_lr[i])
  withinci_m_lr <- read.csv(wcim_list_lr[i])
  withinci_ftp_lr <- read.csv(wci_ftp_list_lr[i])
  withinci_m_ftp_lr <- read.csv(wcim_ftp_list_lr[i])
  ciwidth_lr <- read.csv(ciw_list_lr[i])
  ciwidth_m_lr <- read.csv(ciwm_list_lr[i])
  tsdata <- readRDS(tsd_list[i])
  tscdata <- readRDS(tscd_list[i])
  sampdata <- readRDS(samp_list[i])
  
  # read data into lists
  iternum_tl[i] <- info$ID[1]
  tsgenver_tl[i] <- info$synth_version[1]
  tyears_tl[i] <- info$num_years[1]
  meancv_tl[i] <- info$mean_cv[1]
  meangr_tl[i] <- info$mean_gr_raw[1]
  sdgr_tl[i] <- info$gr_sd_raw[1]
  meansd_tl[i] <- info$mean_sd_raw[1]
  meangrdeg_tl[i] <- info$mean_gr_degraded[1]
  sdgrdeg_tl[i] <- info$gr_sd_degraded[1]
  meansddeg_tl[i] <- info$mean_sd_degraded[1]
  trenddev_tl_lambda[[i]] <- trenddev_lambda$MSI
  trenddev_m_tl_lambda[[i]] <- trenddev_m_lambda[[2]]
  withinci_tl_lambda[[i]] <- withinci_lambda$MSI
  withinci_m_tl_lambda[[i]] <- withinci_m_lambda[[2]]
  withinci_ftp_tl_lambda[[i]] <- withinci_ftp_lambda$MSI
  withinci_m_ftp_tl_lambda[[i]] <- withinci_m_ftp_lambda[[2]]
  ciwidth_tl_lambda[[i]] <- ciwidth_lambda$MSI
  ciwidth_m_tl_lambda[[i]] <- ciwidth_m_lambda[[2]]
  trenddev_tl_lr[[i]] <- trenddev_lr$MSI
  trenddev_m_tl_lr[[i]] <- trenddev_m_lr[[2]]
  withinci_tl_lr[[i]] <- withinci_lr$MSI
  withinci_m_tl_lr[[i]] <- withinci_m_lr[[2]]
  withinci_ftp_tl_lr[[i]] <- withinci_ftp_lr$MSI
  withinci_m_ftp_tl_lr[[i]] <- withinci_m_ftp_lr[[2]]
  ciwidth_tl_lr[[i]] <- ciwidth_lr$MSI
  ciwidth_m_tl_lr[[i]] <- ciwidth_m_lr[[2]]
  samp_sdgr_tl[[i]] <- gr_samples$gr_sd_sample
  samp_meangr_tl[[i]] <- gr_samples$mean_gr_sample
  samp_meansd_tl[[i]] <- gr_samples$mean_sd_sample
  samp_meants_tl[[i]] <- gr_samples$mean_ts_length
  samp_sdgr_m_tl[[i]] <- mean(gr_samples$gr_sd_sample, na.rm=TRUE)
  samp_meangr_m_tl[[i]] <- mean(gr_samples$mean_gr_sample, na.rm=TRUE)
  samp_meansd_m_tl[[i]] <- mean(gr_samples$mean_sd_sample, na.rm=TRUE)
  samp_meants_m_tl[[i]] <- mean(gr_samples$mean_ts_length, na.rm=TRUE)
  boots_tl[i] <- length(trenddev_tl_lambda[[i]])
  tpops_tl[i] <- nrow(tsdata)
  tspec_tl[i] <- length(unique(tsdata$SpecID))
  popspec_tl[i] <- round((tpops_tl[i] / tspec_tl[i]), digits = 1)
  sampsize_tl[i] <- length(sampdata[[1]])
  samppercent_tl[i] <- (sampsize_tl[i] / tpops_tl[i]) * 100
  meanobs_tl[i] <- sum(!is.na(as.vector(tsdata[,1:tyears_tl[i]]))) / tpops_tl[i]
 # meanlength_tl[i] <- sum(!is.na(as.vector(tscdata[,1:tyears_tl[i]]))) / tpops_tl[i]
  meanlength_tl[i] <- info$mean_ts_length[1]
  
  sampspecsize_temp <- vector()
  for (j in 1:boots_tl[i]) {
    
    sampspecsize_temp[j] <- length(unique(tsdata$SpecID[tsdata$PopID %in% sampdata[[j]]]))
    
  }
  sampspecsize_tl[[i]] <- sampspecsize_temp
  sampspecsize_m_tl[i] <- mean(sampspecsize_temp, na.rm=TRUE)


}

 ## create data frame to hold results
test_results <- data.frame(matrix(NA, ncol = 30, nrow = length(info_list)*20))
test_results_m <- data.frame(matrix(NA, ncol = 30, nrow = length(info_list)))

# name columns
colnames(test_results) <- c("ID",
                            "MeanGR",
                            "SDGR",
                            "MeanSD",
                            "MeanGRDeg",
                            "SDGRDeg",
                            "MeanSDDeg",
                            "MeanGRSamp",
                            "SDGRSamp",
                            "MeanSDSamp",
                            "MeanTSSamp",
                            "MeanCV",
                            "SampSize",
                            "SampPercent",
                            "SampSpecSize",
                            "MeanTSLength",
                            "MeanNumObs",
                            "TotalPops",
                            "TotalSpec",
                            "PopSpec",
                            "TotalYears",
                            "TSGenVersion",
                            "TrendDevLambda",
                            "WithinCILambda",
                            "WithinCIFTPLambda",
                            "CIWidthLambda",
                            "TrendDevLR",
                            "WithinCILR",
                            "WithinCIFTPLR",
                            "CIWidthLR")

colnames(test_results_m) <- c("ID",
                            "MeanGR",
                            "SDGR",
                            "MeanSD",
                            "MeanGRDeg",
                            "SDGRDeg",
                            "MeanSDDeg",
                            "MeanGRSamp",
                            "SDGRSamp",
                            "MeanSDSamp",
                            "MeanTSSamp",
                            "MeanCV",
                            "SampSize",
                            "SampPercent",
                            "SampSpecSize",
                            "MeanTSLength",
                            "MeanNumObs",
                            "TotalPops",
                            "TotalSpec",
                            "PopSpec",
                            "TotalYears",
                            "TSGenVersion",
                            "TrendDevLambda",
                            "WithinCILambda",
                            "WithinCIFTPLambda",
                            "CIWidthLambda",
                            "TrendDevLR",
                            "WithinCILR",
                            "WithinCIFTPLR",
                            "CIWidthLR")

counter <- 1
counter_m <- 1

for (i in 1:length(info_list)) {
  
  for (j in 1:boots_tl[i]) {
    
    test_results$ID[counter] <- paste(iternum_tl[i], j, sep="_")
    test_results$MeanGR[counter] <- meangr_tl[i]
    test_results$SDGR[counter] <- sdgr_tl[i]
    test_results$MeanSD[counter] <- meansd_tl[i]
    test_results$MeanGRDeg[counter] <- meangrdeg_tl[i]
    test_results$SDGRDeg[counter] <- sdgrdeg_tl[i]
    test_results$MeanSDDeg[counter] <- meansddeg_tl[i]
    test_results$MeanGRSamp[counter] <- samp_meangr_tl[[i]][j]
    test_results$SDGRSamp[counter] <- samp_sdgr_tl[[i]][j]
    test_results$MeanSDSamp[counter] <- samp_meansd_tl[[i]][j]
    test_results$MeanTSSamp[counter] <- samp_meants_tl[[i]][j]
    test_results$MeanCV[counter] <- meancv_tl[i]
    test_results$SampSize[counter] <- sampsize_tl[i]
    test_results$SampPercent[counter] <- samppercent_tl[i]
    test_results$SampSpecSize[counter] <- sampspecsize_tl[[i]][j]
    test_results$MeanTSLength[counter] <- meanlength_tl[i]
    test_results$MeanNumObs[counter] <- meanobs_tl[i]
    test_results$TotalPops[counter] <- tpops_tl[i]
    test_results$TotalSpec[counter] <- tspec_tl[i]
    test_results$PopSpec[counter] <- popspec_tl[i]
    test_results$TotalYears[counter] <- tyears_tl[i]
    test_results$TSGenVersion[counter] <- tsgenver_tl[i]
    test_results$TrendDevLambda[counter] <- trenddev_tl_lambda[[i]][j]
    test_results$WithinCILambda[counter] <- withinci_tl_lambda[[i]][j]
    test_results$WithinCIFTPLambda[counter] <- withinci_ftp_tl_lambda[[i]][j]
    test_results$CIWidthLambda[counter] <- ciwidth_tl_lambda[[i]][j]
    test_results$TrendDevLR[counter] <- trenddev_tl_lr[[i]][j]
    test_results$WithinCILR[counter] <- withinci_tl_lr[[i]][j]
    test_results$WithinCIFTPLR[counter] <- withinci_ftp_tl_lr[[i]][j]
    test_results$CIWidthLR[counter] <- ciwidth_tl_lr[[i]][j]

    counter <- counter + 1
    
  }
  
  test_results_m$ID[counter_m] <- iternum_tl[i]
  test_results_m$MeanGR[counter_m] <- meangr_tl[i]
  test_results_m$SDGR[counter_m] <- sdgr_tl[i]
  test_results_m$MeanSD[counter_m] <- meansd_tl[i]
  test_results_m$MeanGRDeg[counter_m] <- meangrdeg_tl[i]
  test_results_m$SDGRDeg[counter_m] <- sdgrdeg_tl[i]
  test_results_m$MeanSDDeg[counter_m] <- meansddeg_tl[i]
  test_results_m$MeanGRSamp[counter_m] <- samp_meangr_m_tl[[i]]
  test_results_m$SDGRSamp[counter_m] <- samp_sdgr_m_tl[[i]]
  test_results_m$MeanSDSamp[counter_m] <- samp_meansd_m_tl[[i]]
  test_results_m$MeanTSSamp[counter_m] <- samp_meants_m_tl[[i]]
  test_results_m$MeanCV[counter_m] <- meancv_tl[i]
  test_results_m$SampSize[counter_m] <- sampsize_tl[i]
  test_results_m$SampPercent[counter_m] <- samppercent_tl[i]
  test_results_m$SampSpecSize[counter_m] <- sampspecsize_m_tl[i]
  test_results_m$MeanTSLength[counter_m] <- meanlength_tl[i]
  test_results_m$MeanNumObs[counter_m] <- meanobs_tl[i]
  test_results_m$TotalPops[counter_m] <- tpops_tl[i]
  test_results_m$TotalSpec[counter_m] <- tspec_tl[i]
  test_results_m$PopSpec[counter_m] <- popspec_tl[i]
  test_results_m$TotalYears[counter_m] <- tyears_tl[i]
  test_results_m$TSGenVersion[counter_m] <- tsgenver_tl[i]
  test_results_m$TrendDevLambda[counter_m] <- trenddev_m_tl_lambda[[i]]
  test_results_m$WithinCILambda[counter_m] <- withinci_m_tl_lambda[[i]]
  test_results_m$WithinCIFTPLambda[counter_m] <- withinci_m_ftp_tl_lambda[[i]]
  test_results_m$CIWidthLambda[counter_m] <- ciwidth_m_tl_lambda[[i]]
  test_results_m$TrendDevLR[counter_m] <- trenddev_m_tl_lr[[i]]
  test_results_m$WithinCILR[counter_m] <- withinci_m_tl_lr[[i]]
  test_results_m$WithinCIFTPLR[counter_m] <- withinci_m_ftp_tl_lr[[i]]
  test_results_m$CIWidthLR[counter_m] <- ciwidth_m_tl_lr[[i]]

  counter_m <- counter_m + 1
  
}

#saveRDS(test_results, file="test_results.RData")
#saveRDS(test_results_m, file="test_results_m.RData")

# remove datasets outside LPD parameter range
# test_results_mc <- test_results_m[test_results_m$SDGRSamp < 0.63 
#                              & test_results_m$SDGRSamp > 0.12 
#                              & test_results_m$MeanSDSamp > 0.16 
#                              & test_results_m$MeanSDSamp < 0.89
#                              & test_results_m$MeanGRSamp > -0.19
#                              & test_results_m$MeanGRSamp < 0.16
#                              & test_results_m$MeanTSLength > 6.0
#                              & test_results_m$MeanTSLength < 39,]
# 
# saveRDS(test_results_mc, file="test_results_mc.RData")


##############################
library(tidyr)

test_res2 <- pivot_longer(test_results_m, cols=c(TrendDevLR, TrendDevLambda), names_to = "Method", values_to = "TrendDev")
test_res3 <- pivot_longer(test_res2, cols=c(WithinCILR, WithinCILambda), names_to = "Method2", values_to = "WithinCI")
test_res4 <- pivot_longer(test_res3, cols=c(CIWidthLR, CIWidthLambda), names_to = "Method3", values_to = "CIWidth")
test_res4.5 <- pivot_longer(test_res4, cols=c(WithinCIFTPLR, WithinCIFTPLambda), names_to = "Method4", values_to = "WithinCIFTP")
test_res5 <- test_res4.5[(test_res4.5$Method2=="WithinCILR" 
                        & test_res4.5$Method=="TrendDevLR" 
                        & test_res4.5$Method3=="CIWidthLR"
                        & test_res4.5$Method4=="WithinCIFTPLR") 
                       | (test_res4.5$Method2=="WithinCILambda" 
                          & test_res4.5$Method=="TrendDevLambda" 
                          & test_res4.5$Method3=="CIWidthLambda"
                          & test_res4.5$Method4=="WithinCIFTPLambda"),]

test_res5$MeanTSLength <- c(rep(c(10,20,30,40), each=32), rep(20, each=1056))
test_res5$MeanTSSamp <- c(rep(c(10,20,30,40), each=32), rep(20, each=1056))

test_res5$PDWidth <- NA
test_res5$PDWithin <- NA
for (i in unique(test_res5$ID)) {
  temp <- test_res5$CIWidth[test_res5$ID==i & test_res5$Method3=="CIWidthLR"]
  temp2 <- test_res5$CIWidth[test_res5$ID==i & test_res5$Method3=="CIWidthLambda"]
  #test_res5$PDWidth[test_res5$ID==i] <- (abs(temp - temp2) / ((temp + temp2) / 2)) * 100
  test_res5$PDWidth[test_res5$ID==i] <- ((temp - temp2) / temp2) * 100
  
  temp3 <- test_res5$WithinCI[test_res5$ID==i & test_res5$Method2=="WithinCILR"]
  temp4 <- test_res5$WithinCI[test_res5$ID==i & test_res5$Method2=="WithinCILambda"]
  #test_res5$PDWithin[test_res5$ID==i] <- (abs(temp3 - temp4) / ((temp3 + temp4) / 2)) * 100
  test_res5$PDWithin[test_res5$ID==i] <- ((temp3 - temp4) / temp4) * 100
  
}
test_res5$PDRatio <- test_res5$PDWithin - test_res5$PDWidth

saveRDS(test_res5, file="analysis_data_prepared.RData")
##########################################


p1 <- ggplot(test_res5[test_res5$ID<10100,], aes(x=as.factor(MeanTSLength), y=WithinCI, fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  ylab("")+
  xlab("Mean Time Series Length")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############

p2 <- ggplot(test_res5[test_res5$ID>10116 & test_res5$ID<10200,], aes(x=as.factor(SampSize), y=WithinCI, fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  ylab("")+
  xlab("Sample Size")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############

test_res6 <- test_res5 %>%
  mutate(PopSpec = cut(PopSpec, 10))

p3 <- ggplot(test_res6[test_res6$ID>11000,], aes(x=as.factor(PopSpec), y=WithinCI, fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  scale_x_discrete(labels=c("10", "20", "50", "100"))+
  ylab("% of 'true' trend within sample C.I.")+
  xlab("Populations Per Species")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############

test_res7 <- test_res5 %>%
  mutate(MeanGR = cut(MeanGR, 5))

p4 <- ggplot(test_res7[test_res7$ID>10300 & test_res7$ID<10400,], aes(x=factor(MeanGR), y=WithinCI, fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  scale_x_discrete(labels=c("-0.08", "-0.04", "0", "0.04", "0.08"))+
  ylab("")+
  xlab("Mean Intrinsic Growth Rate (r)")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############

test_res8 <- test_res5 %>%
  mutate(SDGR = cut(SDGR, 5))

p5 <- ggplot(test_res8[test_res8$ID>10400 & test_res8$ID<10500,], aes(x=factor(SDGR), y=WithinCI, fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  scale_x_discrete(labels=c("0.05", "0.15", "0.25", "0.35", "0.45"))+
  ylab("")+
  xlab("Standard Deviation in Mean Growth Rate (r)")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############

test_res9 <- test_res5 %>%
  mutate(MeanSD = cut(MeanSD, 5))

p6 <- ggplot(test_res9[test_res9$ID>10500 & test_res9$ID<10600,], aes(x=factor(MeanSD), y=WithinCI, fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  scale_x_discrete(labels=c("0.1", "0.3", "0.5", "0.7", "0.9"))+
  ylab("")+
  xlab("Mean of the Standard Deviation")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############
library(ggpubr)
ggarrange(p1, p2, p3, p4, p5, p6, ncol=2, nrow=3, common.legend=TRUE, legend="bottom")

ggsave("bar_plots_withinci.tiff",
       plot = last_plot(),
       device = tiff,
       dpi = 1000,
       height = 8000,
       width = 8000,
       units = "px",
       compression = "lzw")

############

##########################################


p1 <- ggplot(test_res5[test_res5$ID<10100,], aes(x=as.factor(MeanTSLength), y=TrendDev, fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  ylab("")+
  xlab("Mean Time Series Length")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############

p2 <- ggplot(test_res5[test_res5$ID>10116 & test_res5$ID<10200,], aes(x=as.factor(SampSize), y=TrendDev, fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  ylab("")+
  xlab("Sample Size")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############

test_res6 <- test_res5 %>%
  mutate(PopSpec = cut(PopSpec, 10))

p3 <- ggplot(test_res6[test_res6$ID>11000,], aes(x=as.factor(PopSpec), y=TrendDev, fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  ylab("Trend Deviation Value")+
  xlab("Populations Per Species")+
  scale_x_discrete(labels=c("10", "20", "50", "100"))+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############

test_res7 <- test_res5 %>%
  mutate(MeanGR = cut(MeanGR, 5))

p4 <- ggplot(test_res7[test_res7$ID>10300 & test_res7$ID<10400,], aes(x=factor(MeanGR), y=TrendDev, fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  scale_x_discrete(labels=c("-0.08", "-0.04", "0", "0.04", "0.08"))+
  ylab("")+
  xlab("Mean Intrinsic Growth Rate (r)")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############

test_res8 <- test_res5 %>%
  mutate(SDGR = cut(SDGR, 5))

p5 <- ggplot(test_res8[test_res8$ID>10400 & test_res8$ID<10500,], aes(x=factor(SDGR), y=TrendDev, fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  scale_x_discrete(labels=c("0.05", "0.15", "0.25", "0.35", "0.45"))+
  ylab("")+
  xlab("Standard Deviation in Mean Growth Rate (r)")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############

test_res9 <- test_res5 %>%
  mutate(MeanSD = cut(MeanSD, 5))

p6 <- ggplot(test_res9[test_res9$ID>10500 & test_res9$ID<10600,], aes(x=factor(MeanSD), y=TrendDev, fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  scale_x_discrete(labels=c("0.1", "0.3", "0.5", "0.7", "0.9"))+
  ylab("")+
  xlab("Mean of the Standard Deviation")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############
library(ggpubr)
ggarrange(p1, p2, p3, p4, p5, p6, ncol=2, nrow=3, common.legend=TRUE, legend="bottom")

ggsave("bar_plots_tdv.tiff",
       plot = last_plot(),
       device = tiff,
       dpi = 1000,
       height = 8000,
       width = 8000,
       units = "px",
       compression = "lzw")

############

p7 <- ggplot(test_res5[test_res5$ID>10600,], aes(x=factor(MeanCV), y=PDRatio, fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  scale_x_discrete(labels=c("0.1", "0.3", "0.5", "0.7", "0.9"))+
  ylab("")+
  xlab("Mean Coefficient of Variation")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

p7

# ggsave("solutions.tiff",
#        device = tiff,
#        dpi = 1000,
#        compression = "lzw")

########################

##########################################


p1 <- ggplot(test_res5[test_res5$ID<10100,], aes(x=as.factor(MeanTSLength), y=log(CIWidth), fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  ylab("")+
  xlab("Mean Time Series Length")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############

p2 <- ggplot(test_res5[test_res5$ID>10116 & test_res5$ID<10200,], aes(x=as.factor(SampSize), y=log(CIWidth), fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  ylab("")+
  xlab("Sample Size")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############

test_res6 <- test_res5 %>%
  mutate(PopSpec = cut(PopSpec, 10))

p3 <- ggplot(test_res6[test_res6$ID>11000,], aes(x=as.factor(PopSpec), y=log(CIWidth), fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  ylab("natural log of C.I. width")+
  xlab("Populations Per Species")+
  scale_x_discrete(labels=c("10", "20", "50", "100"))+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############

test_res7 <- test_res5 %>%
  mutate(MeanGR = cut(MeanGR, 5))

p4 <- ggplot(test_res7[test_res7$ID>10300 & test_res7$ID<10400,], aes(x=factor(MeanGR), y=log(CIWidth), fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  scale_x_discrete(labels=c("-0.08", "-0.04", "0", "0.04", "0.08"))+
  ylab("")+
  xlab("Mean Intrinsic Growth Rate (r)")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############

test_res8 <- test_res5 %>%
  mutate(SDGR = cut(SDGR, 5))

p5 <- ggplot(test_res8[test_res8$ID>10400 & test_res8$ID<10500,], aes(x=factor(SDGR), y=log(CIWidth), fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  scale_x_discrete(labels=c("0.05", "0.15", "0.25", "0.35", "0.45"))+
  ylab("")+
  xlab("Standard Deviation in Mean Growth Rate (r)")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############

test_res9 <- test_res5 %>%
  mutate(MeanSD = cut(MeanSD, 5))

p6 <- ggplot(test_res9[test_res9$ID>10500 & test_res9$ID<10600,], aes(x=factor(MeanSD), y=log(CIWidth), fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  scale_x_discrete(labels=c("0.1", "0.3", "0.5", "0.7", "0.9"))+
  ylab("")+
  xlab("Mean of the Standard Deviation")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############
library(ggpubr)
ggarrange(p1, p2, p3, p4, p5, p6, ncol=2, nrow=3, common.legend=TRUE, legend="bottom")

ggsave("bar_plots_ciwidth.tiff",
       plot = last_plot(),
       device = tiff,
       dpi = 1000,
       height = 8000,
       width = 8000,
       units = "px",
       compression = "lzw")

############


ggplot(test_res5[test_res5$ID>10200 & test_res5$ID<10300,], aes(x=as.factor(PopSpec), y=MeanGR, fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  ylab("")+
  xlab("Populations Per Species")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)



##########################################


p1 <- ggplot(test_res5[test_res5$ID<10100,], aes(x=as.factor(MeanTSLength), y=1-WithinCIFTP, fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  ylab("")+
  xlab("Mean Time Series Length")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############

p2 <- ggplot(test_res5[test_res5$ID>10116 & test_res5$ID<10200,], aes(x=as.factor(SampSize), y=1-WithinCIFTP, fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  ylab("")+
  xlab("Sample Size")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############

test_res6 <- test_res5 %>%
  mutate(PopSpec = cut(PopSpec, 10))

p3 <- ggplot(test_res6[test_res6$ID>11000,], aes(x=as.factor(PopSpec), y=1-WithinCIFTP, fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  scale_x_discrete(labels=c("10", "20", "50", "100"))+
  ylab("% endpoints of 'true' trend within sample C.I.")+
  xlab("Populations Per Species")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############

test_res7 <- test_res5 %>%
  mutate(MeanGR = cut(MeanGR, 5))

p4 <- ggplot(test_res7[test_res7$ID>10300 & test_res7$ID<10400,], aes(x=factor(MeanGR), y=1-WithinCIFTP, fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  scale_x_discrete(labels=c("-0.08", "-0.04", "0", "0.04", "0.08"))+
  ylab("")+
  xlab("Mean Intrinsic Growth Rate (r)")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############

test_res8 <- test_res5 %>%
  mutate(SDGR = cut(SDGR, 5))

p5 <- ggplot(test_res8[test_res8$ID>10400 & test_res8$ID<10500,], aes(x=factor(SDGR), y=1-WithinCIFTP, fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  scale_x_discrete(labels=c("0.05", "0.15", "0.25", "0.35", "0.45"))+
  ylab("")+
  xlab("Standard Deviation in Mean Growth Rate (r)")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############

test_res9 <- test_res5 %>%
  mutate(MeanSD = cut(MeanSD, 5))

p6 <- ggplot(test_res9[test_res9$ID>10500 & test_res9$ID<10600,], aes(x=factor(MeanSD), y=1-WithinCIFTP, fill=Method2))+
  geom_boxplot(show.legend=TRUE, width=0.5)+
  scale_x_discrete(labels=c("0.1", "0.3", "0.5", "0.7", "0.9"))+
  ylab("")+
  xlab("Mean of the Standard Deviation")+
  scale_fill_manual(labels=c("Bootstrap Species Indices", "Rank Envelope"), values=c("orange", "sky blue"))+
  labs(fill = "")+
  theme_classic()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_rect(fill="white"),
        legend.title.align = 0.5)

############
library(ggpubr)
ggarrange(p1, p2, p3, p4, p5, p6, ncol=2, nrow=3, common.legend=TRUE, legend="bottom")

ggsave("bar_plots_withinciftp.tiff",
       plot = last_plot(),
       device = tiff,
       dpi = 1000,
       height = 8000,
       width = 8000,
       units = "px",
       compression = "lzw")

############