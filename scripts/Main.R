### admin ###
#
# Shawn Dove
# May 17, 2021
#
#

## load packages ----

library(plyr)
library(ggplot2)
library(dplyr)
library(mgcv)
library(GET)
library(MASS)
library(reshape2)
library(matrixStats)
library(foreach)
library(doSNOW)
library(TSdist)
library(philentropy)

## set parameters ----

n <- 100 # number of GAM resamples per population
n_boot <- 3000 # number of index resamples for rank envelope method
bootstrap_size <- 20 # number of samples
boots <- 3000 # number of bootstrap resamples for confidence intervals for lpi method
count_thres <- 3 # threshold at which number of counts is too low and we should not include this population
min_ts_length <- 3 # minimum length of time series to be included
tmax <- 50 # number of years (data points per population)
tpops <- 1000 # total number of populations
#popspec <- 100 # mean number of populations per species
#ngrps <- 10 # number of groups to divide species into
c <- tmax # number of years or columns (same as tmax)
m_colnames <- 1:c # column names

mean_cv <- 0.3
cv_sd <- 0.2
pgrowthx <- 7

## Setup Testing ----

#1#
iter_num <- 20000
gr_mean_a <- 0
gr_sd_vec_a <- 0.2
sd_mean <- 0.2
samp_size_ <- 200
mlength_ <- rep(c(7, 10, 15, 20, 25, 33, 42), each=12)
numobs_ <- ceiling(0.5*mlength_)
popspec <- 20

no_cores <- 8 # the number of cores to be used for parallel processing
cl <- makeCluster(no_cores, outfile="TestData/output.txt") # create cluster for parallel processing
registerDoSNOW(cl) # register the cluster
clusterEvalQ(cl, c(library(tcltk),  # send necessary functions to the cluster
                   library(dplyr), 
                   library(MASS), 
                   library(GET), 
                   library(mgcv), 
                   library(reshape2), 
                   library(matrixStats),
                   library(TSdist)))

# load the main function into memory
source("scripts/Main_Function.R")

# call the main function
foreach(i = 1:96) %dopar% {  # loop for parallel processing
  all_fn(popvar = gr_sd_vec_a, # variance in mean growth rate
         popmean = gr_mean_a, # mean growth rate
         sdmean =  sd_mean, # mean of standard deviations in growth rates
         pgrowthx = 7, # which time series generator to use
         iter_num = (iter_num+i), 
         tmax = tmax, # number of years
         tpops = tpops, # total number of time series
         popspec = popspec, # mean number of populations per species
         n = n, # number of GAM resamples 
         n_boot = n_boot, # number of index bootstraps for each species
         boots = boots, # number of bootstrap resamples for confidence intervals for lpi method
         count_thres = count_thres, # minimum number of population counts
         min_ts_length = min_ts_length, # minimum time series length
         c = c, # number of columns (years: same as tmax)
         samp_size = samp_size_, # number of time series in each sample
         m_colnames = m_colnames, # column names
         mlength = mlength_[i], # mean length of time series 
         numobs = numobs_[i], # mean number of observations in each time series
         bootstrap_size = bootstrap_size, # number of samples
         error = TRUE,  # add observation error
         mean_cv = mean_cv, # mean coefficient of variation for observation error
         cv_sd = cv_sd)# standard deviation of coefficient of variation for observation error
}

stopCluster(cl) # stop the cluster

#2#
iter_num <- 20100
gr_mean_a <- 0
gr_sd_vec_a <- 0.2
sd_mean <- 0.2
samp_size_ <- rep(c(50, 70, 100, 150, 200, 300, 500, 800), each=12)
mlength_ <- 20
numobs_ <- ceiling(0.5*mlength_)
popspec <- 20

no_cores <- 8 # the number of cores to be used for parallel processing
cl <- makeCluster(no_cores, outfile="TestData/output.txt") # create cluster for parallel processing
registerDoSNOW(cl) # register the cluster
clusterEvalQ(cl, c(library(tcltk),  # send necessary functions to the cluster
                   library(dplyr), 
                   library(MASS), 
                   library(GET), 
                   library(mgcv), 
                   library(reshape2), 
                   library(matrixStats),
                   library(TSdist)))

# load the main function into memory
source("scripts/Main_Function.R")

# call the main function
foreach(i = 1:96) %dopar% {  # loop for parallel processing
  all_fn(popvar = gr_sd_vec_a, # variance in mean growth rate
         popmean = gr_mean_a, # mean growth rate
         sdmean =  sd_mean, # mean of standard deviations in growth rates
         pgrowthx = 7, # which time series generator to use
         iter_num = (iter_num+i), 
         tmax = tmax, # number of years
         tpops = tpops, # total number of time series
         popspec = popspec, # mean number of populations per species
         n = n, # number of GAM resamples 
         n_boot = n_boot, # number of index bootstraps for each species
         boots = boots, # number of bootstrap resamples for confidence intervals for lpi method
         count_thres = count_thres, # minimum number of population counts
         min_ts_length = min_ts_length, # minimum time series length
         c = c, # number of columns (years: same as tmax)
         samp_size = samp_size_[i], # number of time series in each sample
         m_colnames = m_colnames, # column names
         mlength = mlength_, # mean length of time series 
         numobs = numobs_, # mean number of observations in each time series
         bootstrap_size = bootstrap_size, # number of samples
         error = TRUE,  # add observation error
         mean_cv = mean_cv, # mean coefficient of variation for observation error
         cv_sd = cv_sd)# standard deviation of coefficient of variation for observation error
}

stopCluster(cl) # stop the cluster

#3#
iter_num <- 20200
gr_mean_a <- 0
gr_sd_vec_a <- 0.2
sd_mean <- 0.2
samp_size_ <- 200
mlength_ <- 20
numobs_ <- ceiling(0.5*mlength_)
popspec <- rep(c(5, 10, 15, 20, 30, 50, 100, 200), each=12)

no_cores <- 8 # the number of cores to be used for parallel processing
cl <- makeCluster(no_cores, outfile="TestData/output.txt") # create cluster for parallel processing
registerDoSNOW(cl) # register the cluster
clusterEvalQ(cl, c(library(tcltk),  # send necessary functions to the cluster
                   library(dplyr), 
                   library(MASS), 
                   library(GET), 
                   library(mgcv), 
                   library(reshape2), 
                   library(matrixStats),
                   library(TSdist)))

# load the main function into memory
source("scripts/Main_Function.R")

# call the main function
foreach(i = 1:96) %dopar% {  # loop for parallel processing
  all_fn(popvar = gr_sd_vec_a, # variance in mean growth rate
         popmean = gr_mean_a, # mean growth rate
         sdmean =  sd_mean, # mean of standard deviations in growth rates
         pgrowthx = 7, # which time series generator to use
         iter_num = (iter_num+i), 
         tmax = tmax, # number of years
         tpops = tpops, # total number of time series
         popspec = popspec[i], # mean number of populations per species
         n = n, # number of GAM resamples 
         n_boot = n_boot, # number of index bootstraps for each species
         boots = boots, # number of bootstrap resamples for confidence intervals for lpi method
         count_thres = count_thres, # minimum number of population counts
         min_ts_length = min_ts_length, # minimum time series length
         c = c, # number of columns (years: same as tmax)
         samp_size = samp_size_, # number of time series in each sample
         m_colnames = m_colnames, # column names
         mlength = mlength_, # mean length of time series 
         numobs = numobs_, # mean number of observations in each time series
         bootstrap_size = bootstrap_size, # number of samples
         error = TRUE,  # add observation error
         mean_cv = mean_cv, # mean coefficient of variation for observation error
         cv_sd = cv_sd)# standard deviation of coefficient of variation for observation error
}

stopCluster(cl) # stop the cluster

#4#
iter_num <- 20300
gr_mean_a <- rep(c(-0.08, -0.04, -0.02, 0, 0.02, 0.04, 0.08), each=14)
gr_sd_vec_a <- 0.2
sd_mean <- 0.2
samp_size_ <- 200
mlength_ <- 20
numobs_ <- ceiling(0.5*mlength_)
popspec <- 20

no_cores <- 8 # the number of cores to be used for parallel processing
cl <- makeCluster(no_cores, outfile="TestData/output.txt") # create cluster for parallel processing
registerDoSNOW(cl) # register the cluster
clusterEvalQ(cl, c(library(tcltk),  # send necessary functions to the cluster
                   library(dplyr), 
                   library(MASS), 
                   library(GET), 
                   library(mgcv), 
                   library(reshape2), 
                   library(matrixStats),
                   library(TSdist)))

# load the main function into memory
source("scripts/Main_Function.R")

# call the main function
foreach(i = 1:98) %dopar% {  # loop for parallel processing
  all_fn(popvar = gr_sd_vec_a, # variance in mean growth rate
         popmean = gr_mean_a[i], # mean growth rate
         sdmean =  sd_mean, # mean of standard deviations in growth rates
         pgrowthx = 7, # which time series generator to use
         iter_num = (iter_num+i), 
         tmax = tmax, # number of years
         tpops = tpops, # total number of time series
         popspec = popspec, # mean number of populations per species
         n = n, # number of GAM resamples 
         n_boot = n_boot, # number of index bootstraps for each species
         boots = boots, # number of bootstrap resamples for confidence intervals for lpi method
         count_thres = count_thres, # minimum number of population counts
         min_ts_length = min_ts_length, # minimum time series length
         c = c, # number of columns (years: same as tmax)
         samp_size = samp_size_, # number of time series in each sample
         m_colnames = m_colnames, # column names
         mlength = mlength_, # mean length of time series 
         numobs = numobs_, # mean number of observations in each time series
         bootstrap_size = bootstrap_size, # number of samples
         error = TRUE,  # add observation error
         mean_cv = mean_cv, # mean coefficient of variation for observation error
         cv_sd = cv_sd)# standard deviation of coefficient of variation for observation error
}

stopCluster(cl) # stop the cluster

#5#
iter_num <- 20400
gr_mean_a <- 0
gr_sd_vec_a <- rep(c(0.05, 0.15, 0.25, 0.4, 0.55, 0.6, 0.8, 1), each=12)
sd_mean <- 0.2
samp_size_ <- 200
mlength_ <- 20
numobs_ <- ceiling(0.5*mlength_)
popspec <- 20

no_cores <- 8 # the number of cores to be used for parallel processing
cl <- makeCluster(no_cores, outfile="TestData/output.txt") # create cluster for parallel processing
registerDoSNOW(cl) # register the cluster
clusterEvalQ(cl, c(library(tcltk),  # send necessary functions to the cluster
                   library(dplyr), 
                   library(MASS), 
                   library(GET), 
                   library(mgcv), 
                   library(reshape2), 
                   library(matrixStats),
                   library(TSdist)))

# load the main function into memory
source("scripts/Main_Function.R")

# call the main function
foreach(i = 1:96) %dopar% {  # loop for parallel processing
  all_fn(popvar = gr_sd_vec_a[i], # variance in mean growth rate
         popmean = gr_mean_a, # mean growth rate
         sdmean =  sd_mean, # mean of standard deviations in growth rates
         pgrowthx = 7, # which time series generator to use
         iter_num = (iter_num+i), 
         tmax = tmax, # number of years
         tpops = tpops, # total number of time series
         popspec = popspec, # mean number of populations per species
         n = n, # number of GAM resamples 
         n_boot = n_boot, # number of index bootstraps for each species
         boots = boots, # number of bootstrap resamples for confidence intervals for lpi method
         count_thres = count_thres, # minimum number of population counts
         min_ts_length = min_ts_length, # minimum time series length
         c = c, # number of columns (years: same as tmax)
         samp_size = samp_size_, # number of time series in each sample
         m_colnames = m_colnames, # column names
         mlength = mlength_, # mean length of time series 
         numobs = numobs_, # mean number of observations in each time series
         bootstrap_size = bootstrap_size, # number of samples
         error = TRUE,  # add observation error
         mean_cv = mean_cv, # mean coefficient of variation for observation error
         cv_sd = cv_sd)# standard deviation of coefficient of variation for observation error
}

stopCluster(cl) # stop the cluster

#6#
iter_num <- 20500
gr_mean_a <- 0
gr_sd_vec_a <- 0.2
sd_mean <- rep(c(0.1, 0.2, 0.3, 0.4, 0.55, 0.7, 0.85, 1), each=12)
samp_size_ <- 200
mlength_ <- 20
numobs_ <- ceiling(0.5*mlength_)
popspec <- 20

no_cores <- 8 # the number of cores to be used for parallel processing
cl <- makeCluster(no_cores, outfile="TestData/output.txt") # create cluster for parallel processing
registerDoSNOW(cl) # register the cluster
clusterEvalQ(cl, c(library(tcltk),  # send necessary functions to the cluster
                   library(dplyr), 
                   library(MASS), 
                   library(GET), 
                   library(mgcv), 
                   library(reshape2), 
                   library(matrixStats),
                   library(TSdist)))

# load the main function into memory
source("scripts/Main_Function.R")

# call the main function
foreach(i = 1:96) %dopar% {  # loop for parallel processing
  all_fn(popvar = gr_sd_vec_a, # variance in mean growth rate
         popmean = gr_mean_a, # mean growth rate
         sdmean =  sd_mean[i], # mean of standard deviations in growth rates
         pgrowthx = 7, # which time series generator to use
         iter_num = (iter_num+i), 
         tmax = tmax, # number of years
         tpops = tpops, # total number of time series
         popspec = popspec, # mean number of populations per species
         n = n, # number of GAM resamples 
         n_boot = n_boot, # number of index bootstraps for each species
         boots = boots, # number of bootstrap resamples for confidence intervals for lpi method
         count_thres = count_thres, # minimum number of population counts
         min_ts_length = min_ts_length, # minimum time series length
         c = c, # number of columns (years: same as tmax)
         samp_size = samp_size_, # number of time series in each sample
         m_colnames = m_colnames, # column names
         mlength = mlength_, # mean length of time series 
         numobs = numobs_, # mean number of observations in each time series
         bootstrap_size = bootstrap_size, # number of samples
         error = TRUE,  # add observation error
         mean_cv = mean_cv, # mean coefficient of variation for observation error
         cv_sd = cv_sd)# standard deviation of coefficient of variation for observation error
}

stopCluster(cl) # stop the cluster

#7#
iter_num <- 20800
gr_mean_a <- 0
gr_sd_vec_a <- 0.2
sd_mean <- 0.2
samp_size_ <- 200
mlength_ <- 20
numobs_ <- ceiling(0.5*mlength_)
popspec <- 20

#mean_cv_ <- rep(0.05, each=8)
#cv_sd <- rep(0.05, each=8)
mean_cv_ <- rep(c(0.05, 0.25, 0.45, 0.65, 0.85, 1, 2, 4), each=20)
cv_sd <- rep(c(0.05, 0.25, 0.45, 0.65, 0.85, 1, 2, 4), each=20)

no_cores <- 8 # the number of cores to be used for parallel processing
cl <- makeCluster(no_cores, outfile="TestData/output.txt") # create cluster for parallel processing
registerDoSNOW(cl) # register the cluster
clusterEvalQ(cl, c(library(tcltk),  # send necessary functions to the cluster
                   library(dplyr), 
                   library(MASS), 
                   library(GET), 
                   library(mgcv), 
                   library(reshape2), 
                   library(matrixStats),
                   library(TSdist)))

# load the main function into memory
source("scripts/Main_Function.R")

# call the main function
foreach(i = 1:160) %dopar% {  # loop for parallel processing
  all_fn(popvar = gr_sd_vec_a, # variance in mean growth rate
         popmean = gr_mean_a, # mean growth rate
         sdmean =  sd_mean, # mean of standard deviations in growth rates
         pgrowthx = 7, # which time series generator to use
         iter_num = (iter_num+i), 
         tmax = tmax, # number of years
         tpops = tpops, # total number of time series
         popspec = popspec, # mean number of populations per species
         n = n, # number of GAM resamples 
         n_boot = n_boot, # number of index bootstraps for each species
         boots = boots, # number of bootstrap resamples for confidence intervals for lpi method
         count_thres = count_thres, # minimum number of population counts
         min_ts_length = min_ts_length, # minimum time series length
         c = c, # number of columns (years: same as tmax)
         samp_size = samp_size_, # number of time series in each sample
         m_colnames = m_colnames, # column names
         mlength = mlength_, # mean length of time series 
         numobs = numobs_, # mean number of observations in each time series
         bootstrap_size = bootstrap_size, # number of samples
         error = TRUE,  # add observation error
         mean_cv = mean_cv_[i], # mean coefficient of variation for observation error
         cv_sd = cv_sd[i])# standard deviation of coefficient of variation for observation error
}

stopCluster(cl) # stop the cluster

#8# Process Error (manipulate sd_mean with zero observation error)
iter_num <- 20700
gr_mean_a <- 0
gr_sd_vec_a <- 0.2
sd_mean <- rep(c(0.1, 0.2, 0.3, 0.4, 0.55, 0.7, 0.85, 1), each=12)
#sd_mean <- rep(0.3, each=12)
samp_size_ <- 200
mlength_ <- 20
numobs_ <- ceiling(0.5*mlength_)
popspec <- 20
mean_cv <- 0
cv_sd <- 0

no_cores <- 8 # the number of cores to be used for parallel processing
cl <- makeCluster(no_cores, outfile="TestData/output.txt") # create cluster for parallel processing
registerDoSNOW(cl) # register the cluster
clusterEvalQ(cl, c(library(tcltk),  # send necessary functions to the cluster
                   library(dplyr), 
                   library(MASS), 
                   library(GET), 
                   library(mgcv), 
                   library(reshape2), 
                   library(matrixStats),
                   library(TSdist)))

# load the main function into memory
source("scripts/Main_Function.R")

# call the main function
foreach(i = 1:96) %dopar% {  # loop for parallel processing
  all_fn(popvar = gr_sd_vec_a, # variance in mean growth rate
         popmean = gr_mean_a, # mean growth rate
         sdmean =  sd_mean[i], # mean of standard deviations in growth rates
         pgrowthx = 7, # which time series generator to use
         iter_num = (iter_num+i), 
         tmax = tmax, # number of years
         tpops = tpops, # total number of time series
         popspec = popspec, # mean number of populations per species
         n = n, # number of GAM resamples 
         n_boot = n_boot, # number of index bootstraps for each species
         boots = boots, # number of bootstrap resamples for confidence intervals for lpi method
         count_thres = count_thres, # minimum number of population counts
         min_ts_length = min_ts_length, # minimum time series length
         c = c, # number of columns (years: same as tmax)
         samp_size = samp_size_, # number of time series in each sample
         m_colnames = m_colnames, # column names
         mlength = mlength_, # mean length of time series 
         numobs = numobs_, # mean number of observations in each time series
         bootstrap_size = bootstrap_size, # number of samples
         error = TRUE,  # add observation error
         mean_cv = mean_cv, # mean coefficient of variation for observation error
         cv_sd = cv_sd)# standard deviation of coefficient of variation for observation error
}

stopCluster(cl) # stop the cluster


############

# iter_num <- 1000
# gr_mean_a <- runif(3000, min = -0.08, max = 0.08)
# gr_sd_vec_a <- runif(3000, min = 0.05, max = 0.5)
# sd_mean <- runif(3000, min = 0.05, max = 2)
# mlength_ <- round(runif(3000, min = 6, max = 40))
# numobs_ <- ceiling(0.5*mlength_)
# samp_size_ <- round(exp(runif(3000, min = log(50), max = log(10000))))
# tmax <- 50
# c <- tmax
# m_colnames <- 1:c
# mean_cv <- 0.15
# cv_sd <- 0.1
# pgrowthx <- 7
# 
# no_cores <- 8 # the number of cores to be used for parallel processing
# cl <- makeCluster(no_cores, outfile="TestData/output.txt") # create cluster for parallel processing
# registerDoSNOW(cl) # register the cluster
# clusterEvalQ(cl, c(library(tcltk),  # send necessary functions to the cluster
#                    library(dplyr), 
#                    library(MASS), 
#                    library(GET), 
#                    library(mgcv), 
#                    library(reshape2), 
#                    library(matrixStats),
#                    library(TSdist)))
# 
# # load the main function into memory
# source("scripts/Main_Function.R")
# 
# # call the main function
# foreach(i = 1:64) %dopar% {  # loop for parallel processing
#   all_fn(popvar = gr_sd_vec_a, # variance in mean growth rate
#          popmean = gr_mean_a, # mean growth rate
#          sdmean =  sd_mean, # mean of standard deviations in growth rates
#          pgrowthx = 7, # which time series generator to use
#          iter_num = (iter_num+i), 
#          tmax = tmax, # number of years
#          tpops = tpops, # total number of time series
#          popspec = popspec, # mean number of populations per species
#          n = n, # number of GAM resamples 
#          n_boot = n_boot, # number of index bootstraps for each species
#          boots = boots, # number of bootstrap resamples for confidence intervals for lpi method
#          count_thres = count_thres, # minimum number of population counts
#          min_ts_length = min_ts_length, # minimum time series length
#          c = c, # number of columns (years: same as tmax)
#          samp_size = samp_size_, # number of time series in each sample
#          m_colnames = m_colnames, # column names
#          mlength = mlength_, # mean length of time series 
#          numobs = numobs_, # mean number of observations in each time series
#          bootstrap_size = bootstrap_size, # number of samples
#          error = TRUE,  # add observation error
#          mean_cv = mean_cv, # mean coefficient of variation for observation error
#          cv_sd = cv_sd)# standard deviation of coefficient of variation for observation error
# }
# 
# stopCluster(cl) # stop the cluster
