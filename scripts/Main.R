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
n_boot <- 3000 # number of index bootstraps per species
bootstrap_size <- 20 # number of samples
count_thres <- 3 # threshold at which number of counts is too low and we should not include this population
min_ts_length <- 3 # minimum length of time series to be included
tmax <- 50 # number of years (data points per population)
tpops <- 10000 # total number of populations
popspec <- 100 # mean number of populations per species
#ngrps <- 10 # number of groups to divide species into
c <- tmax # number of years or columns (same as tmax)
m_colnames <- 1:c # column names

mean_cv <- 0.15
cv_sd <- 0.1
pgrowthx <- 7

## Setup Testing ----

#1#
iter_num <- 10000
gr_mean_a <- 0
gr_sd_vec_a <- 0.2
sd_mean <- 0.2
samp_size_ <- 500
mlength_ <- rep(c(10, 20, 30, 40), each=16)
numobs_ <- ceiling(0.5*mlength_)
popspec <- 50

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
foreach(i = 1:64) %dopar% {  # loop for parallel processing
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
         ngrps = ngrps, # number of groups to divide time series into
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
iter_num <- 10100
gr_mean_a <- 0
gr_sd_vec_a <- 0.2
sd_mean <- 0.2
samp_size_ <- rep(c(100, 200, 500, 1000, 2000), each=16)
mlength_ <- 20
numobs_ <- ceiling(0.5*mlength_)
popspec <- 50

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
foreach(i = 1:80) %dopar% {  # loop for parallel processing
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
         ngrps = ngrps, # number of groups to divide time series into
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
iter_num <- 11000
gr_mean_a <- 0
gr_sd_vec_a <- 0.2
sd_mean <- 0.2
samp_size_ <- 500
mlength_ <- 20
numobs_ <- ceiling(0.5*mlength_)
popspec <- rep(c(10, 20, 50, 100), each=16)

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
foreach(i = 1:64) %dopar% {  # loop for parallel processing
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
         ngrps = ngrps, # number of groups to divide time series into
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
iter_num <- 10300
gr_mean_a <- rep(c(-0.08, -0.04, 0, 0.04, 0.08), each=16)
gr_sd_vec_a <- 0.2
sd_mean <- 0.2
samp_size_ <- 500
mlength_ <- 20
numobs_ <- ceiling(0.5*mlength_)
popspec <- 50

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
foreach(i = 1:80) %dopar% {  # loop for parallel processing
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
         ngrps = ngrps, # number of groups to divide time series into
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
iter_num <- 10400
gr_mean_a <- 0
gr_sd_vec_a <- rep(c(0.05, 0.15, 0.25, 0.35, 0.45), each=16)
sd_mean <- 0.2
samp_size_ <- 500
mlength_ <- 20
numobs_ <- ceiling(0.5*mlength_)
popspec <- 50

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
foreach(i = 1:80) %dopar% {  # loop for parallel processing
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
         ngrps = ngrps, # number of groups to divide time series into
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
iter_num <- 10500
gr_mean_a <- 0
gr_sd_vec_a <- 0.2
sd_mean <- rep(c(0.1, 0.3, 0.5, 0.7, 0.9), each=16)
samp_size_ <- 500
mlength_ <- 20
numobs_ <- ceiling(0.5*mlength_)
popspec <- 50

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
foreach(i = 1:80) %dopar% {  # loop for parallel processing
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
         ngrps = ngrps, # number of groups to divide time series into
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
iter_num <- 10600
gr_mean_a <- 0
gr_sd_vec_a <- 0.2
sd_mean <- 0.2
samp_size_ <- 500
mlength_ <- 20
numobs_ <- ceiling(0.5*mlength_)
popspec <- 50

mean_cv_ <- rep(c(0.05, 0.25, 0.45, 0.65, 0.85), each=16)
cv_sd <- 0.1

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
foreach(i = 9:80) %dopar% {  # loop for parallel processing
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
         ngrps = ngrps, # number of groups to divide time series into
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
foreach(i = 1:64) %dopar% {  # loop for parallel processing
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
         ngrps = ngrps, # number of groups to divide time series into
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
