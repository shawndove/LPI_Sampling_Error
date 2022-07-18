library(rlpi)
library(dplyr)
library(mgcv)
library(matrixStats)
library(ggplot2)
library(gridExtra)

# load functions ----
source("scripts/AggTrend_Function.R")
source("scripts/Cull_Function.R")
source("scripts/GAM_Functions.R")
source("scripts/SI_Functions.R")
source("scripts/CI_Functions.R")
source("scripts/GI_Functions.R")

# create groupings ----

tax_group<- list(Birds=c("Aves"), 
                 Mammals=c("Mammalia"), 
                 Herps=c("Reptilia", "Amphibia"), 
                 Fish=c("Actinopteri", "Elasmobranchii", "Petromyzonti", "Dipneusti", "Holocephali", "Myxini", "Coelacanthi"))

# create an ID key for taxonomic groups
tax_group_key <- lapply(1:length(tax_group), function(i) {
  Group <- names(tax_group[i])
  Class <- tax_group[[i]]
  ID <- i
  c <- data.frame(Group, Class, ID)
  return(c)})

tax_group_key <- do.call(rbind, tax_group_key) # bind list into a data frame

tax_group_IDs <- setNames(tax_group_key$ID, tax_group_key$Class) # convert to vector with named elements

t_realm <- list(Afrotropical=c("Afrotropical"),
                IndoPacific=c("Australasia", "Oceania", "Indo-Malayan"),
                Palearctic=c("Palearctic"),
                Neotropical=c("Neotropical"),
                Nearctic=c("Nearctic"),
                Antarctic=c("Antarctic"))

# create an ID key for terrestrial realms
t_realm_key <- lapply(1:length(t_realm), function(i) {
  Group <- names(t_realm[i])
  Class <- t_realm[[i]]
  ID <- i
  c <- data.frame(Group, Class, ID)
  return(c)})
t_realm_key <- do.call(rbind, t_realm_key) # bind list into a data frame

t_realm_IDs <- setNames(t_realm_key$ID, t_realm_key$Class) # convert to vector with named elements

fw_realm <- list(Afrotropical=c("Afrotropical"),
                 IndoPacific=c("Australasia", "Oceania", "Indo-Malayan"),
                 Palearctic=c("Palearctic"),
                 Neotropical=c("Neotropical"),
                 Nearctic=c("Nearctic"),
                 Antarctic=c("Antarctic"))

# create an ID key for freshwater realms
fw_realm_key <- lapply(1:length(fw_realm), function(i) {
  Group <- names(fw_realm[i])
  Class <- fw_realm[[i]]
  ID <- i
  c <- data.frame(Group, Class, ID)
  return(c)})
fw_realm_key <- do.call(rbind, fw_realm_key) # bind list into a data frame

fw_realm_IDs <- setNames(fw_realm_key$ID, fw_realm_key$Class) + length(unique(t_realm_IDs)) # convert to vector with named elements

m_realm <- list(M_Atlantic_NT=c("Atlantic north temperate"),
                Atlantic_TS=c("Atlantic tropical and subtropical"),
                Arctic=c("Arctic"),
                South_TA=c("South temperate and Antarctic"),
                Tropical_SI=c("Tropical and subtropical Indo-Pacific"),
                Pacific_NT=c("Pacific north temperate"))

# create an ID key for marine realms
m_realm_key <- lapply(1:length(m_realm), function(i) {
  Group <- names(m_realm[i])
  Class <- m_realm[[i]]
  ID <- i
  c <- data.frame(Group, Class, ID)
  return(c)})
m_realm_key <- do.call(rbind, m_realm_key) # bind list into a data frame

m_realm_IDs <- setNames(m_realm_key$ID, m_realm_key$Class)  + length(unique(t_realm_IDs)) + length(unique(fw_realm_IDs)) # convert to vector with named elements

sys_grp <- list(Terrest=c("Terrestrial"),
                FW=c("Freshwater"),
                Marine=c("Marine"))

# create an ID key for marine realms
sys_key <- lapply(1:length(sys_grp), function(i) {
  Group <- names(sys_grp[i])
  Class <- sys_grp[[i]]
  ID <- i
  c <- data.frame(Group, Class, ID)
  return(c)})
sys_key <- do.call(rbind, sys_key) # bind list into a data frame

sys_IDs <- setNames(sys_key$ID, sys_key$Class) # convert to vector with named elements

# create list of taxonomic groups for index list
tax_list <- rep(unique(tax_group_IDs), length(unique(m_realm_IDs)) + length(unique(t_realm_IDs)) + length(unique(fw_realm_IDs)))

# create list of realms for index list
realm_list <- rep(c(unique(t_realm_IDs), unique(fw_realm_IDs), unique(m_realm_IDs)), each=length(unique(tax_group_IDs)))

# create list of systems for index list
sys_list <- rep(unique(sys_IDs), each=length(unique(tax_group_IDs)) * length(unique(t_realm_IDs)))

sys_realm_list <- c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3)

# create list of weightings for taxonomic groups within realms (copied from McRae et al., 2017)
Weightings_list <- c(0.387205957, 0.197833813, 0.41496023, NA, 
                     0.396527091, 0.172106825, 0.431366084, NA, 
                     0.433535576, 0.249862107, 0.316602317, NA, 
                     0.387661234, 0.127987201, 0.484351565, NA, 
                     0.376366476, 0.249869859, 0.373763665, NA, 
                     NA, NA, NA, NA, 
                     0.192000, 0.009000, 0.207000, 0.590000, 
                     0.176000, 0.008000, 0.321000, 0.493000, 
                     0.211000, 0.015000, 0.179000, 0.592000, 
                     0.107000, 0.010000, 0.298000, 0.584000, 
                     0.203000, 0.013000, 0.217000, 0.565000, 
                     NA, NA, NA, NA, 
                     0.068635, 0.009774, 0.001303, 0.920286, 
                     0.069353, 0.006224, 0.001630, 0.922791, 
                     0.172867, 0.035011, 0.000000, 0.792123, 
                     0.054261, 0.022342, 0.000957, 0.922438, 
                     0.048714, 0.004878, 0.005505, 0.940901, 
                     0.080916, 0.025257, 0.000935, 0.892890)

# create list of weightings for realms within systems (copied from McRae et al., 2017)
Weightingsr_list <- c(0.189738, 0.292168, 0.116431, 0.321132, 0.061683, NA, 
                      0.211701, 0.225576, 0.123314, 0.365550, 0.060853, NA, 
                      0.146489, 0.214706, 0.014541, 0.099685, 0.456553, 0.068026)

# load public LPI data ----

# all data, including private
LPI_full <- read.csv(file="Data/LPD_output_20201116.csv", sep=",", stringsAsFactors=FALSE)
# fix ID column name
colnames(LPI_full)[1] <- "ID"

# remove X from years in column names
colnames(LPI_full) <- gsub("X", "", colnames(LPI_full))

firstyear <- 1950 # first year of data
startyear <- 1970 # first year of data to use for index
endyear <- 2019 # final year of data
m_colnames <- as.character(startyear:endyear) # index years/column names
m_colnames2 <- as.character(firstyear:endyear) # index years/column names
c <- length(m_colnames) # number of years/columns
c2 <- length(m_colnames2) # number of years/columns

# create subset with only count data
LPI_trimmed <- LPI_full[,which(colnames(LPI_full) %in% (firstyear:endyear))]

# convert to numeric data
LPI_trimmed <- as.data.frame(sapply(LPI_trimmed, as.numeric))

# create a population ID column by assigning each population a separate ID based on its row number
LPI_trimmed$PopID <- LPI_full$ID

# create a species ID column by assigning each unique species a separate ID number
LPI_trimmed$SpecID <- match(LPI_full$Binomial, unique(LPI_full$Binomial))

# create a taxonomic group ID column by assigning species groups separate ID numbers according to LPI groupings
LPI_trimmed$GrpID <- tax_group_IDs[LPI_full$Class]

# create a taxonomic group ID column by assigning terrestrial realms separate ID numbers according to LPI groupings
LPI_trimmed$TRID <- t_realm_IDs[LPI_full$T_realm]

# create a taxonomic group ID column by assigning freshwater realms separate ID numbers according to LPI groupings
LPI_trimmed$FWRID <- fw_realm_IDs[LPI_full$FW_realm]

# create a taxonomic group ID column by assigning marine realms separate ID numbers according to LPI groupings
LPI_trimmed$MRID <- m_realm_IDs[LPI_full$M_realm]

# create a taxonomic group ID column by assigning systems separate ID numbers according to LPI groupings
LPI_trimmed$SysID <- sys_IDs[LPI_full$System]

pop_list <- list()
# select populations to form each group index
for (i in 1:length(tax_list)) {
  
  temp <- LPI_trimmed$PopID[which(LPI_trimmed$SysID==sys_list[i] & 
                                    LPI_trimmed$GrpID==tax_list[i] & 
                                    (LPI_trimmed$TRID==realm_list[i] | 
                                       LPI_trimmed$FWRID==realm_list[i] | 
                                       LPI_trimmed$MRID==realm_list[i]))]
  
  pop_list[[i]] <- temp
  
}

# calculate regional taxonomic group indices and CIs for LPI using rank envelope method
spec.ind.list <- list()
grp.ind.list <- list()
grp.ci.list <- list()
grp.ind.mean.list <- list()
grp.indl.list <- list()
# loop over taxonomic groups
#for (i in 1:length(pop_list)) {
for (i in 63:72) {
  
  if (!any(!is.na(pop_list[[i]]))) {
    
    spec.ind.list[[i]] <- NA
    grp.ind.list[[i]] <- NA
    grp.ind.mean.list[[i]] <- NA
    grp.ci.list[[i]] <- NA
    grp.indl.list[[i]] <- NA
    
    next
    
  }
  
  # select all copies of the populations listed in the sample
  group_data <- LPI_trimmed[LPI_trimmed$PopID %in% pop_list[[i]],]
  
  # remove all populations with less than 3 data points
  group_data_culled <- cull_fn(group_data, 3, 3, c2)
  #group_data_culled <- cull_fn(group_data, 2, 2, c2)
  
  if (!any(!is.na(group_data_culled))) {
    
    spec.ind.list[[i]] <- NA
    grp.ind.list[[i]] <- NA
    grp.ind.mean.list[[i]] <- NA
    grp.ci.list[[i]] <- NA
    grp.indl.list[[i]] <- NA
    
    next
    
  }
  
  #grp_chain1 <- complete_time_series(group_data_culled, c2, m_colnames2, lambda=TRUE)
  #grp_gam2 <- pop_gam_fn(grp_chain1, c2, m_colnames2, n=1000, lambda=TRUE, resample=FALSE, quality=TRUE)
  #grp_gam <- complete_time_series(grp_gam2, c2, m_colnames2, lambda=TRUE)
  #grp_spec <- species_index_fn(grp_gam[,21:length(grp_gam)], c, m_colnames, resample=FALSE, lambda=TRUE)
  #grp_ind <- group_index_fn(grp_spec, c, m_colnames)
  
  # GAM all populations and resample from the GAM n times
  grp_gam <- pop_gam_fn(group_data_culled, c2, m_colnames2, n=1000, lambda=TRUE, resample=TRUE, quality=FALSE)
  
  # create species indices
  grp_spec <- species_index_fn(grp_gam[,21:length(grp_gam)], c, m_colnames, n=1000, n_boot=3000, lambda=TRUE, resample=TRUE)
  
  # add species indices to species indices list
  spec.ind.list[[i]] <- grp_spec
  
  # create index for regional taxonomic group
  grp_ind <- group_index_fn(grp_spec, c, m_colnames, n=1000, n_boot=3000)
  
  # add group index to group indices list
  grp.ind.list[[i]] <- grp_ind
  
  # create index for regional taxonomic group
  grp_indl <- group_index_fn(grp_spec, c, m_colnames, n=1000, n_boot=3000, stay_lambda=TRUE)
  
  # add group index to group indices list
  grp.indl.list[[i]] <- grp_indl
  
  # get group index mean
  grp_ind_mean <- colMeans(grp_ind, na.rm=TRUE)
  
  # add group index mean to list
  grp.ind.mean.list[[i]] <- grp_ind_mean
  
  # create confidence intervals for regional taxonomic group
  grp_ci <- ci_resample(grp_ind, m_colnames)
  
  # add group confidence intervals to group c.i. list
  grp.ci.list[[i]] <- grp_ci

}

saveRDS(grp.ind.list, file="files/grp_ind_list.RData")
saveRDS(grp.indl.list, file="files/grp_indl_list.RData")
saveRDS(grp.ind.mean.list, file="files/grp_ind_mean_list.RData")
saveRDS(grp.ci.list, file="files/grp_ci_list.RData")

grp.indl.list <- readRDS(file="files/grp_indl_list.RData")

# calculate realm indices and CIs for LPI using rank envelope method
realm.ind.list <- list()
realm.ci.list <- list()
realm.ind.mean.list <- list()
realm.indl.list <- list()
# loop over realms
for (i in unique(realm_list)) {
#for (i in 12:18) {
  
  regtaxlist <- which(realm_list==i) # get regional taxonomic group ids
  regtaxweights <- Weightings_list[regtaxlist] # get weights for r.t. groups
  regtaxweights <- regtaxweights * (1 / sum(regtaxweights, na.rm=TRUE)) # adjust weights to add up to 1
  
  rtgrpind <- list()
  counter <- 1
  for (j in regtaxlist) {
    if (is.na(grp.indl.list[[j]])) {
      temp <- grp.indl.list[[j]]
      next
    } 
    temp <- grp.indl.list[[j]] # get regional taxonomic group indices
    temp_weighted <- temp * regtaxweights[counter] # add weighting
    temp_weighted$GrpID <- j # assign regional taxonomic group id
    rtgrpind[[counter]] <- temp_weighted # add to list for realm
    counter <- counter + 1 # increase counter
    
  }
  
  # convert from list to dataframe
  realm_grpind <- do.call(rbind, rtgrpind)
  
  if (is.null(realm_grpind)) {
    
    realm.ind.list[[i]] <- NA
    realm.indl.list[[i]] <- NA
    realm.ind.mean.list[[i]] <- NA
    realm.ci.list[[i]] <- NA
    next
  } 

  # create indices for realm
  realm_ind <- aggregate_index_fn(realm_grpind, c, m_colnames, n=1000, n_boot=3000)
  
  # add realm indices to realm indices list
  realm.ind.list[[i]] <- realm_ind
  
  # create index for regional taxonomic group
  realm_indl <- aggregate_index_fn(realm_grpind, c, m_colnames, n=1000, n_boot=3000, stay_lambda=TRUE)
  
  # add group index to group indices list
  realm.indl.list[[i]] <- realm_indl
  
  # calculate mean of realm indices
  realm_ind_mean <- colMeans(realm_ind, na.rm=TRUE)
  
  # add to list
  realm.ind.mean.list[[i]] <- realm_ind_mean
  
  # create confidence intervals for realms
  realm_ci <- ci_resample(realm_ind, m_colnames)
  
  # add realm confidence intervals to realm c.i. list
  realm.ci.list[[i]] <- realm_ci
  
}

saveRDS(realm.ind.list, file="files/realm_ind_list.RData")
saveRDS(realm.indl.list, file="files/realm_indl_list.RData")
saveRDS(realm.ind.mean.list, file="files/realm_ind_mean_list.RData")
saveRDS(realm.ci.list, file="files/realm_ci_list.RData")

realm.ind.mean.list <- readRDS(file="files/realm_ind_mean_list.RData")

# calculate system indices and CIs for LPI using rank envelope method
sys.ind.list <- list()
sys.ci.list <- list()
sys.ind.mean.list <- list()
sys.indl.list <- list()
# loop over realms
for (i in unique(sys_realm_list)) {
#for (i in 3) { 
  
  rlmlist <- which(sys_realm_list==i) # get realm ids
  rlmweights <- Weightingsr_list[rlmlist] # get realm weights
  rlmweights <- rlmweights * (1 / sum(rlmweights, na.rm=TRUE)) # adjust weights to add up to 1
  
  rlmgrpind <- list()
  counter <- 1
  for (j in rlmlist) {
    if (is.na(realm.indl.list[[j]])) {
      temp <- realm.indl.list[[j]]
      next
    }
    temp <- realm.indl.list[[j]] # get realm indices
    temp_weighted <- temp * rlmweights[counter] # add weighting
    temp_weighted$GrpID <- j # assign realm id
    rlmgrpind[[counter]] <- temp_weighted # add to list for realm
    counter <- counter + 1 # increase counter
    
  }
  
  # convert from list to dataframe
  sys_grpind <- do.call(rbind, rlmgrpind)
  
  # create indices for system
  sys_ind <- aggregate_index_fn(sys_grpind, c, m_colnames, n=1000, n_boot=3000)
  
  # add system indices to system indices list
  sys.ind.list[[i]] <- sys_ind
  
  # create indices for system
  sys_indl <- aggregate_index_fn(sys_grpind, c, m_colnames, n=1000, n_boot=3000, stay_lambda=TRUE)
  
  # add system indices to system indices list
  sys.indl.list[[i]] <- sys_indl
  
  # calculate mean of system indices
  sys_ind_mean <- colMeans(sys_ind, na.rm=TRUE)
  
  # add to list
  sys.ind.mean.list[[i]] <- sys_ind_mean
  
  # create confidence intervals for realms
  sys_ci <- ci_resample(sys_ind, m_colnames)
  
  # add system confidence intervals to system c.i. list
  sys.ci.list[[i]] <- sys_ci
  
}

saveRDS(sys.ind.list, file="files/sys_ind_list.RData")
saveRDS(sys.indl.list, file="files/sys_indl_list.RData")
saveRDS(sys.ind.mean.list, file="files/sys_ind_mean_list.RData")
saveRDS(sys.ci.list, file="files/sys_ci_list.RData")

sys.ind.mean.list <- readRDS(file="files/sys_ind_mean_list.RData")
sys.indl.list <- readRDS(file="files/sys_indl_list.RData")

# calculate final index and CIs for LPI using rank envelope method

sysgrpind <- list()
for (i in 1:3) {
  temp <- sys.indl.list[[i]] # get system indices
  temp_weighted <- temp * (1/3) # add weighting
  temp_weighted$GrpID <- i # assign system id
  sysgrpind[[i]] <- temp_weighted # add to list for system
}

# convert to data frame
final_grpind <- do.call(rbind, sysgrpind)

# create final indices
final_ind_all <- aggregate_index_fn(final_grpind, c, m_colnames, n=1000, n_boot=3000)

# take mean of final indices
final_ind <- colMeans(final_ind_all, na.rm=TRUE)
  
# create confidence intervals for realms
final_ci <- ci_resample(final_ind_all, m_colnames)

# organize data for plotting
final_plot_df <- data.frame(Index = as.vector(final_ind), 
                            CILow = as.vector(as.matrix(lowci)), 
                            CIHigh = as.vector(as.matrix(highci)),
                            Year = as.vector(as.numeric(m_colnames)))

# organize data for plotting
terr_plot_df <- data.frame(Index = as.vector(sys.ind.mean.list[[1]]), 
                            CILow = as.vector(as.matrix(sys.ci.list[[1]][1,])), 
                            CIHigh = as.vector(as.matrix(sys.ci.list[[1]][2,])),
                            Year = as.vector(as.numeric(m_colnames)))# organize data for plotting

fw_plot_df <- data.frame(Index = as.vector(sys.ind.mean.list[[2]]), 
                            CILow = as.vector(as.matrix(sys.ci.list[[2]][1,])), 
                            CIHigh = as.vector(as.matrix(sys.ci.list[[2]][2,])),
                            Year = as.vector(as.numeric(m_colnames)))# organize data for plotting

marine_plot_df <- data.frame(Index = as.vector(sys.ind.mean.list[[3]]), 
                            CILow = as.vector(as.matrix(sys.ci.list[[3]][1,])), 
                            CIHigh = as.vector(as.matrix(sys.ci.list[[3]][2,])),
                            Year = as.vector(as.numeric(m_colnames)))

## plot index in ggplot
final_plot <- ggplot(marine_plot_df, aes(x = Year, y = Index/100))+
  geom_ribbon(aes(ymin = CILow/100, ymax = CIHigh/100), alpha = 0.8, fill = "darkblue")+
  geom_line(size = 0.6, col = "white")+
  geom_hline(yintercept = 1, alpha = 0.8)+
  coord_cartesian(ylim = c(0,2), xlim = NULL)+
  theme_bw()+
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 1))+
  ggtitle("LPI with Rank Envelope Method")+
  ylab("Index (1970 = 1)")+
  scale_y_continuous(trans = "identity", breaks = seq(0, 2, 0.2))
  

######################

LPI_trimmed <- cull_fn(LPI_trimmed, 3, 3, c2)

pop_list <- list()
# select populations to form each group index
for (i in 1:length(tax_list)) {
  
  temp <- LPI_trimmed$PopID[which(LPI_trimmed$SysID==sys_list[i] & 
                                    LPI_trimmed$GrpID==tax_list[i] & 
                                    (LPI_trimmed$TRID==realm_list[i] | 
                                       LPI_trimmed$FWRID==realm_list[i] | 
                                       LPI_trimmed$MRID==realm_list[i]))]
  
  pop_list[[i]] <- temp
  
}

T_Afrotropical_Aves <- LPI_trimmed$PopID %in% pop_list[[1]]
T_Afrotropical_Mammalia <- LPI_trimmed$PopID %in% pop_list[[2]]
T_Afrotropical_Herps <- LPI_trimmed$PopID %in% pop_list[[3]]

T_IndoPacific_Aves <- LPI_trimmed$PopID %in% pop_list[[5]]
T_IndoPacific_Mammalia <- LPI_trimmed$PopID %in% pop_list[[6]]
T_IndoPacific_Herps <- LPI_trimmed$PopID %in% pop_list[[7]]

T_Palearctic_Aves <- LPI_trimmed$PopID %in% pop_list[[9]]
T_Palearctic_Mammalia <- LPI_trimmed$PopID %in% pop_list[[10]]
T_Palearctic_Herps <- LPI_trimmed$PopID %in% pop_list[[11]]

T_Neotropical_Aves <- LPI_trimmed$PopID %in% pop_list[[13]]
T_Neotropical_Mammalia <- LPI_trimmed$PopID %in% pop_list[[14]]
T_Neotropical_Herps <- LPI_trimmed$PopID %in% pop_list[[15]]

T_Nearctic_Aves <- LPI_trimmed$PopID %in% pop_list[[17]]
T_Nearctic_Mammalia <- LPI_trimmed$PopID %in% pop_list[[18]]
T_Nearctic_Herps <- LPI_trimmed$PopID %in% pop_list[[19]]

T_Antarctic_Aves <- LPI_trimmed$PopID %in% pop_list[[21]]
T_Antarctic_Mammalia <- LPI_trimmed$PopID %in% pop_list[[22]]

fw_Afrotropical_Aves <- LPI_trimmed$PopID %in% pop_list[[25]]
fw_Afrotropical_Mammalia <- LPI_trimmed$PopID %in% pop_list[[26]]
fw_Afrotropical_Herps <- LPI_trimmed$PopID %in% pop_list[[27]]
fw_Afrotropical_Fish <- LPI_trimmed$PopID %in% pop_list[[28]]

fw_IndoPacific_Aves <- LPI_trimmed$PopID %in% pop_list[[29]]
fw_IndoPacific_Mammalia <- LPI_trimmed$PopID %in% pop_list[[30]]
fw_IndoPacific_Herps <- LPI_trimmed$PopID %in% pop_list[[31]]
fw_IndoPacific_Fish <- LPI_trimmed$PopID %in% pop_list[[32]]

fw_Palearctic_Aves <- LPI_trimmed$PopID %in% pop_list[[33]]
fw_Palearctic_Mammalia <- LPI_trimmed$PopID %in% pop_list[[34]]
fw_Palearctic_Herps <- LPI_trimmed$PopID %in% pop_list[[35]]
fw_Palearctic_Fish <- LPI_trimmed$PopID %in% pop_list[[36]]

fw_Neotropical_Aves <- LPI_trimmed$PopID %in% pop_list[[37]]
fw_Neotropical_Mammalia <- LPI_trimmed$PopID %in% pop_list[[38]]
fw_Neotropical_Herps <- LPI_trimmed$PopID %in% pop_list[[39]]
fw_Neotropical_Fish <- LPI_trimmed$PopID %in% pop_list[[40]]

fw_Nearctic_Aves <- LPI_trimmed$PopID %in% pop_list[[41]]
fw_Nearctic_Mammalia <- LPI_trimmed$PopID %in% pop_list[[42]]
fw_Nearctic_Herps <- LPI_trimmed$PopID %in% pop_list[[43]]
fw_Nearctic_Fish <- LPI_trimmed$PopID %in% pop_list[[44]]

m_AtNoTemp_Aves <- LPI_trimmed$PopID %in% pop_list[[49]]
m_AtNoTemp_Mammalia <- LPI_trimmed$PopID %in% pop_list[[50]]
m_AtNoTemp_Herps <- LPI_trimmed$PopID %in% pop_list[[51]]
m_AtNoTemp_Fish <- LPI_trimmed$PopID %in% pop_list[[52]]

m_AtTrSub_Aves <- LPI_trimmed$PopID %in% pop_list[[53]]
m_AtTrSub_Mammalia <- LPI_trimmed$PopID %in% pop_list[[54]]
m_AtTrSub_Herps <- LPI_trimmed$PopID %in% pop_list[[55]]
m_AtTrSub_Fish <- LPI_trimmed$PopID %in% pop_list[[56]]

m_Arctic_Aves <- LPI_trimmed$PopID %in% pop_list[[57]]
m_Arctic_Mammalia <- LPI_trimmed$PopID %in% pop_list[[58]]
m_Arctic_Fish <- LPI_trimmed$PopID %in% pop_list[[60]]

m_SoTeAnt_Aves <- LPI_trimmed$PopID %in% pop_list[[61]]
m_SoTeAnt_Mammalia <- LPI_trimmed$PopID %in% pop_list[[62]]
m_SoTeAnt_Herps <- LPI_trimmed$PopID %in% pop_list[[63]]
m_SoTeAnt_Fish <- LPI_trimmed$PopID %in% pop_list[[64]]

m_TroSubIndo_Aves <- LPI_trimmed$PopID %in% pop_list[[65]]
m_TroSubIndo_Mammalia <- LPI_trimmed$PopID %in% pop_list[[66]]
m_TroSubIndo_Herps <- LPI_trimmed$PopID %in% pop_list[[67]]
m_TroSubIndo_Fish <- LPI_trimmed$PopID %in% pop_list[[68]]

m_PaNoTemp_Aves <- LPI_trimmed$PopID %in% pop_list[[69]]
m_PaNoTemp_Mammalia <- LPI_trimmed$PopID %in% pop_list[[70]]
m_PaNoTemp_Herps <- LPI_trimmed$PopID %in% pop_list[[71]]
m_PaNoTemp_Fish <- LPI_trimmed$PopID %in% pop_list[[72]]

LPI_full <- LPI_full[LPI_full$ID %in% LPI_trimmed$PopID,]

# add X back to years in column names (to make create_infile work properly)
colnames(LPI_full)[65:134] <- paste("X", colnames(LPI_full)[65:134], sep="") # full
#colnames(LPI_full)[30:98] <- paste("X", colnames(LPI_full)[30:98], sep="") # public

# create infiles
T_Afrotropical_Aves_infile <- create_infile(LPI_full, index_vector=T_Afrotropical_Aves, name="Infiles/T_Afrotropical_Aves", end_col_name = "X2017")
T_Afrotropical_Mammalia_infile <- create_infile(LPI_full, index_vector=T_Afrotropical_Mammalia, name="Infiles/T_Afrotropical_Mammalia", end_col_name = "X2017")
T_Afrotropical_Herps_infile <- create_infile(LPI_full, index_vector=T_Afrotropical_Herps, name="Infiles/T_Afrotropical_Herps", end_col_name = "X2017")

T_IndoPacific_Aves_infile <- create_infile(LPI_full, index_vector=T_IndoPacific_Aves, name="Infiles/T_IndoPacific_Aves", end_col_name = "X2017")
T_IndoPacific_Mammalia_infile <- create_infile(LPI_full, index_vector=T_IndoPacific_Mammalia, name="Infiles/T_IndoPacific_Mammalia", end_col_name = "X2017")
T_IndoPacific_Herps_infile <- create_infile(LPI_full, index_vector=T_IndoPacific_Herps, name="Infiles/T_IndoPacific_Herps", end_col_name = "X2017")

T_Palearctic_Aves_infile <- create_infile(LPI_full, index_vector=T_Palearctic_Aves, name="Infiles/T_Palearctic_Aves", end_col_name = "X2017")
T_Palearctic_Mammalia_infile <- create_infile(LPI_full, index_vector=T_Palearctic_Mammalia, name="Infiles/T_Palearctic_Mammalia", end_col_name = "X2017")
T_Palearctic_Herps_infile <- create_infile(LPI_full, index_vector=T_Palearctic_Herps, name="Infiles/T_Palearctic_Herps", end_col_name = "X2017")

T_Neotropical_Aves_infile <- create_infile(LPI_full, index_vector=T_Neotropical_Aves, name="Infiles/T_Neotropical_Aves", end_col_name = "X2017")
T_Neotropical_Mammalia_infile <- create_infile(LPI_full, index_vector=T_Neotropical_Mammalia, name="Infiles/T_Neotropical_Mammalia", end_col_name = "X2017")
T_Neotropical_Herps_infile <- create_infile(LPI_full, index_vector=T_Neotropical_Herps, name="Infiles/T_Neotropical_Herps", end_col_name = "X2017")

T_Nearctic_Aves_infile <- create_infile(LPI_full, index_vector=T_Nearctic_Aves, name="Infiles/T_Nearctic_Aves", end_col_name = "X2017")
T_Nearctic_Mammalia_infile <- create_infile(LPI_full, index_vector=T_Nearctic_Mammalia, name="Infiles/T_Nearctic_Mammalia", end_col_name = "X2017")
T_Nearctic_Herps_infile <- create_infile(LPI_full, index_vector=T_Nearctic_Herps, name="Infiles/T_Nearctic_Herps", end_col_name = "X2017")

T_Antarctic_Aves_infile <- create_infile(LPI_full, index_vector=T_Antarctic_Aves, name="T_Antarctic_Aves", end_col_name = "X2017")
T_Antarctic_Mammalia_infile <- create_infile(LPI_full, index_vector=T_Antarctic_Mammalia, name="T_Antarctic_Mammalia", end_col_name = "X2017")

fw_Afrotropical_Aves_infile <- create_infile(LPI_full, index_vector=fw_Afrotropical_Aves, name="Infiles/fw_Afrotropical_Aves", end_col_name = "X2017")
fw_Afrotropical_Mammalia_infile <- create_infile(LPI_full, index_vector=fw_Afrotropical_Mammalia, name="Infiles/fw_Afrotropical_Mammalia", end_col_name = "X2017")
fw_Afrotropical_Herps_infile <- create_infile(LPI_full, index_vector=fw_Afrotropical_Herps, name="Infiles/fw_Afrotropical_Herps", end_col_name = "X2017")
fw_Afrotropical_Fish_infile <- create_infile(LPI_full, index_vector=fw_Afrotropical_Fish, name="Infiles/fw_Afrotropical_Fish", end_col_name = "X2017")

fw_IndoPacific_Aves_infile <- create_infile(LPI_full, index_vector=fw_IndoPacific_Aves, name="Infiles/fw_IndoPacific_Aves", end_col_name = "X2017")
fw_IndoPacific_Mammalia_infile <- create_infile(LPI_full, index_vector=fw_IndoPacific_Mammalia, name="Infiles/fw_IndoPacific_Mammalia", end_col_name = "X2017")
fw_IndoPacific_Herps_infile <- create_infile(LPI_full, index_vector=fw_IndoPacific_Herps, name="Infiles/fw_IndoPacific_Herps", end_col_name = "X2017")
fw_IndoPacific_Fish_infile <- create_infile(LPI_full, index_vector=fw_IndoPacific_Fish, name="Infiles/fw_IndoPacific_Fish", end_col_name = "X2017")

fw_Palearctic_Aves_infile <- create_infile(LPI_full, index_vector=fw_Palearctic_Aves, name="Infiles/fw_Palearctic_Aves", end_col_name = "X2017")
fw_Palearctic_Mammalia_infile <- create_infile(LPI_full, index_vector=fw_Palearctic_Mammalia, name="Infiles/fw_Palearctic_Mammalia", end_col_name = "X2017")
fw_Palearctic_Herps_infile <- create_infile(LPI_full, index_vector=fw_Palearctic_Herps, name="Infiles/fw_Palearctic_Herps", end_col_name = "X2017")
fw_Palearctic_Fish_infile <- create_infile(LPI_full, index_vector=fw_Palearctic_Fish, name="Infiles/fw_Palearctic_Fish", end_col_name = "X2017")

fw_Neotropical_Aves_infile <- create_infile(LPI_full, index_vector=fw_Neotropical_Aves, name="Infiles/fw_Neotropical_Aves", end_col_name = "X2017")
fw_Neotropical_Mammalia_infile <- create_infile(LPI_full, index_vector=fw_Neotropical_Mammalia, name="Infiles/fw_Neotropical_Mammalia", end_col_name = "X2017")
fw_Neotropical_Herps_infile <- create_infile(LPI_full, index_vector=fw_Neotropical_Herps, name="Infiles/fw_Neotropical_Herps", end_col_name = "X2017")
fw_Neotropical_Fish_infile <- create_infile(LPI_full, index_vector=fw_Neotropical_Fish, name="Infiles/fw_Neotropical_Fish", end_col_name = "X2017")

fw_Nearctic_Aves_infile <- create_infile(LPI_full, index_vector=fw_Nearctic_Aves, name="Infiles/fw_Nearctic_Aves", end_col_name = "X2017")
fw_Nearctic_Mammalia_infile <- create_infile(LPI_full, index_vector=fw_Nearctic_Mammalia, name="Infiles/fw_Nearctic_Mammalia", end_col_name = "X2017")
fw_Nearctic_Herps_infile <- create_infile(LPI_full, index_vector=fw_Nearctic_Herps, name="Infiles/fw_Nearctic_Herps", end_col_name = "X2017")
fw_Nearctic_Fish_infile <- create_infile(LPI_full, index_vector=fw_Nearctic_Fish, name="Infiles/fw_Nearctic_Fish", end_col_name = "X2017")

m_AtNoTemp_Aves_infile <- create_infile(LPI_full, index_vector=m_AtNoTemp_Aves, name="Infiles/m_AtNoTemp_Aves", end_col_name = "X2017")
m_AtNoTemp_Mammalia_infile <- create_infile(LPI_full, index_vector=m_AtNoTemp_Mammalia, name="Infiles/m_AtNoTemp_Mammalia", end_col_name = "X2017")
m_AtNoTemp_Herps_infile <- create_infile(LPI_full, index_vector=m_AtNoTemp_Herps, name="Infiles/m_AtNoTemp_Herps", end_col_name = "X2017")
m_AtNoTemp_Fish_infile <- create_infile(LPI_full, index_vector=m_AtNoTemp_Fish, name="Infiles/m_AtNoTemp_Fish", end_col_name = "X2017")

m_AtTrSub_Aves_infile <- create_infile(LPI_full, index_vector=m_AtTrSub_Aves, name="Infiles/m_AtTrSub_Aves", end_col_name = "X2017")
m_AtTrSub_Mammalia_infile <- create_infile(LPI_full, index_vector=m_AtTrSub_Mammalia, name="Infiles/m_AtTrSub_Mammalia", end_col_name = "X2017")
m_AtTrSub_Herps_infile <- create_infile(LPI_full, index_vector=m_AtTrSub_Herps, name="Infiles/m_AtTrSub_Herps", end_col_name = "X2017")
m_AtTrSub_Fish_infile <- create_infile(LPI_full, index_vector=m_AtTrSub_Fish, name="Infiles/m_AtTrSub_Fish", end_col_name = "X2017")

m_Arctic_Aves_infile <- create_infile(LPI_full, index_vector=m_Arctic_Aves, name="Infiles/m_Arctic_Aves", end_col_name = "X2017")
m_Arctic_Mammalia_infile <- create_infile(LPI_full, index_vector=m_Arctic_Mammalia, name="Infiles/m_Arctic_Mammalia", end_col_name = "X2017")
m_Arctic_Fish_infile <- create_infile(LPI_full, index_vector=m_Arctic_Fish, name="Infiles/m_Arctic_Fish", end_col_name = "X2017")

m_SoTeAnt_Aves_infile <- create_infile(LPI_full, index_vector=m_SoTeAnt_Aves, name="Infiles/m_SoTeAnt_Aves", end_col_name = "X2017")
m_SoTeAnt_Mammalia_infile <- create_infile(LPI_full, index_vector=m_SoTeAnt_Mammalia, name="Infiles/m_SoTeAnt_Mammalia", end_col_name = "X2017")
m_SoTeAnt_Herps_infile <- create_infile(LPI_full, index_vector=m_SoTeAnt_Herps, name="Infiles/m_SoTeAnt_Herps", end_col_name = "X2017")
m_SoTeAnt_Fish_infile <- create_infile(LPI_full, index_vector=m_SoTeAnt_Fish, name="Infiles/m_SoTeAnt_Fish", end_col_name = "X2017")

m_TroSubIndo_Aves_infile <- create_infile(LPI_full, index_vector=m_TroSubIndo_Aves, name="Infiles/m_TroSubIndo_Aves", end_col_name = "X2017")
m_TroSubIndo_Mammalia_infile <- create_infile(LPI_full, index_vector=m_TroSubIndo_Mammalia, name="Infiles/m_TroSubIndo_Mammalia", end_col_name = "X2017")
m_TroSubIndo_Herps_infile <- create_infile(LPI_full, index_vector=m_TroSubIndo_Herps, name="Infiles/m_TroSubIndo_Herps", end_col_name = "X2017")
m_TroSubIndo_Fish_infile <- create_infile(LPI_full, index_vector=m_TroSubIndo_Fish, name="Infiles/m_TroSubIndo_Fish", end_col_name = "X2017")

m_PaNoTemp_Aves_infile <- create_infile(LPI_full, index_vector=m_PaNoTemp_Aves, name="Infiles/m_PaNoTemp_Aves", end_col_name = "X2017")
m_PaNoTemp_Mammalia_infile <- create_infile(LPI_full, index_vector=m_PaNoTemp_Mammalia, name="Infiles/m_PaNoTemp_Mammalia", end_col_name = "X2017")
m_PaNoTemp_Herps_infile <- create_infile(LPI_full, index_vector=m_PaNoTemp_Herps, name="Infiles/m_PaNoTemp_Herps", end_col_name = "X2017")
m_PaNoTemp_Fish_infile <- create_infile(LPI_full, index_vector=m_PaNoTemp_Fish, name="Infiles/m_PaNoTemp_Fish", end_col_name = "X2017")



####


# create indices and plot
T_Afrotropical_Aves_lpi  <- LPIMain("Infiles/T_Afrotropical_Aves_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
T_Afrotropical_Mammalia_lpi  <- LPIMain("Infiles/T_Afrotropical_Mammalia_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
T_Afrotropical_Herps_lpi  <- LPIMain("Infiles/T_Afrotropical_Herps_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)

T_IndoPacific_Aves_lpi  <- LPIMain("Infiles/T_IndoPacific_Aves_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
T_IndoPacific_Mammalia_lpi  <- LPIMain("Infiles/T_IndoPacific_Mammalia_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
T_IndoPacific_Herps_lpi  <- LPIMain("Infiles/T_IndoPacific_Herps_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)

T_Palearctic_Aves_lpi  <- LPIMain("Infiles/T_Palearctic_Aves_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
T_Palearctic_Mammalia_lpi  <- LPIMain("Infiles/T_Palearctic_Mammalia_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
T_Palearctic_Herps_lpi  <- LPIMain("Infiles/T_Palearctic_Herps_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)

T_Neotropical_Aves_lpi  <- LPIMain("Infiles/T_Neotropical_Aves_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
T_Neotropical_Mammalia_lpi  <- LPIMain("Infiles/T_Neotropical_Mammalia_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
T_Neotropical_Herps_lpi  <- LPIMain("Infiles/T_Neotropical_Herps_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)

T_Nearctic_Aves_lpi  <- LPIMain("Infiles/T_Nearctic_Aves_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
T_Nearctic_Mammalia_lpi  <- LPIMain("Infiles/T_Nearctic_Mammalia_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
T_Nearctic_Herps_lpi  <- LPIMain("Infiles/T_Nearctic_Herps_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)

T_Antarctic_Aves_lpi <- LPIMain("Infiles/T_Antarctic_Aves_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
T_Antarctic_Mammalia_lpi <- LPIMain("Infiles/T_Antarctic_Mammalia_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)

fw_Afrotropical_Aves_lpi  <- LPIMain("Infiles/fw_Afrotropical_Aves_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
fw_Afrotropical_Mammalia_lpi  <- LPIMain("Infiles/fw_Afrotropical_Mammalia_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
fw_Afrotropical_Herps_lpi  <- LPIMain("Infiles/fw_Afrotropical_Herps_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
fw_Afrotropical_fish_lpi  <- LPIMain("Infiles/fw_Afrotropical_Fish_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)

fw_IndoPacific_Aves_lpi  <- LPIMain("Infiles/fw_IndoPacific_Aves_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
fw_IndoPacific_Mammalia_lpi  <- LPIMain("Infiles/fw_IndoPacific_Mammalia_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
fw_IndoPacific_Herps_lpi  <- LPIMain("Infiles/fw_IndoPacific_Herps_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
fw_IndoPacific_fish_lpi  <- LPIMain("Infiles/fw_IndoPacific_Fish_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)

fw_Palearctic_Aves_lpi  <- LPIMain("Infiles/fw_Palearctic_Aves_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
fw_Palearctic_Mammalia_lpi  <- LPIMain("Infiles/fw_Palearctic_Mammalia_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
fw_Palearctic_Herps_lpi  <- LPIMain("Infiles/fw_Palearctic_Herps_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
fw_Palearctic_fish_lpi  <- LPIMain("Infiles/fw_Palearctic_Fish_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)

fw_Neotropical_Aves_lpi  <- LPIMain("Infiles/fw_Neotropical_Aves_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
fw_Neotropical_Mammalia_lpi  <- LPIMain("Infiles/fw_Neotropical_Mammalia_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
fw_Neotropical_Herps_lpi  <- LPIMain("Infiles/fw_Neotropical_Herps_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
fw_Neotropical_fish_lpi  <- LPIMain("Infiles/fw_Neotropical_Fish_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)

fw_Nearctic_Aves_lpi  <- LPIMain("Infiles/fw_Nearctic_Aves_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
fw_Nearctic_Mammalia_lpi  <- LPIMain("Infiles/fw_Nearctic_Mammalia_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
fw_Nearctic_Herps_lpi  <- LPIMain("Infiles/fw_Nearctic_Herps_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
fw_Nearctic_fish_lpi  <- LPIMain("Infiles/fw_Nearctic_Fish_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)

m_AtNoTemp_Aves <- LPIMain("Infiles/m_AtNoTemp_Aves_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
m_AtNoTemp_Mammalia <- LPIMain("Infiles/m_AtNoTemp_Mammalia_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
m_AtNoTemp_Herps <- LPIMain("Infiles/m_AtNoTemp_Herps_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
m_AtNoTemp_Fish <- LPIMain("Infiles/m_AtNoTemp_Fish_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)

m_AtTrSub_Aves <- LPIMain("Infiles/m_AtTrSub_Aves_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
m_AtTrSub_Mammalia <- LPIMain("Infiles/m_AtTrSub_Mammalia_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
m_AtTrSub_Herps <- LPIMain("Infiles/m_AtTrSub_Herps_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
m_AtTrSub_Fish <- LPIMain("Infiles/m_AtTrSub_Fish_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)

m_Arctic_Aves <- LPIMain("Infiles/m_Arctic_Aves_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
m_Arctic_Mammalia <- LPIMain("Infiles/m_Arctic_Mammalia_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
m_Arctic_Fish <- LPIMain("Infiles/m_Arctic_Fish_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)

m_SoTeAnt_Aves <- LPIMain("Infiles/m_SoTeAnt_Aves_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
m_SoTeAnt_Mammalia <- LPIMain("Infiles/m_SoTeAnt_Mammalia_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
m_SoTeAnt_Herps <- LPIMain("Infiles/m_SoTeAnt_Herps_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
m_SoTeAnt_Fish <- LPIMain("Infiles/m_SoTeAnt_Fish_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)

m_TroSubIndo_Aves <- LPIMain("Infiles/m_TroSubIndo_Aves_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
m_TroSubIndo_Mammalia <- LPIMain("Infiles/m_TroSubIndo_Mammalia_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
m_TroSubIndo_Herps <- LPIMain("Infiles/m_TroSubIndo_Herps_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
m_TroSubIndo_Fish <- LPIMain("Infiles/m_TroSubIndo_Fish_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)

m_PaNoTemp_Aves <- LPIMain("Infiles/m_PaNoTemp_Aves_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
m_PaNoTemp_Mammalia <- LPIMain("Infiles/m_PaNoTemp_Mammalia_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
m_PaNoTemp_Herps <- LPIMain("Infiles/m_PaNoTemp_Herps_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)
m_PaNoTemp_Fish <- LPIMain("Infiles/m_PaNoTemp_Fish_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=0, use_weightings_B=0)

setwd("Infiles/")
terrestrial_lpi <- LPIMain("terrestrial_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=1, use_weightings_B=1)
freshwater_lpi <- LPIMain("freshwater_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=1, use_weightings_B=1)
marine_lpi <- LPIMain("marine_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE = 100, force_recalculation=1, use_weightings=1, use_weightings_B=1)
full_lpi <- LPIMain("lpi_infile2.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE=100, force_recalculation=1, use_weightings=1, use_weightings_B=1)
full_lpi_culled <- LPIMain("lpi_infile2.txt", REF_YEAR = 1970, PLOT_MAX = 2018, BOOT_STRAP_SIZE=100, force_recalculation=1, use_weightings=1, use_weightings_B=1)

terrestrial_lpi <- terrestrial_lpi[complete.cases(terrestrial_lpi), ]
freshwater_lpi <- freshwater_lpi[complete.cases(freshwater_lpi), ]
marine_lpi <- marine_lpi[complete.cases(marine_lpi), ] 
full_lpi <- full_lpi[complete.cases(full_lpi), ] 
full_lpi_culled <- full_lpi_culled[complete.cases(full_lpi_culled), ] 

gg_terr <- ggplot_lpi(terrestrial_lpi, ylim=c(0,2))
gg_fresh <- ggplot_lpi(freshwater_lpi, ylim=c(0,2))
gg_marine <- ggplot_lpi(marine_lpi, ylim=c(0,2))
gg_full <- ggplot_lpi(full_lpi, ylim=c(0,2))
gg_full_culled <- ggplot_lpi(full_lpi_culled, ylim=c(0,2), title="LPI with Bootstrapped Species Intervals")

saveRDS(gg_full, file="c:/R_projects/LPI_Sampling_Error/files/ggplot_LPI_all2.RData")
saveRDS(gg_full_culled, file="c:/R_projects/LPI_Sampling_Error/files/ggplot_LPI_all_culled2.RData")

library(ggpubr)
ggarrange(final_plot, gg_full_culled, ncol=2, nrow=1, common.legend=TRUE, legend="bottom")
