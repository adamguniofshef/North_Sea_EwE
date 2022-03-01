# ecoEnsemble comparing fish scenarios
library("EcoEnsemble")
library(mizer)
library(FishSUMS8)
library(Rcpp)
library(RcppArmadillo)
sourceCpp('calc_M2.cpp')


# From the previous ecoEnsemble scripts we have run everything
# under FMSY for all species. With Natural England we are considering
# the effects on the ecosystem when the fishing mortality on sandeels
# changes from FMSY. The two alternative scenarios considered are
# half FMSY and no F. 

# We want to fit the simulators for the two alternative scenarios,
# then fit the ensemble model with 3 variables per fish species:
# e.g., sandeels with have the 3 variables sandeels_FMSY, 
# sandeels_halfFMSY and sandeels_noF. Then the ensemble model
# is fitted as before, and in this case we should find the differences
# using the joint distribution of the ensemble outputs.

# Firstly, we need to fit the other 3 simulators under the 
# two alternatives (note that only sandeels F changes):

halfFMSY_mizer <- c(0.15, 0.7, 0.26, 0.172, 0.207, 0.21, 0.194, 0.31, 0.363)
mizer_halfFMSY <- get_mizer(Fs = halfFMSY_mizer, theta = theta, years_post_data = 58, simulation_end_date = 2077)

# FishSUMS --- sandeel, norway pout, herring, whiting,
# plaice, haddock, cod and saithe

halfFMSY_FishSUMS <- c(0.15, 0.7, 0.26, 0.172, 0.21, 0.194, 0.31, 0.363)
fishSUMS_halfFMSY <- get_fishsums(Fs = halfFMSY_FishSUMS, theta = theta, spec_nam = spec_nam,
                              simulation_end_date = 2077)

# LeMans --- 1) Sandeel, 2) N. pout, 3) Herring, 4) Whiting,
# 5) Sole, 6) Plaice 7) Haddock 8) Cod 9) Saithe

halfFMSY_LeMans <- c(0.15, 0.7, 0.26, 0.172, 0.207, 0.21, 0.194, 0.31, 0.363)
LeMans_halfFMSY <- get_lemans(new_fs = halfFMSY_LeMans, n_years = 92)

noF_mizer <- c(0, 0.7, 0.26, 0.172, 0.207, 0.21, 0.194, 0.31, 0.363)
mizer_noF <- get_mizer(Fs = noF_mizer, theta = theta, years_post_data = 58, simulation_end_date = 2077)

# FishSUMS --- sandeel, norway pout, herring, whiting,
# plaice, haddock, cod and saithe

noF_FishSUMS <- c(0, 0.7, 0.26, 0.172, 0.21, 0.194, 0.31, 0.363)
fishSUMS_noF <- get_fishsums(Fs = noF_FishSUMS, theta = theta, spec_nam = spec_nam,
                                  simulation_end_date = 2077)

# LeMans --- 1) Sandeel, 2) N. pout, 3) Herring, 4) Whiting,
# 5) Sole, 6) Plaice 7) Haddock 8) Cod 9) Saithe

noF_LeMans <- c(0, 0.7, 0.26, 0.172, 0.207, 0.21, 0.194, 0.31, 0.363)
LeMans_noF <- get_lemans(new_fs = noF_LeMans, n_years = 92)


NS_biomass <- read.csv("NS_EwE_biomass_TS_updated.csv", skip = 0)
# already rescaled (for fish species of interest)
NS_biomass_EE_all_log <- data.frame(Sandeel_FMSY = log(NS_biomass[-1,42]), Sprat_FMSY = log(NS_biomass[-1, 28]), N.pout_FMSY = log(NS_biomass[-1,30]),
                                Herring_FMSY = log(NS_biomass[-1,13] + NS_biomass[-1,15]), Dab_FMSY = log(NS_biomass[-1,18]),
                                Whiting_FMSY = log(NS_biomass[-1, 9] + NS_biomass[-1,31]), Sole_FMSY = log(NS_biomass[-1,16]), Gurnard_FMSY = log(NS_biomass[-1,17]),
                                Plaice_FMSY = log(NS_biomass[-1, 8]), Haddock_FMSY = log(NS_biomass[-1, 44] + NS_biomass[-1, 46]),
                                Cod_FMSY = log(NS_biomass[-1, 11] + NS_biomass[-1, 34]), Saithe_FMSY = log(NS_biomass[-1, 3] + NS_biomass[-1, 21]),
                                Nephrops_FMSY = log(NS_biomass[-1, 25]), Sandeel_halfFMSY = log(NS_biomass[-1,42]), Sprat_halfFMSY = log(NS_biomass[-1, 28]), N.pout_halfFMSY = log(NS_biomass[-1,30]),
                                Herring_halfFMSY = log(NS_biomass[-1,13] + NS_biomass[-1,15]), Dab_halfFMSY = log(NS_biomass[-1,18]),
                                Whiting_halfFMSY = log(NS_biomass[-1, 9] + NS_biomass[-1,31]), Sole_halfFMSY = log(NS_biomass[-1,16]), Gurnard_halfFMSY = log(NS_biomass[-1,17]),
                                Plaice_halfFMSY = log(NS_biomass[-1, 8]), Haddock_halfFMSY = log(NS_biomass[-1, 44] + NS_biomass[-1, 46]),
                                Cod_halfFMSY = log(NS_biomass[-1, 11] + NS_biomass[-1, 34]), Saithe_halfFMSY = log(NS_biomass[-1, 3] + NS_biomass[-1, 21]),
                                Nephrops_halfFMSY = log(NS_biomass[-1, 25]), Sandeel_noF = log(NS_biomass[-1,42]), Sprat_noF = log(NS_biomass[-1, 28]), N.pout_noF = log(NS_biomass[-1,30]),
                                Herring_noF = log(NS_biomass[-1,13] + NS_biomass[-1,15]), Dab_noF = log(NS_biomass[-1,18]),
                                Whiting_noF = log(NS_biomass[-1, 9] + NS_biomass[-1,31]), Sole_noF = log(NS_biomass[-1,16]), Gurnard_noF = log(NS_biomass[-1,17]),
                                Plaice_noF = log(NS_biomass[-1, 8]), Haddock_noF = log(NS_biomass[-1, 44] + NS_biomass[-1, 46]),
                                Cod_noF = log(NS_biomass[-1, 11] + NS_biomass[-1, 34]), Saithe_noF = log(NS_biomass[-1, 3] + NS_biomass[-1, 21]),
                                Nephrops_noF = log(NS_biomass[-1, 25]))

rownames(NS_biomass_EE_all_log) <- as.numeric(NS_biomass[-1,1])

# Getting the EwE code in

EwE_FMSY_40_iterations <- vector("list", 40)
EwE_FMSY_40_iterations[[1]] <- read.csv(paste0("C:\\Users\\smp18atg\\Google Drive\\Natural England project\\N.Sea_EwE_3_scenarios\\NSea_sandeel_MSY\\Sample_baseline\\ecosim_Basic setup\\biomass_annual.csv"),
                                        skip = 9)
EwE_FMSY_40_iterations[[1]][,-1] <- EwE_FMSY_40_iterations[[1]][,-1] * 570000
colnames(EwE_FMSY_40_iterations[[1]]) <- c("year", func_groups)

for(i in 1:39){
  
  EwE_FMSY_40_iterations[[i + 1]] <- read.csv(paste0("C:\\Users\\smp18atg\\Google Drive\\Natural England project\\N.Sea_EwE_3_scenarios\\NSea_sandeel_MSY\\Sample_", i, "\\ecosim_Basic setup\\biomass_annual.csv"),
                                              skip = 9)
  EwE_FMSY_40_iterations[[i + 1]][,-1] <- EwE_FMSY_40_iterations[[i + 1]][,-1] * 570000
  colnames(EwE_FMSY_40_iterations[[i + 1]]) <- c("year", func_groups)
  
}

EwE_FMSY_40_iterations[[2]][, "Sandeels"]

EwE_FMSY_EE_log <- array(NA, dim = c(87, N_species, 40))

for(i in 1:40){
  
  EwE_FMSY_EE_log[, 1, i] <- log(EwE_FMSY_40_iterations[[i]][1:87, "Sandeels"])
  EwE_FMSY_EE_log[, 2, i] <- log(EwE_FMSY_40_iterations[[i]][1:87, "Sprat"])
  EwE_FMSY_EE_log[, 3, i] <- log(EwE_FMSY_40_iterations[[i]][1:87, "Norway pout"])
  EwE_FMSY_EE_log[, 4, i] <- log(EwE_FMSY_40_iterations[[i]][1:87, "Herring (juvenile 0-1)"] + EwE_FMSY_40_iterations[[i]][1:87, "Herring (adult)"])
  EwE_FMSY_EE_log[, 5, i] <- log(EwE_FMSY_40_iterations[[i]][1:87, "Dab"])
  EwE_FMSY_EE_log[, 6, i] <- log(EwE_FMSY_40_iterations[[i]][1:87, "Whiting (juvenile 0-1)"] + EwE_FMSY_40_iterations[[i]][1:87, "Whiting (adult)"])
  EwE_FMSY_EE_log[, 7, i] <- log(EwE_FMSY_40_iterations[[i]][1:87, "Sole"])
  EwE_FMSY_EE_log[, 8, i] <- log(EwE_FMSY_40_iterations[[i]][1:87, "Gurnards"])
  EwE_FMSY_EE_log[, 9, i] <- log(EwE_FMSY_40_iterations[[i]][1:87, "Plaice"])
  EwE_FMSY_EE_log[, 10, i] <- log(EwE_FMSY_40_iterations[[i]][1:87, "Haddock (juvenile 0-1)"] + EwE_FMSY_40_iterations[[i]][1:87, "Haddock (adult)"])
  EwE_FMSY_EE_log[, 11, i] <- log(EwE_FMSY_40_iterations[[i]][1:87, "Cod (juvenile 0-2)"] + EwE_FMSY_40_iterations[[i]][1:87, "Cod (adult)"])
  EwE_FMSY_EE_log[, 12, i] <- log(EwE_FMSY_40_iterations[[i]][1:87, "Saithe (juvenile 0-3)"] + EwE_FMSY_40_iterations[[i]][1:87, "Saithe (adult)"])
  EwE_FMSY_EE_log[, 13, i] <- log(EwE_FMSY_40_iterations[[i]][1:87, "Nephrops"])
  
}
apply(EwE_FMSY_EE_log[, 1, ], 1, mean)

EwE_FMSY_EE_log_mean <- matrix(NA, 87, N_species)
EwE_FMSY_EE_log_cov <- array(NA, dim = c(N_species, N_species, 87))

for(j in 1:N_species){
  
  EwE_FMSY_EE_log_mean[, j] <- apply(EwE_FMSY_EE_log[, j, ], 1, mean) # mean for each year over 40 iters
  
  for(k in 1:N_species){
    for(n in 1:87){
      
      EwE_FMSY_EE_log_cov[j, k, n] <- cov(EwE_FMSY_EE_log[n, j, ], EwE_FMSY_EE_log[n, k, ]) # cov between each species for each year over 40 iters
      
    }
    
  }
  
}

EwE_FMSY_EE_log_mean <- as.data.frame(EwE_FMSY_EE_log_mean)
colnames(EwE_FMSY_EE_log_mean) <- c("Sandeel_FMSY", "Sprat_FMSY", "N.pout_FMSY", "Herring_FMSY",
                                    "Dab_FMSY", "Whiting_FMSY", "Sole_FMSY", "Gurnard_FMSY", "Plaice_FMSY",
                                    "Haddock_FMSY", "Cod_FMSY", "Saithe_FMSY", "Nephrops_FMSY")
rownames(EwE_FMSY_EE_log_mean) <- 1991:2077
EwE_FMSY_EE_log_cov_mean <- apply(EwE_FMSY_EE_log_cov, c(1, 2), mean)
colnames(EwE_FMSY_EE_log_cov_mean) <- c("Sandeel_FMSY", "Sprat_FMSY", "N.pout_FMSY", "Herring_FMSY",
                                        "Dab_FMSY", "Whiting_FMSY", "Sole_FMSY", "Gurnard_FMSY", "Plaice_FMSY",
                                        "Haddock_FMSY", "Cod_FMSY", "Saithe_FMSY", "Nephrops_FMSY")
rownames(EwE_FMSY_EE_log_cov_mean) <- c("Sandeel_FMSY", "Sprat_FMSY", "N.pout_FMSY", "Herring_FMSY",
                                        "Dab_FMSY", "Whiting_FMSY", "Sole_FMSY", "Gurnard_FMSY", "Plaice_FMSY",
                                        "Haddock_FMSY", "Cod_FMSY", "Saithe_FMSY", "Nephrops_FMSY")


mizer_FMSY_EE <- as.data.frame(mizer_FMSY$SSB[, c(2, 1, 3:12)])
colnames(mizer_FMSY_EE) <- paste0(colnames(mizer_FMSY_EE), "_FMSY")

fishSUMS_FMSY_log_tonnes <- log(fishSUMS_FMSY$SSB * 1000)
fishSUMS_FMSY_log_tonnes[, 12] <- exp(fishSUMS_FMSY_log_tonnes[, 12])/1000

fishSUMS_FMSY_EE <- as.data.frame(fishSUMS_FMSY_log_tonnes[, c(9, 11, 10, 6, 3, 5, 7, 2,
                                                               1, 4, 8)])
rownames(fishSUMS_FMSY_EE) <- fishSUMS_FMSY_log_tonnes[, 12]
colnames(fishSUMS_FMSY_EE)[2] <- "N.pout"
colnames(fishSUMS_FMSY_EE) <- paste0(colnames(fishSUMS_FMSY_EE), "_FMSY")

LeMans_FMSY_EE <- as.data.frame(LeMans_FMSY$SSB)
rownames(LeMans_FMSY_EE) <- 1986:2077
colnames(LeMans_FMSY_EE) <- c("Sandeel", "N.pout", "Herring", "Whiting", "Sole", 
                              "Plaice", "Haddock", "Cod", "Saithe")
colnames(LeMans_FMSY_EE) <- paste0(colnames(LeMans_FMSY_EE), "_FMSY")

EwE_halfFMSY_40_iterations <- vector("list", 40)
EwE_halfFMSY_40_iterations[[1]] <- read.csv(paste0("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_3_scenarios\\NSea_sandeel_half_MSY\\Sample_baseline\\ecosim_Basic setup\\biomass_annual.csv"),
                                            skip = 9)
EwE_halfFMSY_40_iterations[[1]][,-1] <- EwE_halfFMSY_40_iterations[[1]][,-1] * 570000
colnames(EwE_halfFMSY_40_iterations[[1]]) <- c("year", func_groups)

for(i in 1:39){
  
  EwE_halfFMSY_40_iterations[[i + 1]] <- read.csv(paste0("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_3_scenarios\\NSea_sandeel_half_MSY\\Sample_", i, "\\ecosim_Basic setup\\biomass_annual.csv"),
                                                  skip = 9)
  EwE_halfFMSY_40_iterations[[i + 1]][,-1] <- EwE_halfFMSY_40_iterations[[i + 1]][,-1] * 570000
  colnames(EwE_halfFMSY_40_iterations[[i + 1]]) <- c("year", func_groups)
  
}

EwE_halfFMSY_EE_log <- array(NA, dim = c(87, N_species, 40))

for(i in 1:40){
  
  EwE_halfFMSY_EE_log[, 1, i] <- log(EwE_halfFMSY_40_iterations[[i]][1:87, "Sandeels"])
  EwE_halfFMSY_EE_log[, 2, i] <- log(EwE_halfFMSY_40_iterations[[i]][1:87, "Sprat"])
  EwE_halfFMSY_EE_log[, 3, i] <- log(EwE_halfFMSY_40_iterations[[i]][1:87, "Norway pout"])
  EwE_halfFMSY_EE_log[, 4, i] <- log(EwE_halfFMSY_40_iterations[[i]][1:87, "Herring (juvenile 0-1)"] + EwE_halfFMSY_40_iterations[[i]][1:87, "Herring (adult)"])
  EwE_halfFMSY_EE_log[, 5, i] <- log(EwE_halfFMSY_40_iterations[[i]][1:87, "Dab"])
  EwE_halfFMSY_EE_log[, 6, i] <- log(EwE_halfFMSY_40_iterations[[i]][1:87, "Whiting (juvenile 0-1)"] + EwE_halfFMSY_40_iterations[[i]][1:87, "Whiting (adult)"])
  EwE_halfFMSY_EE_log[, 7, i] <- log(EwE_halfFMSY_40_iterations[[i]][1:87, "Sole"])
  EwE_halfFMSY_EE_log[, 8, i] <- log(EwE_halfFMSY_40_iterations[[i]][1:87, "Gurnards"])
  EwE_halfFMSY_EE_log[, 9, i] <- log(EwE_halfFMSY_40_iterations[[i]][1:87, "Plaice"])
  EwE_halfFMSY_EE_log[, 10, i] <- log(EwE_halfFMSY_40_iterations[[i]][1:87, "Haddock (juvenile 0-1)"] + EwE_halfFMSY_40_iterations[[i]][1:87, "Haddock (adult)"])
  EwE_halfFMSY_EE_log[, 11, i] <- log(EwE_halfFMSY_40_iterations[[i]][1:87, "Cod (juvenile 0-2)"] + EwE_halfFMSY_40_iterations[[i]][1:87, "Cod (adult)"])
  EwE_halfFMSY_EE_log[, 12, i] <- log(EwE_halfFMSY_40_iterations[[i]][1:87, "Saithe (juvenile 0-3)"] + EwE_halfFMSY_40_iterations[[i]][1:87, "Saithe (adult)"])
  EwE_halfFMSY_EE_log[, 13, i] <- log(EwE_halfFMSY_40_iterations[[i]][1:87, "Nephrops"])
  
}

EwE_halfFMSY_EE_log_mean <- matrix(NA, 87, N_species)
EwE_halfFMSY_EE_log_cov <- array(NA, dim = c(N_species, N_species, 87))

for(j in 1:N_species){
  
  EwE_halfFMSY_EE_log_mean[, j] <- apply(EwE_halfFMSY_EE_log[, j, ], 1, mean) # mean for each year over 40 iters
  
  for(k in 1:N_species){
    for(n in 1:87){
      
      EwE_halfFMSY_EE_log_cov[j, k, n] <- cov(EwE_halfFMSY_EE_log[n, j, ], EwE_halfFMSY_EE_log[n, k, ]) # cov between each species for each year over 40 iters
      
    }
    
  }
  
}

EwE_halfFMSY_EE_log_mean <- as.data.frame(EwE_halfFMSY_EE_log_mean)
colnames(EwE_halfFMSY_EE_log_mean) <- c("Sandeel_halfFMSY", "Sprat_halfFMSY", "N.pout_halfFMSY", "Herring_halfFMSY",
                                        "Dab_halfFMSY", "Whiting_halfFMSY", "Sole_halfFMSY", "Gurnard_halfFMSY", "Plaice_halfFMSY",
                                        "Haddock_halfFMSY", "Cod_halfFMSY", "Saithe_halfFMSY", "Nephrops_halfFMSY")
rownames(EwE_halfFMSY_EE_log_mean) <- 1991:2077
EwE_halfFMSY_EE_log_cov_mean <- apply(EwE_halfFMSY_EE_log_cov, c(1, 2), mean)
colnames(EwE_halfFMSY_EE_log_cov_mean) <- c("Sandeel_halfFMSY", "Sprat_halfFMSY", "N.pout_halfFMSY", "Herring_halfFMSY",
                                            "Dab_halfFMSY", "Whiting_halfFMSY", "Sole_halfFMSY", "Gurnard_halfFMSY", "Plaice_halfFMSY",
                                            "Haddock_halfFMSY", "Cod_halfFMSY", "Saithe_halfFMSY", "Nephrops_halfFMSY")
rownames(EwE_halfFMSY_EE_log_cov_mean) <- c("Sandeel_halfFMSY", "Sprat_halfFMSY", "N.pout_halfFMSY", "Herring_halfFMSY",
                                            "Dab_halfFMSY", "Whiting_halfFMSY", "Sole_halfFMSY", "Gurnard_halfFMSY", "Plaice_halfFMSY",
                                            "Haddock_halfFMSY", "Cod_halfFMSY", "Saithe_halfFMSY", "Nephrops_halfFMSY")


mizer_halfFMSY_EE <- as.data.frame(mizer_halfFMSY$SSB[, c(2, 1, 3:12)])
colnames(mizer_halfFMSY_EE) <- paste0(colnames(mizer_halfFMSY_EE), "_halfFMSY")

fishSUMS_halfFMSY_log_tonnes <- log(fishSUMS_halfFMSY$SSB * 1000)
fishSUMS_halfFMSY_log_tonnes[, 12] <- exp(fishSUMS_halfFMSY_log_tonnes[, 12])/1000

fishSUMS_halfFMSY_EE <- as.data.frame(fishSUMS_halfFMSY_log_tonnes[, c(9, 11, 10, 6, 3, 5, 7, 2,
                                                                       1, 4, 8)])
rownames(fishSUMS_halfFMSY_EE) <- fishSUMS_halfFMSY_log_tonnes[, 12]
colnames(fishSUMS_halfFMSY_EE)[2] <- "N.pout"
colnames(fishSUMS_halfFMSY_EE) <- paste0(colnames(fishSUMS_halfFMSY_EE), "_halfFMSY")

LeMans_halfFMSY_EE <- as.data.frame(LeMans_halfFMSY$SSB)
rownames(LeMans_halfFMSY_EE) <- 1986:2077
colnames(LeMans_halfFMSY_EE) <- c("Sandeel", "N.pout", "Herring", "Whiting", "Sole", 
                                  "Plaice", "Haddock", "Cod", "Saithe")
colnames(LeMans_halfFMSY_EE) <- paste0(colnames(LeMans_halfFMSY_EE), "_halfFMSY")

EwE_noF_40_iterations <- vector("list", 40)
EwE_noF_40_iterations[[1]] <- read.csv(paste0("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_3_scenarios\\NSea_sandeel_no_F\\Sample_baseline\\ecosim_Basic setup\\biomass_annual.csv"),
                                       skip = 9)
EwE_noF_40_iterations[[1]][,-1] <- EwE_noF_40_iterations[[1]][,-1] * 570000
colnames(EwE_noF_40_iterations[[1]]) <- c("year", func_groups)

for(i in 1:39){
  
  EwE_noF_40_iterations[[i + 1]] <- read.csv(paste0("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_3_scenarios\\NSea_sandeel_no_F\\Sample_", i, "\\ecosim_Basic setup\\biomass_annual.csv"),
                                             skip = 9)
  EwE_noF_40_iterations[[i + 1]][,-1] <- EwE_noF_40_iterations[[i + 1]][,-1] * 570000
  colnames(EwE_noF_40_iterations[[i + 1]]) <- c("year", func_groups)
  
}

EwE_noF_EE_log <- array(NA, dim = c(87, N_species, 40))

for(i in 1:40){
  
  EwE_noF_EE_log[, 1, i] <- log(EwE_noF_40_iterations[[i]][1:87, "Sandeels"])
  EwE_noF_EE_log[, 2, i] <- log(EwE_noF_40_iterations[[i]][1:87, "Sprat"])
  EwE_noF_EE_log[, 3, i] <- log(EwE_noF_40_iterations[[i]][1:87, "Norway pout"])
  EwE_noF_EE_log[, 4, i] <- log(EwE_noF_40_iterations[[i]][1:87, "Herring (juvenile 0-1)"] + EwE_noF_40_iterations[[i]][1:87, "Herring (adult)"])
  EwE_noF_EE_log[, 5, i] <- log(EwE_noF_40_iterations[[i]][1:87, "Dab"])
  EwE_noF_EE_log[, 6, i] <- log(EwE_noF_40_iterations[[i]][1:87, "Whiting (juvenile 0-1)"] + EwE_noF_40_iterations[[i]][1:87, "Whiting (adult)"])
  EwE_noF_EE_log[, 7, i] <- log(EwE_noF_40_iterations[[i]][1:87, "Sole"])
  EwE_noF_EE_log[, 8, i] <- log(EwE_noF_40_iterations[[i]][1:87, "Gurnards"])
  EwE_noF_EE_log[, 9, i] <- log(EwE_noF_40_iterations[[i]][1:87, "Plaice"])
  EwE_noF_EE_log[, 10, i] <- log(EwE_noF_40_iterations[[i]][1:87, "Haddock (juvenile 0-1)"] + EwE_noF_40_iterations[[i]][1:87, "Haddock (adult)"])
  EwE_noF_EE_log[, 11, i] <- log(EwE_noF_40_iterations[[i]][1:87, "Cod (juvenile 0-2)"] + EwE_noF_40_iterations[[i]][1:87, "Cod (adult)"])
  EwE_noF_EE_log[, 12, i] <- log(EwE_noF_40_iterations[[i]][1:87, "Saithe (juvenile 0-3)"] + EwE_noF_40_iterations[[i]][1:87, "Saithe (adult)"])
  EwE_noF_EE_log[, 13, i] <- log(EwE_noF_40_iterations[[i]][1:87, "Nephrops"])
  
}

EwE_noF_EE_log_mean <- matrix(NA, 87, N_species)
EwE_noF_EE_log_cov <- array(NA, dim = c(N_species, N_species, 87))

for(j in 1:N_species){
  
  EwE_noF_EE_log_mean[, j] <- apply(EwE_noF_EE_log[, j, ], 1, mean) # mean for each year over 40 iters
  
  for(k in 1:N_species){
    for(n in 1:87){
      
      EwE_noF_EE_log_cov[j, k, n] <- cov(EwE_noF_EE_log[n, j, ], EwE_noF_EE_log[n, k, ]) # cov between each species for each year over 40 iters
      
    }
    
  }
  
}

EwE_noF_EE_log_mean <- as.data.frame(EwE_noF_EE_log_mean)
colnames(EwE_noF_EE_log_mean) <- c("Sandeel_noF", "Sprat_noF", "N.pout_noF", "Herring_noF",
                                   "Dab_noF", "Whiting_noF", "Sole_noF", "Gurnard_noF", "Plaice_noF",
                                   "Haddock_noF", "Cod_noF", "Saithe_noF", "Nephrops_noF")
rownames(EwE_noF_EE_log_mean) <- 1991:2077
EwE_noF_EE_log_cov_mean <- apply(EwE_noF_EE_log_cov, c(1, 2), mean)
colnames(EwE_noF_EE_log_cov_mean) <- c("Sandeel_noF", "Sprat_noF", "N.pout_noF", "Herring_noF",
                                       "Dab_noF", "Whiting_noF", "Sole_noF", "Gurnard_noF", "Plaice_noF",
                                       "Haddock_noF", "Cod_noF", "Saithe_noF", "Nephrops_noF")
rownames(EwE_noF_EE_log_cov_mean) <- c("Sandeel_noF", "Sprat_noF", "N.pout_noF", "Herring_noF",
                                       "Dab_noF", "Whiting_noF", "Sole_noF", "Gurnard_noF", "Plaice_noF",
                                       "Haddock_noF", "Cod_noF", "Saithe_noF", "Nephrops_noF")


mizer_noF_EE <- as.data.frame(mizer_noF$SSB[, c(2, 1, 3:12)])
colnames(mizer_noF_EE) <- paste0(colnames(mizer_noF_EE), "_noF")

fishSUMS_noF_log_tonnes <- log(fishSUMS_noF$SSB * 1000)
fishSUMS_noF_log_tonnes[, 12] <- exp(fishSUMS_noF_log_tonnes[, 12])/1000

fishSUMS_noF_EE <- as.data.frame(fishSUMS_noF_log_tonnes[, c(9, 11, 10, 6, 3, 5, 7, 2,
                                                             1, 4, 8)])
rownames(fishSUMS_noF_EE) <- fishSUMS_noF_log_tonnes[, 12]
colnames(fishSUMS_noF_EE)[2] <- "N.pout"
colnames(fishSUMS_noF_EE) <- paste0(colnames(fishSUMS_noF_EE), "_noF")

LeMans_noF_EE <- as.data.frame(LeMans_noF$SSB)
rownames(LeMans_noF_EE) <- 1986:2077
colnames(LeMans_noF_EE) <- c("Sandeel", "N.pout", "Herring", "Whiting", "Sole", 
                             "Plaice", "Haddock", "Cod", "Saithe")
colnames(LeMans_noF_EE) <- paste0(colnames(LeMans_noF_EE), "_noF")


# majority of parameter uncertainty covariances not known, so
# broadly sensible values are randomly chosen in that case.

Stock_Sigma_all <- matrix(NA, N_species, N_species)
mizer_FMSY_EE_Sigma <- matrix(NA, ncol(mizer_FMSY_EE), ncol(mizer_FMSY_EE))
LeMans_FMSY_EE_Sigma <- matrix(NA, ncol(LeMans_FMSY_EE), ncol(LeMans_FMSY_EE))
fishSUMS_FMSY_EE_Sigma <- matrix(NA, ncol(fishSUMS_FMSY_EE), ncol(fishSUMS_FMSY_EE))

install.packages("LaplacesDemon")
library(LaplacesDemon)

set.seed(99)
Stock_Sigma_all <- matrix(rWishart(1, df = N_species*3, Sigma = diag(N_species*3)/ 100), N_species*3, N_species*3)
colnames(Stock_Sigma_all) <- colnames(NS_biomass_EE_all_log)
rownames(Stock_Sigma_all) <- colnames(NS_biomass_EE_all_log)
eigen(Stock_Sigma_all)$values


#diag(Stock_Sigma) <- rnorm(n = ncol(Stock_Sigma), mean = 0.08, sd = 0.02)
#Stock_Sigma[upper.tri(Stock_Sigma)] <- rnorm(n = ncol(Stock_Sigma)*(ncol(Stock_Sigma) - 1)/2, mean = 0, sd = 0.01)
#Stock_Sigma[lower.tri(Stock_Sigma)] <- Stock_Sigma[upper.tri(Stock_Sigma)][c(1, 2, 4, 7, 11, 16, 22, 29,
#                                                                             37, 46, 56, 67, 3, 5, 8,
#                                                                             12, 17, 23, 30, 38, 47, 57,
#                                                                             68, 6, 9, 13, 18, 24, 31, 39,
#                                                                             48, 58, 69, 10, 14, 19, 25,
#                                                                             32, 40, 49, 59, 70, 15, 20,
#                                                                             26, 33, 41, 50, 60, 71, 21,
#                                                                             27, 34, 42, 51, 61, 72, 28,
#                                                                             35, 43, 52, 62, 73, 36, 44,
#                                                                             53, 63, 74, 45, 54, 64, 75,
#                                                                             55, 65, 76, 66, 77, 78)]

mizer_all_EE_Sigma <- matrix(rWishart(1, df = ncol(mizer_FMSY_EE), Sigma = diag(ncol(mizer_FMSY_EE))/ 100), ncol(mizer_FMSY_EE), ncol(mizer_FMSY_EE))
colnames(mizer_FMSY_EE_Sigma) <- paste0(colnames(mizer_FMSY_EE), "_FMSY")
rownames(mizer_FMSY_EE_Sigma) <- paste0(colnames(mizer_FMSY_EE), "_FMSY")
eigen(mizer_FMSY_EE_Sigma)$values

#mizer_FMSY_EE_Sigma[rownames(Sigma_miz), colnames(Sigma_miz)] <- Sigma_miz

LeMans_FMSY_EE_Sigma <- matrix(rWishart(1, df = ncol(LeMans_FMSY_EE), Sigma = diag(ncol(LeMans_FMSY_EE))/ 100), ncol(LeMans_FMSY_EE), ncol(LeMans_FMSY_EE))
colnames(LeMans_FMSY_EE_Sigma) <- paste0(colnames(LeMans_FMSY_EE), "_FMSY")
rownames(LeMans_FMSY_EE_Sigma) <- paste0(colnames(LeMans_FMSY_EE), "_FMSY")
eigen(LeMans_FMSY_EE_Sigma)$values


#LeMans_FMSY_EE_Sigma[rownames(Sigma_lm), colnames(Sigma_lm)] <- Sigma_lm

fishSUMS_FMSY_EE_Sigma <- matrix(rWishart(1, df = ncol(fishSUMS_FMSY_EE), Sigma = diag(ncol(fishSUMS_FMSY_EE))/ 100), ncol(fishSUMS_FMSY_EE), ncol(fishSUMS_FMSY_EE))
colnames(fishSUMS_FMSY_EE_Sigma) <- paste0(colnames(fishSUMS_FMSY_EE), "_FMSY")
rownames(fishSUMS_FMSY_EE_Sigma) <- paste0(colnames(fishSUMS_FMSY_EE), "_FMSY")
eigen(fishSUMS_FMSY_EE_Sigma)$values


#fishSUMS_FMSY_EE_Sigma[rownames(Sigma_fs), colnames(Sigma_fs)] <- Sigma_fs




# majority of parameter uncertainty covariances not known, so
# broadly sensible values are randomly chosen in that case.

Stock_Sigma <- matrix(NA, N_species, N_species)
mizer_halfFMSY_EE_Sigma <- matrix(NA, ncol(mizer_halfFMSY_EE), ncol(mizer_halfFMSY_EE))
LeMans_halfFMSY_EE_Sigma <- matrix(NA, ncol(LeMans_halfFMSY_EE), ncol(LeMans_halfFMSY_EE))
fishSUMS_halfFMSY_EE_Sigma <- matrix(NA, ncol(fishSUMS_halfFMSY_EE), ncol(fishSUMS_halfFMSY_EE))

install.packages("LaplacesDemon")
library(LaplacesDemon)

set.seed(99)
Stock_Sigma <- matrix(rWishart(1, df = N_species, Sigma = diag(N_species)/ 100), N_species, N_species)
colnames(Stock_Sigma) <- colnames(NS_biomass_EE_log)
rownames(Stock_Sigma) <- colnames(NS_biomass_EE_log)
eigen(Stock_Sigma)$values


#diag(Stock_Sigma) <- rnorm(n = ncol(Stock_Sigma), mean = 0.08, sd = 0.02)
#Stock_Sigma[upper.tri(Stock_Sigma)] <- rnorm(n = ncol(Stock_Sigma)*(ncol(Stock_Sigma) - 1)/2, mean = 0, sd = 0.01)
#Stock_Sigma[lower.tri(Stock_Sigma)] <- Stock_Sigma[upper.tri(Stock_Sigma)][c(1, 2, 4, 7, 11, 16, 22, 29,
#                                                                             37, 46, 56, 67, 3, 5, 8,
#                                                                             12, 17, 23, 30, 38, 47, 57,
#                                                                             68, 6, 9, 13, 18, 24, 31, 39,
#                                                                             48, 58, 69, 10, 14, 19, 25,
#                                                                             32, 40, 49, 59, 70, 15, 20,
#                                                                             26, 33, 41, 50, 60, 71, 21,
#                                                                             27, 34, 42, 51, 61, 72, 28,
#                                                                             35, 43, 52, 62, 73, 36, 44,
#                                                                             53, 63, 74, 45, 54, 64, 75,
#                                                                             55, 65, 76, 66, 77, 78)]

set.seed(99)
mizer_halfFMSY_EE_Sigma <- matrix(rWishart(1, df = ncol(mizer_halfFMSY_EE), Sigma = diag(ncol(mizer_halfFMSY_EE))/ 100), ncol(mizer_halfFMSY_EE), ncol(mizer_halfFMSY_EE))
colnames(mizer_halfFMSY_EE_Sigma) <- colnames(mizer_halfFMSY_EE)
rownames(mizer_halfFMSY_EE_Sigma) <- colnames(mizer_halfFMSY_EE)
eigen(mizer_halfFMSY_EE_Sigma)$values

#mizer_halfFMSY_EE_Sigma[rownames(Sigma_miz), colnames(Sigma_miz)] <- Sigma_miz

set.seed(99)
LeMans_halfFMSY_EE_Sigma <- matrix(rWishart(1, df = ncol(LeMans_halfFMSY_EE), Sigma = diag(ncol(LeMans_halfFMSY_EE))/ 100), ncol(LeMans_halfFMSY_EE), ncol(LeMans_halfFMSY_EE))
colnames(LeMans_halfFMSY_EE_Sigma) <- colnames(LeMans_halfFMSY_EE)
rownames(LeMans_halfFMSY_EE_Sigma) <- colnames(LeMans_halfFMSY_EE)
eigen(LeMans_halfFMSY_EE_Sigma)$values


#LeMans_halfFMSY_EE_Sigma[rownames(Sigma_lm), colnames(Sigma_lm)] <- Sigma_lm

set.seed(99)
fishSUMS_halfFMSY_EE_Sigma <- matrix(rWishart(1, df = ncol(fishSUMS_halfFMSY_EE), Sigma = diag(ncol(fishSUMS_halfFMSY_EE))/ 100), ncol(fishSUMS_halfFMSY_EE), ncol(fishSUMS_halfFMSY_EE))
colnames(fishSUMS_halfFMSY_EE_Sigma) <- colnames(fishSUMS_halfFMSY_EE)
rownames(fishSUMS_halfFMSY_EE_Sigma) <- colnames(fishSUMS_halfFMSY_EE)
eigen(fishSUMS_halfFMSY_EE_Sigma)$values


#fishSUMS_halfFMSY_EE_Sigma[rownames(Sigma_fs), colnames(Sigma_fs)] <- Sigma_fs


# majority of parameter uncertainty covariances not known, so
# broadly sensible values are randomly chosen in that case.

Stock_Sigma <- matrix(NA, N_species, N_species)
mizer_noF_EE_Sigma <- matrix(NA, ncol(mizer_noF_EE), ncol(mizer_noF_EE))
LeMans_noF_EE_Sigma <- matrix(NA, ncol(LeMans_noF_EE), ncol(LeMans_noF_EE))
fishSUMS_noF_EE_Sigma <- matrix(NA, ncol(fishSUMS_noF_EE), ncol(fishSUMS_noF_EE))

install.packages("LaplacesDemon")
library(LaplacesDemon)

set.seed(99)
Stock_Sigma <- matrix(rWishart(1, df = N_species, Sigma = diag(N_species)/ 100), N_species, N_species)
colnames(Stock_Sigma) <- paste0(colnames(NS_biomass_EE_log), "_noF")
rownames(Stock_Sigma) <- paste0(colnames(NS_biomass_EE_log), "_noF")
eigen(Stock_Sigma)$values


#diag(Stock_Sigma) <- rnorm(n = ncol(Stock_Sigma), mean = 0.08, sd = 0.02)
#Stock_Sigma[upper.tri(Stock_Sigma)] <- rnorm(n = ncol(Stock_Sigma)*(ncol(Stock_Sigma) - 1)/2, mean = 0, sd = 0.01)
#Stock_Sigma[lower.tri(Stock_Sigma)] <- Stock_Sigma[upper.tri(Stock_Sigma)][c(1, 2, 4, 7, 11, 16, 22, 29,
#                                                                             37, 46, 56, 67, 3, 5, 8,
#                                                                             12, 17, 23, 30, 38, 47, 57,
#                                                                             68, 6, 9, 13, 18, 24, 31, 39,
#                                                                             48, 58, 69, 10, 14, 19, 25,
#                                                                             32, 40, 49, 59, 70, 15, 20,
#                                                                             26, 33, 41, 50, 60, 71, 21,
#                                                                             27, 34, 42, 51, 61, 72, 28,
#                                                                             35, 43, 52, 62, 73, 36, 44,
#                                                                             53, 63, 74, 45, 54, 64, 75,
#                                                                             55, 65, 76, 66, 77, 78)]

mizer_noF_EE_Sigma <- matrix(rWishart(1, df = ncol(mizer_noF_EE), Sigma = diag(ncol(mizer_noF_EE))/ 100), ncol(mizer_noF_EE), ncol(mizer_noF_EE))
colnames(mizer_noF_EE_Sigma) <- paste0(colnames(mizer_noF_EE), "_noF")
rownames(mizer_noF_EE_Sigma) <- paste0(colnames(mizer_noF_EE), "_noF")
eigen(mizer_noF_EE_Sigma)$values

#mizer_noF_EE_Sigma[rownames(Sigma_miz), colnames(Sigma_miz)] <- Sigma_miz

LeMans_noF_EE_Sigma <- matrix(rWishart(1, df = ncol(LeMans_noF_EE), Sigma = diag(ncol(LeMans_noF_EE))/ 100), ncol(LeMans_noF_EE), ncol(LeMans_noF_EE))
colnames(LeMans_noF_EE_Sigma) <- paste0(colnames(LeMans_noF_EE), "_noF")
rownames(LeMans_noF_EE_Sigma) <- paste0(colnames(LeMans_noF_EE), "_noF")
eigen(LeMans_noF_EE_Sigma)$values


#LeMans_noF_EE_Sigma[rownames(Sigma_lm), colnames(Sigma_lm)] <- Sigma_lm

fishSUMS_noF_EE_Sigma <- matrix(rWishart(1, df = ncol(fishSUMS_noF_EE), Sigma = diag(ncol(fishSUMS_noF_EE))/ 100), ncol(fishSUMS_noF_EE), ncol(fishSUMS_noF_EE))
colnames(fishSUMS_noF_EE_Sigma) <- paste0(colnames(fishSUMS_noF_EE), "_noF")
rownames(fishSUMS_noF_EE_Sigma) <- paste0(colnames(fishSUMS_noF_EE), "_noF")
eigen(fishSUMS_noF_EE_Sigma)$values


#fishSUMS_FMSY_EE_Sigma[rownames(Sigma_fs), colnames(Sigma_fs)] <- Sigma_fs
