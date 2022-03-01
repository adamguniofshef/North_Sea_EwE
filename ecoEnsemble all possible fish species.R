library("EcoEnsemble")
library(mizer)
library(FishSUMS8)
library(Rcpp)
library(RcppArmadillo)
sourceCpp('calc_M2.cpp')


# running the ensemble model with all possible fish species.

# mizer produces output for sprat, sandeel, norway pout, herring, dab, whiting
# sole, gurnard, plaice, haddock, cod and saithe

# fishSUMS produces output for cod, haddock, whiting, saithe, gurnard, dab,
# plaice, nephrops, sandeel, herring and norway pout

# LeMans produces output for Sandeel, N. pout, Herring, Whiting, Sole,
# Plaice, Haddock, Cod, and Saithe 

# EwE produces output for all fish mentioned above, so all fish above will
# be included

# so fish species are sandeel, sprat, norway pout, herring, dab, whiting
# sole, gurnard, plaice, haddock, cod, saithe, nephrops

# four simulators: EwE, mizer, FishSUMS, LeMans

# fish species occurring in all simulators:
# sandeels, norway pout, herring, whiting, plaice, haddock, cod, saithe 

# Stock assessment data being used from Natural England

# Running the 3 non-EwE simulators under FMSY for each species up until 2078
# (instead of 2100, as 2078 is the end of the 60-year project, and we assume
# all stock biomass will have settled to steady state by then)

FMSY_mizer <- c(0.3, 0.7, 0.26, 0.172, 0.207, 0.21, 0.194, 0.31, 0.363)
mizer_FMSY <- get_mizer(Fs = FMSY_mizer, theta = theta, years_post_data = 58, simulation_end_date = 2077)

# FishSUMS --- sandeel, norway pout, herring, whiting,
# plaice, haddock, cod and saithe

FMSY_FishSUMS <- c(0.3, 0.7, 0.26, 0.172, 0.21, 0.194, 0.31, 0.363)
fishSUMS_FMSY <- get_fishsums(Fs = FMSY_FishSUMS, theta = theta, spec_nam = spec_nam,
                              simulation_end_date = 2077)

# LeMans --- 1) Sandeel, 2) N. pout, 3) Herring, 4) Whiting,
# 5) Sole, 6) Plaice 7) Haddock 8) Cod 9) Saithe

FMSY_LeMans <- c(0.3, 0.7, 0.26, 0.172, 0.207, 0.21, 0.194, 0.31, 0.363)
LeMans_FMSY <- get_lemans(new_fs = FMSY_LeMans, n_years = 92)


NS_biomass <- read.csv("NS_EwE_biomass_TS_updated.csv", skip = 0)
# already rescaled (for fish species of interest)
NS_biomass_EE_log <- data.frame(Sandeel = log(NS_biomass[-1,42]), Sprat = log(NS_biomass[-1, 28]), N.pout = log(NS_biomass[-1,30]),
                            Herring = log(NS_biomass[-1,13] + NS_biomass[-1,15]), Dab = log(NS_biomass[-1,18]),
                            Whiting = log(NS_biomass[-1, 9] + NS_biomass[-1,31]), Sole = log(NS_biomass[-1,16]), Gurnard = log(NS_biomass[-1,17]),
                            Plaice = log(NS_biomass[-1, 8]), Haddock = log(NS_biomass[-1, 44] + NS_biomass[-1, 46]),
                            Cod = log(NS_biomass[-1, 11] + NS_biomass[-1, 34]), Saithe = log(NS_biomass[-1, 3] + NS_biomass[-1, 21]),
                            Nephrops = log(NS_biomass[-1, 25]))
rownames(NS_biomass_EE_log) <- as.numeric(NS_biomass[-1,1])

# Getting the EwE code in

N_species <- ncol(NS_biomass_EE_log)
func_groups_output <- read.csv("C:\\Users\\adamg\\Google Drive\\Natural England project\\Previous report\\NorthSea_EwE_basic_estimates_functional_groups.csv")
func_groups_output <- func_groups_output[!is.na(func_groups_output[, 1]), ]
func_groups <- func_groups_output[, 2]

EwE_FMSY_40_iterations <- vector("list", 40)
EwE_FMSY_40_iterations[[1]] <- read.csv(paste0("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_3_scenarios\\NSea_sandeel_MSY\\Sample_baseline\\ecosim_Basic setup\\biomass_annual.csv"),
                                        skip = 9)
EwE_FMSY_40_iterations[[1]][,-1] <- EwE_FMSY_40_iterations[[1]][,-1] * 570000
colnames(EwE_FMSY_40_iterations[[1]]) <- c("year", func_groups)

for(i in 1:39){
  
  EwE_FMSY_40_iterations[[i + 1]] <- read.csv(paste0("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_3_scenarios\\NSea_sandeel_MSY\\Sample_", i, "\\ecosim_Basic setup\\biomass_annual.csv"),
                                              skip = 9)
  EwE_FMSY_40_iterations[[i + 1]][,-1] <- EwE_FMSY_40_iterations[[i + 1]][,-1] * 570000
  colnames(EwE_FMSY_40_iterations[[i + 1]]) <- c("year", func_groups)
  
}

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
colnames(EwE_FMSY_EE_log_mean) <- c("Sandeel", "Sprat", "N.pout", "Herring",
                                    "Dab", "Whiting", "Sole", "Gurnard", "Plaice",
                                    "Haddock", "Cod", "Saithe", "Nephrops")
rownames(EwE_FMSY_EE_log_mean) <- 1991:2077
EwE_FMSY_EE_log_cov_mean <- apply(EwE_FMSY_EE_log_cov, c(1, 2), mean)
colnames(EwE_FMSY_EE_log_cov_mean) <- c("Sandeel", "Sprat", "N.pout", "Herring",
                                    "Dab", "Whiting", "Sole", "Gurnard", "Plaice",
                                    "Haddock", "Cod", "Saithe", "Nephrops")
rownames(EwE_FMSY_EE_log_cov_mean) <- c("Sandeel", "Sprat", "N.pout", "Herring",
                                        "Dab", "Whiting", "Sole", "Gurnard", "Plaice",
                                        "Haddock", "Cod", "Saithe", "Nephrops")


mizer_FMSY_EE <- as.data.frame(mizer_FMSY$SSB[, c(2, 1, 3:12)])

fishSUMS_FMSY_log_tonnes <- log(fishSUMS_FMSY$SSB * 1000)
fishSUMS_FMSY_log_tonnes[, 12] <- exp(fishSUMS_FMSY_log_tonnes[, 12])/1000

fishSUMS_FMSY_EE <- as.data.frame(fishSUMS_FMSY_log_tonnes[, c(9, 11, 10, 6, 3, 5, 7, 2,
                                                               1, 4, 8)])
rownames(fishSUMS_FMSY_EE) <- fishSUMS_FMSY_log_tonnes[, 12]
colnames(fishSUMS_FMSY_EE)[2] <- "N.pout"

LeMans_FMSY_EE <- as.data.frame(LeMans_FMSY$SSB)
rownames(LeMans_FMSY_EE) <- 1986:2077
colnames(LeMans_FMSY_EE) <- c("Sandeel", "N.pout", "Herring", "Whiting", "Sole", 
                                        "Plaice", "Haddock", "Cod", "Saithe")


# majority of parameter uncertainty covariances not known, so
# broadly sensible values are randomly chosen in that case.

Stock_Sigma <- matrix(NA, N_species, N_species)
mizer_FMSY_EE_Sigma <- matrix(NA, ncol(mizer_FMSY_EE), ncol(mizer_FMSY_EE))
LeMans_FMSY_EE_Sigma <- matrix(NA, ncol(LeMans_FMSY_EE), ncol(LeMans_FMSY_EE))
fishSUMS_FMSY_EE_Sigma <- matrix(NA, ncol(fishSUMS_FMSY_EE), ncol(fishSUMS_FMSY_EE))

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

mizer_FMSY_EE_Sigma <- matrix(rWishart(1, df = ncol(mizer_FMSY_EE), Sigma = diag(ncol(mizer_FMSY_EE))/ 100), ncol(mizer_FMSY_EE), ncol(mizer_FMSY_EE))
colnames(mizer_FMSY_EE_Sigma) <- colnames(mizer_FMSY_EE)
rownames(mizer_FMSY_EE_Sigma) <- colnames(mizer_FMSY_EE)
eigen(mizer_FMSY_EE_Sigma)$values

#mizer_FMSY_EE_Sigma[rownames(Sigma_miz), colnames(Sigma_miz)] <- Sigma_miz

LeMans_FMSY_EE_Sigma <- matrix(rWishart(1, df = ncol(LeMans_FMSY_EE), Sigma = diag(ncol(LeMans_FMSY_EE))/ 100), ncol(LeMans_FMSY_EE), ncol(LeMans_FMSY_EE))
colnames(LeMans_FMSY_EE_Sigma) <- colnames(LeMans_FMSY_EE)
rownames(LeMans_FMSY_EE_Sigma) <- colnames(LeMans_FMSY_EE)
eigen(LeMans_FMSY_EE_Sigma)$values


#LeMans_FMSY_EE_Sigma[rownames(Sigma_lm), colnames(Sigma_lm)] <- Sigma_lm

fishSUMS_FMSY_EE_Sigma <- matrix(rWishart(1, df = ncol(fishSUMS_FMSY_EE), Sigma = diag(ncol(fishSUMS_FMSY_EE))/ 100), ncol(fishSUMS_FMSY_EE), ncol(fishSUMS_FMSY_EE))
colnames(fishSUMS_FMSY_EE_Sigma) <- colnames(fishSUMS_FMSY_EE)
rownames(fishSUMS_FMSY_EE_Sigma) <- colnames(fishSUMS_FMSY_EE)
eigen(fishSUMS_FMSY_EE_Sigma)$values


#fishSUMS_FMSY_EE_Sigma[rownames(Sigma_fs), colnames(Sigma_fs)] <- Sigma_fs


FMSY_Ensemble_priors_EE <- define_priors(ind_st_var_params = list(25, 0.25),
                                                  ind_st_cor_form = "lkj", #Using an LKJ distribution for individual short-term discrepancies
                                                  ind_st_cor_params = 30, #The parameter is 30
                                                  ind_lt_var_params = list(rep(25, N_species),rep(0.25, N_species)),
                                                  ind_lt_cor_form = "beta",
                                                  ind_lt_cor_params = list(matrix(40, N_species, N_species), matrix(40, N_species, N_species)),
                                                  sha_st_var_exp = 3,
                                                  sha_st_cor_form = "lkj",
                                                  sha_st_cor_params = 30,
                                                  sha_lt_sd = rep(4, N_species))


FMSY_Ensemble_EE_fit <- fit_ensemble_model(observations = list(NS_biomass_EE_log, Stock_Sigma),
                                                    simulators = list(list(EwE_FMSY_EE_log_mean, EwE_FMSY_EE_log_cov_mean, "EwE"),
                                                                      list(fishSUMS_FMSY_EE,  fishSUMS_FMSY_EE_Sigma, "FishSUMS"),
                                                                      list(LeMans_FMSY_EE,  LeMans_FMSY_EE_Sigma, "LeMans"),
                                                                      list(mizer_FMSY_EE, mizer_FMSY_EE_Sigma, "mizer")),
                                                    priors = FMSY_Ensemble_priors_EE,
                                                    full_sample = FALSE)
# Was having issues with fitting the ensemble with all the species,
# just checked again and with just cod everything works
# will include more and more species then see when things start to fail

# fish species occurring in all simulators:
# sandeels, norway pout, herring, whiting, plaice, haddock, cod, saithe

FMSY_Ensemble_priors_EE_8species <- define_priors(ind_st_var_params = list(25, 0.25),
                                         ind_st_cor_form = "lkj", #Using an LKJ distribution for individual short-term discrepancies
                                         ind_st_cor_params = 30, #The parameter is 30
                                         ind_lt_var_params = list(rep(25, 8),rep(0.25, 8)),
                                         ind_lt_cor_form = "beta",
                                         ind_lt_cor_params = list(matrix(40, 8, 8), matrix(40, 8, 8)),
                                         sha_st_var_exp = 3,
                                         sha_st_cor_form = "lkj",
                                         sha_st_cor_params = 30,
                                         sha_lt_sd = rep(4, 8))


FMSY_Ensemble_EE_fit_8species <- fit_ensemble_model(observations = list(NS_biomass_EE_log[, c(1, 3, 4, 6, 9, 10, 11, 12)], Stock_Sigma[c(1, 3, 4, 6, 9, 10, 11, 12), c(1, 3, 4, 6, 9, 10, 11, 12)]),
                                           simulators = list(list(EwE_FMSY_EE_log_mean[, c(1, 3, 4, 6, 9, 10, 11, 12)], EwE_FMSY_EE_log_cov_mean[c(1, 3, 4, 6, 9, 10, 11, 12), c(1, 3, 4, 6, 9, 10, 11, 12)], "EwE"),
                                                             list(fishSUMS_FMSY_EE[, c(1, 2, 3, 5, 7, 8, 9, 10)],  fishSUMS_FMSY_EE_Sigma[c(1, 2, 3, 5, 7, 8, 9, 10), c(1, 2, 3, 5, 7, 8, 9, 10)], "FishSUMS"),
                                                             list(LeMans_FMSY_EE[, c(1, 2, 3, 4, 6, 7, 8, 9)],  LeMans_FMSY_EE_Sigma[c(1, 2, 3, 4, 6, 7, 8, 9), c(1, 2, 3, 4, 6, 7, 8, 9)], "LeMans"),
                                                             list(mizer_FMSY_EE[, c(1, 3, 4, 6, 9, 10, 11, 12)], mizer_FMSY_EE_Sigma[c(1, 3, 4, 6, 9, 10, 11, 12), c(1, 3, 4, 6, 9, 10, 11, 12)], "mizer")),
                                           priors = FMSY_Ensemble_priors_EE_8species,
                                           full_sample = FALSE)

# ensemble with 8 species works, with all 8 species in every simulator

FMSY_Ensemble_EE_sample_8species <- generate_sample(FMSY_Ensemble_EE_fit_8species, num_samples = 2000)
EcoEnsemble:::plot.EnsembleSample(FMSY_Ensemble_EE_sample_8species, variable = 8,
                                  quantiles = c(0.025, 0.975))

# now want to see if this works then with fewer species, but a species
# that doesn't exist in every simulator

FMSY_Ensemble_priors_EE_3species <- define_priors(ind_st_var_params = list(25, 0.25),
                                                  ind_st_cor_form = "lkj", #Using an LKJ distribution for individual short-term discrepancies
                                                  ind_st_cor_params = 30, #The parameter is 30
                                                  ind_lt_var_params = list(rep(25, 3),rep(0.25, 3)),
                                                  ind_lt_cor_form = "beta",
                                                  ind_lt_cor_params = list(matrix(40, 3, 3), matrix(40, 3, 3)),
                                                  sha_st_var_exp = 3,
                                                  sha_st_cor_form = "lkj",
                                                  sha_st_cor_params = 30,
                                                  sha_lt_sd = rep(4, 3))


FMSY_Ensemble_EE_fit_3species <- fit_ensemble_model(observations = list(NS_biomass_EE_log[, c(1, 2, 3)], Stock_Sigma[c(1, 2, 3), c(1, 2, 3)]),
                                                    simulators = list(list(EwE_FMSY_EE_log_mean[, c(1, 2, 3)], EwE_FMSY_EE_log_cov_mean[c(1, 2, 3), c(1, 2, 3)], "EwE"),
                                                                      list(fishSUMS_FMSY_EE[, c(1, 2)],  fishSUMS_FMSY_EE_Sigma[c(1, 2), c(1, 2)], "FishSUMS"),
                                                                      list(LeMans_FMSY_EE[, c(1, 2)],  LeMans_FMSY_EE_Sigma[c(1, 2), c(1, 2)], "LeMans"),
                                                                      list(mizer_FMSY_EE[, c(1, 2, 3)], mizer_FMSY_EE_Sigma[c(1, 2, 3), c(1, 2, 3)], "mizer")),
                                                    priors = FMSY_Ensemble_priors_EE_3species,
                                                    full_sample = FALSE)

FMSY_Ensemble_EE_sample_3species <- generate_sample(FMSY_Ensemble_EE_fit_3species, num_samples = 2000)
EcoEnsemble:::plot.EnsembleSample(FMSY_Ensemble_EE_sample_3species, variable = 2,
                                  quantiles = c(0.025, 0.975))

# so a model with sandeels, sprat and N.pout works, where sandeels
# and norway pout occur in all models, sprat in just two models

# think I will work with the 8-species model and build from there


FMSY_Ensemble_priors_EE_9species <- define_priors(ind_st_var_params = list(25, 0.25),
                                                  ind_st_cor_form = "lkj", #Using an LKJ distribution for individual short-term discrepancies
                                                  ind_st_cor_params = 30, #The parameter is 30
                                                  ind_lt_var_params = list(rep(25, 9),rep(0.25, 9)),
                                                  ind_lt_cor_form = "beta",
                                                  ind_lt_cor_params = list(matrix(40, 9, 9), matrix(40, 9, 9)),
                                                  sha_st_var_exp = 3,
                                                  sha_st_cor_form = "lkj",
                                                  sha_st_cor_params = 30,
                                                  sha_lt_sd = rep(4, 9))


FMSY_Ensemble_EE_fit_9species <- fit_ensemble_model(observations = list(NS_biomass_EE_log[, c(1, 2, 3, 4, 6, 9, 10, 11, 12)], Stock_Sigma[c(1, 2, 3, 4, 6, 9, 10, 11, 12), c(1, 2, 3, 4, 6, 9, 10, 11, 12)]),
                                                    simulators = list(list(EwE_FMSY_EE_log_mean[, c(1, 2, 3, 4, 6, 9, 10, 11, 12)], EwE_FMSY_EE_log_cov_mean[c(1, 2, 3, 4, 6, 9, 10, 11, 12), c(1, 2, 3, 4, 6, 9, 10, 11, 12)], "EwE"),
                                                                      list(fishSUMS_FMSY_EE[, c(1, 2, 3, 5, 7, 8, 9, 10)],  fishSUMS_FMSY_EE_Sigma[c(1, 2, 3, 5, 7, 8, 9, 10), c(1, 2, 3, 5, 7, 8, 9, 10)], "FishSUMS"),
                                                                      list(LeMans_FMSY_EE[, c(1, 2, 3, 4, 6, 7, 8, 9)],  LeMans_FMSY_EE_Sigma[c(1, 2, 3, 4, 6, 7, 8, 9), c(1, 2, 3, 4, 6, 7, 8, 9)], "LeMans"),
                                                                      list(mizer_FMSY_EE[, c(1, 2, 3, 4, 6, 9, 10, 11, 12)], mizer_FMSY_EE_Sigma[c(1, 2, 3, 4, 6, 9, 10, 11, 12), c(1, 2, 3, 4, 6, 9, 10, 11, 12)], "mizer")),
                                                    priors = FMSY_Ensemble_priors_EE_9species,
                                                    full_sample = FALSE)

# ensemble with 9 species works, with 8 species in every simulator
# and 1 species in just two simulators

FMSY_Ensemble_EE_sample_9species <- generate_sample(FMSY_Ensemble_EE_fit_9species, num_samples = 2000)
EcoEnsemble:::plot.EnsembleSample(FMSY_Ensemble_EE_sample_9species, variable = 8,
                                  quantiles = c(0.025, 0.975))


FMSY_Ensemble_priors_EE_10species <- define_priors(ind_st_var_params = list(25, 0.25),
                                                  ind_st_cor_form = "lkj", #Using an LKJ distribution for individual short-term discrepancies
                                                  ind_st_cor_params = 30, #The parameter is 30
                                                  ind_lt_var_params = list(rep(25, 10),rep(0.25, 10)),
                                                  ind_lt_cor_form = "beta",
                                                  ind_lt_cor_params = list(matrix(40, 10, 10), matrix(40, 10, 10)),
                                                  sha_st_var_exp = 3,
                                                  sha_st_cor_form = "lkj",
                                                  sha_st_cor_params = 30,
                                                  sha_lt_sd = rep(4, 10))


FMSY_Ensemble_EE_fit_10species <- fit_ensemble_model(observations = list(NS_biomass_EE_log[, c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12)], Stock_Sigma[c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12), c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12)]),
                                                    simulators = list(list(EwE_FMSY_EE_log_mean[, c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12)], EwE_FMSY_EE_log_cov_mean[c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12), c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12)], "EwE"),
                                                                      list(fishSUMS_FMSY_EE[, c(1, 2, 3, 4, 5, 7, 8, 9, 10)],  fishSUMS_FMSY_EE_Sigma[c(1, 2, 3, 4, 5, 7, 8, 9, 10), c(1, 2, 3, 4, 5, 7, 8, 9, 10)], "FishSUMS"),
                                                                      list(LeMans_FMSY_EE[, c(1, 2, 3, 4, 6, 7, 8, 9)],  LeMans_FMSY_EE_Sigma[c(1, 2, 3, 4, 6, 7, 8, 9), c(1, 2, 3, 4, 6, 7, 8, 9)], "LeMans"),
                                                                      list(mizer_FMSY_EE[, c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12)], mizer_FMSY_EE_Sigma[c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12), c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12)], "mizer")),
                                                    priors = FMSY_Ensemble_priors_EE_10species,
                                                    full_sample = FALSE)

# ensemble with 10 species works, with 8 species in every simulator,
# 1 species in just two simulators, and 1 species in just 3 simulators

FMSY_Ensemble_EE_sample_10species <- generate_sample(FMSY_Ensemble_EE_fit_10species, num_samples = 2000)
EcoEnsemble:::plot.EnsembleSample(FMSY_Ensemble_EE_sample_10species, variable = 8,
                                  quantiles = c(0.025, 0.975))



FMSY_Ensemble_priors_EE_11species <- define_priors(ind_st_var_params = list(25, 0.25),
                                                   ind_st_cor_form = "lkj", #Using an LKJ distribution for individual short-term discrepancies
                                                   ind_st_cor_params = 30, #The parameter is 30
                                                   ind_lt_var_params = list(rep(25, 11),rep(0.25, 11)),
                                                   ind_lt_cor_form = "beta",
                                                   ind_lt_cor_params = list(matrix(40, 11, 11), matrix(40, 11, 11)),
                                                   sha_st_var_exp = 3,
                                                   sha_st_cor_form = "lkj",
                                                   sha_st_cor_params = 30,
                                                   sha_lt_sd = rep(4, 11))


FMSY_Ensemble_EE_fit_11species <- fit_ensemble_model(observations = list(NS_biomass_EE_log[, c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12)], Stock_Sigma[c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12), c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12)]),
                                                     simulators = list(list(EwE_FMSY_EE_log_mean[, c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12)], EwE_FMSY_EE_log_cov_mean[c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12), c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12)], "EwE"),
                                                                       list(fishSUMS_FMSY_EE[, c(1, 2, 3, 4, 5, 7, 8, 9, 10)],  fishSUMS_FMSY_EE_Sigma[c(1, 2, 3, 4, 5, 7, 8, 9, 10), c(1, 2, 3, 4, 5, 7, 8, 9, 10)], "FishSUMS"),
                                                                       list(LeMans_FMSY_EE[, c(1, 2, 3, 4, 5, 6, 7, 8, 9)],  LeMans_FMSY_EE_Sigma[c(1, 2, 3, 4, 5, 6, 7, 8, 9), c(1, 2, 3, 4, 5, 6, 7, 8, 9)], "LeMans"),
                                                                       list(mizer_FMSY_EE[, c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12)], mizer_FMSY_EE_Sigma[c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12), c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12)], "mizer")),
                                                     priors = FMSY_Ensemble_priors_EE_11species,
                                                     full_sample = FALSE)

# ensemble with 11 species does not work, at least it does not work with
# the 11 species being sandeel, sprat, n.pout, herring, dab, whiting,
# sole, plaice, haddock and cod. 10 had worked, with these species minus sole,
# so will try including one of the other two species not used in the 10 species ensemble


FMSY_Ensemble_EE_fit_11species_try2 <- fit_ensemble_model(observations = list(NS_biomass_EE_log[, c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12)], Stock_Sigma[c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12), c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12)]),
                                                          simulators = list(list(EwE_FMSY_EE_log_mean[, c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12)], EwE_FMSY_EE_log_cov_mean[c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12), c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12)], "EwE"),
                                                                            list(fishSUMS_FMSY_EE[, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)],  fishSUMS_FMSY_EE_Sigma[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)], "FishSUMS"),
                                                                            list(LeMans_FMSY_EE[, c(1, 2, 3, 4, 6, 7, 8, 9)],  LeMans_FMSY_EE_Sigma[c(1, 2, 3, 4, 6, 7, 8, 9), c(1, 2, 3, 4, 6, 7, 8, 9)], "LeMans"),
                                                                            list(mizer_FMSY_EE[, c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12)], mizer_FMSY_EE_Sigma[c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12), c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12)], "mizer")),
                                                          priors = FMSY_Ensemble_priors_EE_11species,
                                                          full_sample = FALSE)

# 11 species again didn't work, this time with gurnards included instead
# of sole

FMSY_Ensemble_EE_fit_11species_try3 <- fit_ensemble_model(observations = list(NS_biomass_EE_log[, c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12, 13)], Stock_Sigma[c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12, 13), c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12, 13)]),
                                                          simulators = list(list(EwE_FMSY_EE_log_mean[, c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12, 13)], EwE_FMSY_EE_log_cov_mean[c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12, 13), c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12, 13)], "EwE"),
                                                                            list(fishSUMS_FMSY_EE[, c(1, 2, 3, 4, 5, 7, 8, 9, 10, 11)],  fishSUMS_FMSY_EE_Sigma[c(1, 2, 3, 4, 5, 7, 8, 9, 10, 11), c(1, 2, 3, 4, 5, 7, 8, 9, 10, 11)], "FishSUMS"),
                                                                            list(LeMans_FMSY_EE[, c(1, 2, 3, 4, 6, 7, 8, 9)],  LeMans_FMSY_EE_Sigma[c(1, 2, 3, 4, 6, 7, 8, 9), c(1, 2, 3, 4, 6, 7, 8, 9)], "LeMans"),
                                                                            list(mizer_FMSY_EE[, c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12)], mizer_FMSY_EE_Sigma[c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12), c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12)], "mizer")),
                                                          priors = FMSY_Ensemble_priors_EE_11species,
                                                          full_sample = FALSE)

# 11 species again didn't work, this time with nephrops included instead
# of sole or gurnard

# I'm not tempted to see whether 10 is the maximum number of species? this would
# probably be specified in the package help page --- just checked and it isn't

# Tempting to fit an ensemble model with 10 species using a different subset
# of species as a test



FMSY_Ensemble_EE_fit_10species_v2 <- fit_ensemble_model(observations = list(NS_biomass_EE_log[, c(1, 2, 3, 4, 5, 6, 9, 10, 11, 13)], Stock_Sigma[c(1, 2, 3, 4, 5, 6, 9, 10, 11, 13), c(1, 2, 3, 4, 5, 6, 9, 10, 11, 13)]),
                                                     simulators = list(list(EwE_FMSY_EE_log_mean[, c(1, 2, 3, 4, 5, 6, 9, 10, 11, 13)], EwE_FMSY_EE_log_cov_mean[c(1, 2, 3, 4, 5, 6, 9, 10, 11, 13), c(1, 2, 3, 4, 5, 6, 9, 10, 11, 13)], "EwE"),
                                                                       list(fishSUMS_FMSY_EE[, c(1, 2, 3, 4, 5, 7, 8, 9, 11)],  fishSUMS_FMSY_EE_Sigma[c(1, 2, 3, 4, 5, 7, 8, 9, 11), c(1, 2, 3, 4, 5, 7, 8, 9, 11)], "FishSUMS"),
                                                                       list(LeMans_FMSY_EE[, c(1, 2, 3, 4, 6, 7, 8)],  LeMans_FMSY_EE_Sigma[c(1, 2, 3, 4, 6, 7, 8), c(1, 2, 3, 4, 6, 7, 8)], "LeMans"),
                                                                       list(mizer_FMSY_EE[, c(1, 2, 3, 4, 5, 6, 9, 10, 11)], mizer_FMSY_EE_Sigma[c(1, 2, 3, 4, 5, 6, 9, 10, 11), c(1, 2, 3, 4, 5, 6, 9, 10, 11)], "mizer")),
                                                     priors = FMSY_Ensemble_priors_EE_10species,
                                                     full_sample = FALSE)

# ensemble with 10 species works, with 8 species in every simulator,
# 1 species in just two simulators, and 1 species in just 3 simulators
