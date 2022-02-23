library("EcoEnsemble")
library(mizer)
library(FishSUMS8)
library(Rcpp)
library(RcppArmadillo)
sourceCpp('calc_M2.cpp')


# running the ensemble model with 2 fish species, sandeels and cod
# four simulators: EwE, mizer, FishSUMS, LeMans

# Stock assessment data being used from Natural England

# Running the 3 non-EwE simulators under FMSY for each species up until 2100

FMSY_mizer <- c(0.3, 0.7, 0.26, 0.172, 0.207, 0.21, 0.194, 0.31, 0.363)
mizer_FMSY <- get_mizer(Fs = FMSY_mizer, theta = theta, years_post_data = 81, simulation_end_date = 2100)

# FishSUMS --- sandeel, norway pout, herring, whiting,
# plaice, haddock, cod and saithe

FMSY_FishSUMS <- c(0.3, 0.7, 0.26, 0.172, 0.21, 0.194, 0.31, 0.363)
fishSUMS_FMSY <- get_fishsums(Fs = FMSY_FishSUMS, theta = theta, spec_nam = spec_nam,
                              simulation_end_date = 2100)

# LeMans --- 1) Sandeel, 2) N. pout, 3) Herring, 4) Whiting,
# 5) Sole, 6) Plaice 7) Haddock 8) Cod 9) Saithe

FMSY_LeMans <- c(0.3, 0.7, 0.26, 0.172, 0.207, 0.21, 0.194, 0.31, 0.363)
LeMans_FMSY <- get_lemans(new_fs = FMSY_LeMans, n_years = 115)


NS_biomass <- read.csv(paste0("C:\\Users\\adamg\\Google Drive\\Natural England project\\North_Sea_EwE\\NS_EwE_biomass_TS_updated.csv"),
                       skip = 0)
# already rescaled (for fish species of interest)
NS_biomass_sandeel_log <- data.frame(Sandeels = log(NS_biomass[-1,42]))
rownames(NS_biomass_sandeel_log) <- as.numeric(NS_biomass[-1,1])

NS_biomass_cod_log <- matrix(log(NS_biomass[-1, 11]), 23, 1)
rownames(NS_biomass_cod_log) <- as.numeric(NS_biomass[-1,1])
colnames(NS_biomass_cod_log) <- "Cod"
NS_biomass_sandeel_cod_log <- as.data.frame(cbind(NS_biomass_sandeel_log, NS_biomass_cod_log))
colnames(NS_biomass_sandeel_cod_log) <- c("Sandeel", "Cod")

# Getting the EwE code in

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

EwE_FMSY_Sandeels <- matrix(NA, nrow = nrow(EwE_FMSY_40_iterations[[1]]), ncol = 40)

for(i in 1:40){
  
  EwE_FMSY_Sandeels[, i] <- EwE_FMSY_40_iterations[[i]][, "Sandeels"]
  
}

# everything in tonnes, so just need to take log

EwE_FMSY_Sandeels_log <- log(EwE_FMSY_Sandeels)
EwE_FMSY_Sandeels_log_mean <- apply(EwE_FMSY_Sandeels_log, 1, mean) # mean over 40 iters
EwE_FMSY_Sandeels_log_var <- apply(EwE_FMSY_Sandeels_log, 1, var) # var over 40 iters
EwE_FMSY_Sandeels_log_var_mean <- mean(EwE_FMSY_Sandeels_log_var)

EwE_FMSY_cod <- matrix(NA, nrow = nrow(EwE_FMSY_40_iterations[[1]]), ncol = 40)

for(i in 1:40){
  for(j in 1:110){
    
    EwE_FMSY_cod[j, i] <- sum(EwE_FMSY_40_iterations[[i]][j, "Cod (juvenile 0-2)"],
                              EwE_FMSY_40_iterations[[i]][j, "Cod (adult)"])
    
  }
}

# everything in tonnes, so just need to take log

EwE_FMSY_cod_log <- log(EwE_FMSY_cod)
EwE_FMSY_cod_log_mean <- apply(EwE_FMSY_cod_log, 1, mean) # mean over 40 iters
EwE_FMSY_Sandeel_cod_log_cov <- rep(NA, 110)

for(j in 1:110){
  
  EwE_FMSY_Sandeel_cod_log_cov[j] <- cov(EwE_FMSY_Sandeels_log[j, ], EwE_FMSY_cod_log[j, ])
  
}
# var over 40 iters
EwE_FMSY_Sandeel_cod_log_cov_mean <- mean(EwE_FMSY_Sandeel_cod_log_cov)
EwE_FMSY_cod_log_var <- apply(EwE_FMSY_cod_log, 1, var)
EwE_FMSY_cod_log_var_mean <- mean(EwE_FMSY_cod_log_var)

EwE_FMSY_Sandeel_cod_covariance_mat <- matrix(c(EwE_FMSY_Sandeels_log_var_mean,
                                                EwE_FMSY_Sandeel_cod_log_cov_mean,
                                                EwE_FMSY_Sandeel_cod_log_cov_mean,
                                                EwE_FMSY_cod_log_var_mean), 2, 2)
colnames(EwE_FMSY_Sandeel_cod_covariance_mat) <- c("Sandeel", "Cod")
rownames(EwE_FMSY_Sandeel_cod_covariance_mat) <- c("Sandeel", "Cod")

EwE_FMSY_Sandeel_cod_log_mean <- as.data.frame(cbind(EwE_FMSY_Sandeels_log_mean, EwE_FMSY_cod_log_mean))
colnames(EwE_FMSY_Sandeel_cod_log_mean) <- c("Sandeel", "Cod")
rownames(EwE_FMSY_Sandeel_cod_log_mean) <- 1991:2100


mizer_FMSY_sandeels_cod <- as.data.frame(mizer_FMSY$SSB[, c(2, 11)])

fishSUMS_FMSY_log_tonnes[, 12] <- exp(fishSUMS_FMSY_log_tonnes[, 12])/1000

fishSUMS_FMSY_sandeels_cod <- as.data.frame(fishSUMS_FMSY_log_tonnes[, c(9, 1)])
rownames(fishSUMS_FMSY_sandeels_cod) <- fishSUMS_FMSY_log_tonnes[, 12]

LeMans_FMSY_sandeels_cod <- as.data.frame(LeMans_FMSY$SSB[, c(1, 8)])
rownames(LeMans_FMSY_sandeels_cod) <- 1986:2100
colnames(LeMans_FMSY_sandeels_cod) <- c("Sandeel", "Cod")



# majority of parameter uncertainty covariances not known, so
# broadly sensible values are randomly chosen in that case.

Sigma_sandeel_stock <- 0.0083
Sigma_sandeel_FishSUMS <- 0.0088
Sigma_sandeel_LeMans <- 0.0082
Sigma_sandeel_mizer <- 0.0075
Sigma_fs_cod <- Sigma_fs[3, 3]
Sigma_lm_cod <- Sigma_lm[3, 3]
Sigma_miz_cod <- Sigma_miz[3, 3]
Sigma_cod_stock <- 0.076 
Sigma_fs_sandeel_cod <- matrix(c(Sigma_sandeel_FishSUMS, 0, 0, Sigma_fs_cod), 2, 2)
Sigma_stock_sandeel_cod <- matrix(c(Sigma_sandeel_stock, 0, 0, Sigma_cod_stock), 2, 2)
Sigma_miz_sandeel_cod <- matrix(c(Sigma_sandeel_mizer, 0, 0, Sigma_miz_cod), 2, 2)
Sigma_lm_sandeel_cod <- matrix(c(Sigma_sandeel_LeMans, 0, 0, Sigma_lm_cod), 2, 2)
colnames(Sigma_fs_sandeel_cod) <- c("Sandeel", "Cod")
colnames(Sigma_stock_sandeel_cod) <- c("Sandeel", "Cod")
colnames(Sigma_miz_sandeel_cod) <- c("Sandeel", "Cod")
colnames(Sigma_lm_sandeel_cod) <- c("Sandeel", "Cod")
rownames(Sigma_fs_sandeel_cod) <- c("Sandeel", "Cod")
rownames(Sigma_stock_sandeel_cod) <- c("Sandeel", "Cod")
rownames(Sigma_miz_sandeel_cod) <- c("Sandeel", "Cod")
rownames(Sigma_lm_sandeel_cod) <- c("Sandeel", "Cod")


FMSY_Ensemble_priors_sandeel_cod <- define_priors(ind_st_var_params = list(25, 0.25),
                                                  ind_st_cor_form = "lkj", #Using an LKJ distribution for individual short-term discrepancies
                                                  ind_st_cor_params = 30, #The parameter is 30
                                                  ind_lt_var_params = list(rep(25,2),rep(0.25,2)),
                                                  ind_lt_cor_form = "beta",
                                                  ind_lt_cor_params = list(matrix(40,2, 2), matrix(40, 2, 2)),
                                                  sha_st_var_exp = 3,
                                                  sha_st_cor_form = "lkj",
                                                  sha_st_cor_params = 30,
                                                  sha_lt_sd = rep(4,2))


FMSY_Ensemble_sandeel_cod_fit <- fit_ensemble_model(observations = list(NS_biomass_sandeel_cod_log, Sigma_stock_sandeel_cod),
                                               simulators = list(list(EwE_FMSY_Sandeel_cod_log_mean, EwE_FMSY_Sandeel_cod_covariance_mat, "EwE"),
                                                                 list(fishSUMS_FMSY_sandeels_cod,  Sigma_fs_sandeel_cod, "FishSUMS"),
                                                                 list(LeMans_FMSY_sandeels_cod,  Sigma_lm_sandeel_cod, "LeMans"),
                                                                 list(mizer_FMSY_sandeels_cod, Sigma_miz_sandeel_cod, "mizer")),
                                               priors = FMSY_Ensemble_priors_sandeel_cod,
                                               full_sample = FALSE)

FMSY_Ensemble_sandeel_cod_sample <- generate_sample(FMSY_Ensemble_sandeel_cod_fit, num_samples = 2000)
EcoEnsemble:::get_mle(FMSY_Ensemble_sandeel_cod_sample)

gen_sample(FMSY_Ensemble_sandeel_cod_sample)
EcoEnsemble:::get_transformed_data(FMSY_Ensemble_sandeel_cod_sample)


FMSY_Ensemble_sandeel_cod_sample@mle
# so, the rows in the samples correspond to the years
# assume quite likely that the first two columns are the SSB predictions
# from the ensemble model for sandeel and cod respectively

length(FMSY_Ensemble_sandeel_cod_sample@mle[,1]) # 117 years from 1984 to 2100
EcoEnsemble:::get_parameters(FMSY_Ensemble_sandeel_cod_fit@samples)

FMSY_Ensemble_sandeel_cod_fit@point_estimate
rstan::extract(FMSY_Ensemble_sandeel_cod_fit@point_estimate)

EcoEnsemble:::plot.EnsembleSample(FMSY_Ensemble_sandeel_cod_sample)
generate_sample
