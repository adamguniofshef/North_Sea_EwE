# in order to run EcoEnsemble, the package for running the ensemble
# model, we need model outputs from multiple simulators for at least
# one fish species. Our first test will be to look at running 
# the ensemble model with just sandeels, which means we need to run
# mizer, FishSUMS and LeMans to get sandeel data.

# With EwE, we have been interested in the differences between the outputs
# under different fishing mortality scenarios F. I aim to run 
# the other simulators under each of these fishing scenarios,
# so the ensemble can then be fitted under the 3 fishing scenarios.

# Currently we have 40 iterations of the EwE simulator for each fishing
# scenario --- for the ensemble model, we will take the mean of those
# as well as a variance estimate, which will be used in the ensemble model.

# I'll firstly list the fishing mortality used in EwE for each of the fish 
# species that are modelled in any of the non-EwE simulators. The sandeel
# fishing mortality will be given for the baseline scenario F_MSY.
# All the other Fs given for other fish species are also their F_MSY.

# mizer --- 1) Sandeel, 2) N. pout, 3) Herring, 4) Whiting, 5) Sole,
# 6) Plaice 7) Haddock 8) Cod 9) Saithe

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

# Just realised --- the SSB_obs data is in the format of log tonnes
# So I just need to get everything else in that same format

mizer_FMSY$SSB[1,] # 117 years from 1984 to 2100, in log tonnes
fishSUMS_FMSY$SSB[1,] # 117 years from 1984 to 2100, spawning stock biomass in 1000 tonnes
LeMans_FMSY$SSB # 81 years from 2020 to 2100 (I think), in log tonnes

# fishSUMS_FMSY is in 1000 tonnes, so will convert to tonnes, then take the log

# NOTE: was an issue in the get_lemans function, which is actually an issue with
# the getting_mse function within it --- one of the arguments for getting_mse
# is nyears (number of years for which the outputs are predicted),
# but within the function is a for-loop from 1:n_years, which I think should
# be nyears --- so either change the argument to n_years or change the n_years
# in the function nyears

getting_mse <- function(targetF,lemans_runs1,n_years=31){
  #lemans_runs1 <- lemans_runs[[1]]
  SSB1 <- matrix(0, n_years, 9)
  Yield1 <- matrix(0, n_years, 9)
  Fs <- targetF
  
  for (yr in 1:n_years) {
    ### get the next F
    ### xhat[1:7] is the current SSB
    ### run with new F
    lemans_runs1 <- LeMans1(lemans_runs1, Fs=Fs)
    ssb_out <- (log(lemans_runs1$SSB)-log(1e6))[
      c(2,3,7,12,10,15,17,20,21)] ### calculate the next SSB
    SSB1[yr, ] <- ssb_out
    
    yield_out <- (log(colSums(lemans_runs1$Catch))-log(1e6))[
      c(2,3,7,12,10,15,17,20,21)] ### calculate the next yield
    Yield1[yr, ] <- yield_out
  }
  return(list(SSB=SSB1, Yield = Yield1))
}

# changed everything to be n_years now

# going back to above scaling stuff

fishSUMS_FMSY_log_tonnes <- log(fishSUMS_FMSY$SSB * 1000)

# for now, only care about sandeels

fishSUMS_FMSY_sandeels <- fishSUMS_FMSY_log_tonnes[,9]
mizer_FMSY_sandeels <- mizer_FMSY$SSB[,2]
LeMans_FMSY_sandeels <- LeMans_FMSY$SSB[,1]

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
EwE_FMSY_Sandeels_log_var_mean_mat <- matrix(EwE_FMSY_Sandeels_log_var_mean, 1, 1)
colnames(EwE_FMSY_Sandeels_log_var_mean_mat) <- "Sandeels"
rownames(EwE_FMSY_Sandeels_log_var_mean_mat) <- "Sandeels"


FMSY_Ensemble_priors <- define_priors(ind_st_var_params = list(25, 0.25),
                                      ind_st_cor_form = "lkj", #Using an LKJ distribution for individual short-term discrepancies
                                      ind_st_cor_params = 30, #The parameter is 30
                                      ind_lt_var_params = list(25, 0.25),
                                      ind_lt_cor_form = "beta",
                                      ind_lt_cor_params = list(as.matrix(40), as.matrix(40)),
                                      sha_st_var_exp = 3,
                                      sha_st_cor_form = "lkj",
                                      sha_st_cor_params = 30,
                                      sha_lt_sd = 4)

EwE_FMSY_Sandeels_log_mean_mat <- data.frame(Sandeels = EwE_FMSY_Sandeels_log_mean)
fishSUMS_FMSY_sandeels_mat <- data.frame(Sandeels = fishSUMS_FMSY_sandeels)
mizer_FMSY_sandeels_mat <- data.frame(Sandeels = mizer_FMSY_sandeels)
LeMans_FMSY_sandeels_mat <- data.frame(Sandeels = LeMans_FMSY_sandeels)

rownames(EwE_FMSY_Sandeels_log_mean_mat) <- 1991:2100
rownames(fishSUMS_FMSY_sandeels_mat) <- 1984:2100
rownames(mizer_FMSY_sandeels_mat) <- 1984:2100
rownames(LeMans_FMSY_sandeels_mat) <- 1986:2100

NS_biomass <- read.csv(paste0("C:\\Users\\adamg\\Google Drive\\Natural England project\\North_Sea_EwE\\NS_EwE_biomass_TS_updated.csv"),
                       skip = 0)

NS_biomass_sandeel_log <- data.frame(Sandeels = log(NS_biomass[-1,42]))
rownames(NS_biomass_sandeel_log) <- as.numeric(NS_biomass[-1,1])

Sigma_lm
Sigma_miz

# for now, I just want to see if this can run, so I will make up some
# values for the uncertainty

Sigma_sandeel_stock <- as.matrix(0.0083)
Sigma_sandeel_FishSUMS <- as.matrix(0.0088)
Sigma_sandeel_LeMans <- as.matrix(0.0082)
Sigma_sandeel_mizer <- as.matrix(0.0075)
rownames(Sigma_sandeel_stock) <- "Sandeels"
rownames(Sigma_sandeel_FishSUMS) <- "Sandeels"
rownames(Sigma_sandeel_LeMans) <- "Sandeels"
rownames(Sigma_sandeel_mizer) <- "Sandeels"
colnames(Sigma_sandeel_stock) <- "Sandeels"
colnames(Sigma_sandeel_FishSUMS) <- "Sandeels"
colnames(Sigma_sandeel_LeMans) <- "Sandeels"
colnames(Sigma_sandeel_mizer) <- "Sandeels"

FMSY_Ensemble_sandeel_data <- EnsembleData(observations = list(NS_biomass_sandeel_log, Sigma_sandeel_stock),
                              simulators = list(list(EwE_FMSY_Sandeels_log_mean_mat, EwE_FMSY_Sandeels_log_var_mean_mat, "EwE"),
                              list(fishSUMS_FMSY_sandeels_mat,  Sigma_sandeel_FishSUMS, "FishSUMS"),
                              list(LeMans_FMSY_sandeels_mat,  Sigma_sandeel_LeMans, "LeMans"),
                              list(mizer_FMSY_sandeels_mat, Sigma_sandeel_mizer, "mizer")),
                              priors = FMSY_Ensemble_priors)

# looking likely that the ensemble model cannot be run with just one species.
# Error in if (nrow(dat_for_year) == 1) { : argument is of length zero
# Not entirely sure what this error means, but there doesn't seem to be any
# issues in the priors I have defined, nor in the EnsembleData function

# Will try to run it with sandeels and one other group, which appears in 
# all of the simulators

# Taking the stock assessment SSB from the NS_EwE_biomass_TS_updated file for
# cod

NS_biomass_cod_log <- matrix(log(NS_biomass[-1, 11]), 23, 1)
rownames(NS_biomass_cod_log) <- as.numeric(NS_biomass[-1,1])
colnames(NS_biomass_cod_log) <- "Cod"
NS_biomass_sandeel_cod_log <- as.data.frame(cbind(NS_biomass_sandeel_log, NS_biomass_cod_log))
colnames(NS_biomass_sandeel_cod_log) <- c("Sandeel", "Cod")


mizer_FMSY_sandeels_cod <- as.data.frame(mizer_FMSY$SSB[, c(2, 11)])

fishSUMS_FMSY_log_tonnes[, 12] <- exp(fishSUMS_FMSY_log_tonnes[, 12])/1000

fishSUMS_FMSY_sandeels_cod <- as.data.frame(fishSUMS_FMSY_log_tonnes[, c(9, 1)])
rownames(fishSUMS_FMSY_sandeels_cod) <- fishSUMS_FMSY_log_tonnes[, 12]

LeMans_FMSY_sandeels_cod <- as.data.frame(LeMans_FMSY$SSB[, c(1, 8)])
rownames(LeMans_FMSY_sandeels_cod) <- 1986:2100
colnames(LeMans_FMSY_sandeels_cod) <- c("Sandeel", "Cod")

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

Sigma_sandeel_stock
Sigma_sandeel_FishSUMS
Sigma_sandeel_LeMans
Sigma_sandeel_mizer
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


FMSY_Ensemble_sandeel_cod_data <- EnsembleData(observations = list(NS_biomass_sandeel_cod_log, Sigma_stock_sandeel_cod),
                                           simulators = list(list(EwE_FMSY_Sandeel_cod_log_mean, EwE_FMSY_Sandeel_cod_covariance_mat, "EwE"),
                                                             list(fishSUMS_FMSY_sandeels_cod,  Sigma_fs_sandeel_cod, "FishSUMS"),
                                                             list(LeMans_FMSY_sandeels_cod,  Sigma_lm_sandeel_cod, "LeMans"),
                                                             list(mizer_FMSY_sandeels_cod, Sigma_miz_sandeel_cod, "mizer")),
                                           priors = FMSY_Ensemble_priors_sandeel_cod)

# same error as when just using sandeels, so now I assume that the issue
# is the data I am providing as stock assessments is from 1991 onwards, 
# and it needs to be from 1984 onwards, as we have mizer and fishsums
# that start in 1984, and maybe end in 2017?
# NOTE all is fixed now, change from matrices to data frames
# although data frame with one variable does not seem to work

NS_biomass_sandeel_cod_log
NS_biomass_sandeel_cod_sim1_data <- matrix(NA, 7, 2)
for(i in 1:7){
  
  NS_biomass_sandeel_cod_sim1_data[i, 1] <- rnorm(1, mean = 14.5, sd = 0.5) 
  NS_biomass_sandeel_cod_sim1_data[i, 2] <- rnorm(1, mean = 11.5, sd = 0.25)
  
}
colnames(NS_biomass_sandeel_cod_sim1_data) <- c("Sandeel", "Cod")
rownames(NS_biomass_sandeel_cod_sim1_data) <- 1984:1990

NS_biomass_sandeel_cod_sim1_test <- rbind(NS_biomass_sandeel_cod_sim1_data, NS_biomass_sandeel_cod_log)

FMSY_Ensemble_sandeel_cod_data <- EnsembleData(observations = list(NS_biomass_sandeel_cod_sim1_test, Sigma_stock_sandeel_cod),
                                               simulators = list(list(EwE_FMSY_Sandeel_cod_log_mean, EwE_FMSY_Sandeel_cod_covariance_mat, "EwE"),
                                                                 list(fishSUMS_FMSY_sandeels_cod,  Sigma_fs_sandeel_cod, "FishSUMS"),
                                                                 list(LeMans_FMSY_sandeels_cod,  Sigma_lm_sandeel_cod, "LeMans"),
                                                                 list(mizer_FMSY_sandeels_cod, Sigma_miz_sandeel_cod, "mizer")),
                                               priors = FMSY_Ensemble_priors_sandeel_cod)

NS_biomass_sandeel_cod_sim2_data <- matrix(NA, 4, 2)
for(i in 1:4){
  
  NS_biomass_sandeel_cod_sim2_data[i, 1] <- rnorm(1, mean = 13.8, sd = 0.3) 
  NS_biomass_sandeel_cod_sim2_data[i, 2] <- rnorm(1, mean = 11.2, sd = 0.3)
  
}
colnames(NS_biomass_sandeel_cod_sim2_data) <- c("Sandeel", "Cod")
rownames(NS_biomass_sandeel_cod_sim2_data) <- 2014:2017

NS_biomass_sandeel_cod_sim1_sim2_test <- rbind(NS_biomass_sandeel_cod_sim1_test, NS_biomass_sandeel_cod_sim2_data)


FMSY_Ensemble_sandeel_cod_data <- EnsembleData(observations = list(NS_biomass_sandeel_cod_sim1_sim2_test, Sigma_stock_sandeel_cod),
                                               simulators = list(list(EwE_FMSY_Sandeel_cod_log_mean, EwE_FMSY_Sandeel_cod_covariance_mat, "EwE"),
                                                                 list(fishSUMS_FMSY_sandeels_cod,  Sigma_fs_sandeel_cod, "FishSUMS"),
                                                                 list(LeMans_FMSY_sandeels_cod,  Sigma_lm_sandeel_cod, "LeMans"),
                                                                 list(mizer_FMSY_sandeels_cod, Sigma_miz_sandeel_cod, "mizer")),
                                               priors = FMSY_Ensemble_priors_sandeel_cod)

# another thought --- maybe the ensemble model cannot run up to year 100?
# I will supply data up to year 2050, and see how that goes

# this means running each simulator again with everything ending 50 years
# earlier, or maybe just taking the same simulations and removing
# the final 50 years

EwE_FMSY_Sandeel_cod_log_mean_50yrs <- as.data.frame(EwE_FMSY_Sandeel_cod_log_mean[-c(61:110), ])
fishSUMS_FMSY_sandeels_cod_50yrs <- as.data.frame(fishSUMS_FMSY_sandeels_cod[-c(68:117), ])
LeMans_FMSY_sandeels_cod_50yrs <- as.data.frame(LeMans_FMSY_sandeels_cod[-c(66:115), ])
mizer_FMSY_sandeels_cod_50yrs <- as.data.frame(mizer_FMSY_sandeels_cod[-c(68:117), ])

FMSY_Ensemble_sandeel_cod_50yrs_data <- EnsembleData(observations = list(as.data.frame(NS_biomass_sandeel_cod_sim1_sim2_test), Sigma_stock_sandeel_cod),
                                               simulators = list(list(EwE_FMSY_Sandeel_cod_log_mean_50yrs, EwE_FMSY_Sandeel_cod_covariance_mat, "EwE"),
                                                                 list(fishSUMS_FMSY_sandeels_cod_50yrs,  Sigma_fs_sandeel_cod, "FishSUMS"),
                                                                 list(LeMans_FMSY_sandeels_cod_50yrs,  Sigma_lm_sandeel_cod, "LeMans"),
                                                                 list(mizer_FMSY_sandeels_cod_50yrs, Sigma_miz_sandeel_cod, "mizer")),
                                               priors = FMSY_Ensemble_priors_sandeel_cod)

FMSY_Ensemble_sandeel_cod_50yrs_fit <- fit_ensemble_model(observations = list(as.data.frame(NS_biomass_sandeel_cod_sim1_sim2_test), Sigma_stock_sandeel_cod),
                                                          simulators = list(list(EwE_FMSY_Sandeel_cod_log_mean_50yrs, EwE_FMSY_Sandeel_cod_covariance_mat, "EwE"),
                                                                            list(fishSUMS_FMSY_sandeels_cod_50yrs,  Sigma_fs_sandeel_cod, "FishSUMS"),
                                                                            list(LeMans_FMSY_sandeels_cod_50yrs,  Sigma_lm_sandeel_cod, "LeMans"),
                                                                            list(mizer_FMSY_sandeels_cod_50yrs, Sigma_miz_sandeel_cod, "mizer")),
                                                          priors = FMSY_Ensemble_priors_sandeel_cod,
                                                          full_sample = FALSE)

# so the issue, using traceback() is within the function
# generate_simulator_stan_data, which needs the ::: to be seen
# note specifically that the errors is as follows:
# Error in if (nrow(dat_for_year) == 1) { : argument is of length zero


# here is the function
#function (observations, simulators) 
#{
#  N <- ncol(observations[[1]])
#  M <- length(simulators) 
#  times <- c()
#  model_num_species <- rep(NA, M)
#  Ms <- matrix(NA, nrow = 0, ncol = N)
#  model_covariances <- c()
#  for (i in 1:M) {
#    sim <- simulators[[i]]
#    model_ouput <- sim[[1]]
#    times <- unique(append(times, rownames(model_ouput)))
#    model_num_species[i] <- ncol(model_ouput)
#    Mi <- matrix(0, nrow = model_num_species[i], ncol = N)
#    observed_species <- colnames(observations[[1]])
#    sim_species <- colnames(model_ouput)
#    for (k in 1:model_num_species[i]) {
#      for (l in 1:N) {
#        Mi[k, l] <- sim_species[k] == observed_species[l]
#      }
#    }
#    Ms <- rbind(Ms, Mi)
#    model_covariances <- append(model_covariances, as.numeric(sim[[2]]))
#  }
#  times <- sort(as.integer(times))
#  observation_times <- matrix(NA, nrow = length(times), ncol = 0)
#  model_outputs <- matrix(NA, nrow = length(times), ncol = 0)
#  observation_times <- cbind(observation_times, times %in% 
#                               rownames(observations[[1]]))
#  for (i in 1:M) {
#    sim <- simulators[[i]]
#    model_ouput <- sim[[1]]
#    present_data <- as.numeric(times %in% rownames(model_ouput))
#    observation_times <- cbind(observation_times, present_data)
#    y_i <- matrix(0, nrow = length(times), ncol = ncol(model_ouput))
#    for (k in 1:length(times)) {
#      year = times[k]
#      dat_for_year <- model_ouput[year == rownames(model_ouput), 
#      ]
#      if (nrow(dat_for_year) == 1) {
#        y_i[k, ] <- unlist(dat_for_year)
#      }
#    }
#   model_outputs <- cbind(model_outputs, y_i)
#  }
#  if (M == 1) {
#    model_num_species = array(model_num_species, 1)
#  }
#  obs_data <- observations[[1]]
#  obs_data_all <- matrix(0, nrow = length(times), ncol = N)
#  for (k in 1:length(times)) {
#    year = times[k]
#    obs_for_year <- obs_data[year == rownames(obs_data), 
#    ]
#    if (nrow(obs_for_year) == 1) {
#      obs_data_all[k, ] <- unlist(obs_for_year)
#    }
#  }
#  obs_covariances <- observations[[2]]
#  return(list(N = N, time = length(times), M = M, model_num_species = model_num_species, 
#              Ms = Ms, observation_times = observation_times, model_outputs = model_outputs, 
#              model_covariances = model_covariances, observations = obs_data_all, 
#              obs_covariances = obs_covariances))
#}

EwE_FMSY_Sandeels_log_mean_mat[FALSE, ]
EwE_FMSY_Sandeel_cod_log_mean_50yrs[FALSE, ]

str(EwE_FMSY_Sandeels_log_mean_mat)
str(EwE_FMSY_Sandeel_cod_log_mean_50yrs)


FMSY_Ensemble_sandeel_data <- EnsembleData(observations = list(NS_biomass_sandeel_log, Sigma_sandeel_stock),
                                           simulators = list(list(EwE_FMSY_Sandeels_log_mean_mat, EwE_FMSY_Sandeels_log_var_mean_mat, "EwE"),
                                                             list(fishSUMS_FMSY_sandeels_mat,  Sigma_sandeel_FishSUMS, "FishSUMS"),
                                                             list(LeMans_FMSY_sandeels_mat,  Sigma_sandeel_LeMans, "LeMans"),
                                                             list(mizer_FMSY_sandeels_mat, Sigma_sandeel_mizer, "mizer")),
                                           priors = FMSY_Ensemble_priors)

  N <- ncol(list(NS_biomass_sandeel_cod_sim1_sim2_test, Sigma_stock_sandeel_cod)[[1]])
  M <- length(list(list(EwE_FMSY_Sandeel_cod_log_mean_50yrs, EwE_FMSY_Sandeel_cod_covariance_mat, "EwE"),
                   list(fishSUMS_FMSY_sandeels_cod_50yrs,  Sigma_fs_sandeel_cod, "FishSUMS"),
                   list(LeMans_FMSY_sandeels_cod_50yrs,  Sigma_lm_sandeel_cod, "LeMans"),
                   list(mizer_FMSY_sandeels_cod_50yrs, Sigma_miz_sandeel_cod, "mizer"))) 
  times <- c()
  model_num_species <- rep(NA, M)
  Ms <- matrix(NA, nrow = 0, ncol = N)
  model_covariances <- c()
  for (i in 1:M) {
    sim <- list(list(EwE_FMSY_Sandeel_cod_log_mean_50yrs, EwE_FMSY_Sandeel_cod_covariance_mat, "EwE"),
                list(fishSUMS_FMSY_sandeels_cod_50yrs,  Sigma_fs_sandeel_cod, "FishSUMS"),
                list(LeMans_FMSY_sandeels_cod_50yrs,  Sigma_lm_sandeel_cod, "LeMans"),
                list(mizer_FMSY_sandeels_cod_50yrs, Sigma_miz_sandeel_cod, "mizer"))[[i]]
    model_ouput <- sim[[1]]
    times <- unique(append(times, rownames(model_ouput)))
    model_num_species[i] <- ncol(model_ouput)
    Mi <- matrix(0, nrow = model_num_species[i], ncol = N)
    observed_species <- colnames(list(NS_biomass_sandeel_cod_sim1_sim2_test, Sigma_stock_sandeel_cod)[[1]])
    sim_species <- colnames(model_ouput)
    for (k in 1:model_num_species[i]) {
      for (l in 1:N) {
        Mi[k, l] <- sim_species[k] == observed_species[l]
      }
    }
    Ms <- rbind(Ms, Mi)
    model_covariances <- append(model_covariances, as.numeric(sim[[2]]))
  }
  times <- sort(as.integer(times))
  observation_times <- matrix(NA, nrow = length(times), ncol = 0)
  model_outputs <- matrix(NA, nrow = length(times), ncol = 0)
  observation_times <- cbind(observation_times, times %in% 
                               rownames(list(NS_biomass_sandeel_cod_sim1_sim2_test, Sigma_stock_sandeel_cod)[[1]]))
  for (i in 1:1) {
    sim <- list(list(EwE_FMSY_Sandeel_cod_log_mean_50yrs, EwE_FMSY_Sandeel_cod_covariance_mat, "EwE"),
                list(fishSUMS_FMSY_sandeels_cod_50yrs,  Sigma_fs_sandeel_cod, "FishSUMS"),
                list(LeMans_FMSY_sandeels_cod_50yrs,  Sigma_lm_sandeel_cod, "LeMans"),
                list(mizer_FMSY_sandeels_cod_50yrs, Sigma_miz_sandeel_cod, "mizer"))[[i]]
    model_ouput <- sim[[1]]
    present_data <- as.numeric(times %in% rownames(model_ouput))
    observation_times <- cbind(observation_times, present_data)
    y_i <- matrix(0, nrow = length(times), ncol = ncol(model_ouput))
    for (k in 1:1
         #length(times)
         ) {
      year = times[k]
      dat_for_year <- model_ouput[year == rownames(model_ouput), 
      ]
      if (nrow(dat_for_year) == 1) { # nrow(dat_for_year) is NULL
        y_i[k, ] <- unlist(dat_for_year)
      }
    }
   model_outputs <- cbind(model_outputs, y_i)
  }
  if (M == 1) {
    model_num_species = array(model_num_species, 1)
  }
  obs_data <- observations[[1]]
  obs_data_all <- matrix(0, nrow = length(times), ncol = N)
  for (k in 1:length(times)) {
    year = times[k]
    obs_for_year <- obs_data[year == rownames(obs_data), 
    ]
    if (nrow(obs_for_year) == 1) {
      obs_data_all[k, ] <- unlist(obs_for_year)
    }
  }
  obs_covariances <- observations[[2]]
  return(list(N = N, time = length(times), M = M, model_num_species = model_num_species, 
              Ms = Ms, observation_times = observation_times, model_outputs = model_outputs, 
              model_covariances = model_covariances, observations = obs_data_all, 
              obs_covariances = obs_covariances))
  
# having looked through this function, i think the issue now is that
# I'm inputting data as matrices (which is okay according to the help),
# but I think these need to be data frames.
  
  N <- ncol(list(NS_biomass_sandeel_log, Sigma_sandeel_stock)[[1]])
  M <- length(list(list(EwE_FMSY_Sandeels_log_mean_mat, EwE_FMSY_Sandeels_log_var_mean_mat, "EwE"),
                   list(fishSUMS_FMSY_sandeels_mat,  Sigma_sandeel_FishSUMS, "FishSUMS"),
                   list(LeMans_FMSY_sandeels_mat,  Sigma_sandeel_LeMans, "LeMans"),
                   list(mizer_FMSY_sandeels_mat, Sigma_sandeel_mizer, "mizer"))) 
  times <- c()
  model_num_species <- rep(NA, M)
  Ms <- matrix(NA, nrow = 0, ncol = N)
  model_covariances <- c()
  for (i in 1:M) {
    sim <- list(list(EwE_FMSY_Sandeels_log_mean_mat, EwE_FMSY_Sandeels_log_var_mean_mat, "EwE"),
                list(fishSUMS_FMSY_sandeels_mat,  Sigma_sandeel_FishSUMS, "FishSUMS"),
                list(LeMans_FMSY_sandeels_mat,  Sigma_sandeel_LeMans, "LeMans"),
                list(mizer_FMSY_sandeels_mat, Sigma_sandeel_mizer, "mizer"))[[i]]
    model_ouput <- sim[[1]]
    times <- unique(append(times, rownames(model_ouput)))
    model_num_species[i] <- ncol(model_ouput)
    Mi <- matrix(0, nrow = model_num_species[i], ncol = N)
    observed_species <- colnames(list(NS_biomass_sandeel_cod_sim1_sim2_test, Sigma_stock_sandeel_cod)[[1]])
    sim_species <- colnames(model_ouput)
    for (k in 1:model_num_species[i]) {
      for (l in 1:N) {
        Mi[k, l] <- sim_species[k] == observed_species[l]
      }
    }
    Ms <- rbind(Ms, Mi)
    model_covariances <- append(model_covariances, as.numeric(sim[[2]]))
  }
  times <- sort(as.integer(times))
  observation_times <- matrix(NA, nrow = length(times), ncol = 0)
  model_outputs <- matrix(NA, nrow = length(times), ncol = 0)
  observation_times <- cbind(observation_times, times %in% 
                               rownames(list(NS_biomass_sandeel_log, Sigma_sandeel_stock)[[1]]))
  for (i in 1:1
       #M
       ) {
    sim <- list(list(EwE_FMSY_Sandeels_log_mean_mat, EwE_FMSY_Sandeels_log_var_mean_mat, "EwE"),
                list(fishSUMS_FMSY_sandeels_mat,  Sigma_sandeel_FishSUMS, "FishSUMS"),
                list(LeMans_FMSY_sandeels_mat,  Sigma_sandeel_LeMans, "LeMans"),
                list(mizer_FMSY_sandeels_mat, Sigma_sandeel_mizer, "mizer"))[[i]]
    model_ouput <- sim[[1]]
    present_data <- as.numeric(times %in% rownames(model_ouput))
    observation_times <- cbind(observation_times, present_data)
    y_i <- matrix(0, nrow = length(times), ncol = ncol(model_ouput))
    for (k in 1:1
         #length(times)
         ) {
      year = times[k]
      dat_for_year <- model_ouput[year == rownames(model_ouput), 
      ]
      if (nrow(dat_for_year) == 1) { # nrow(dat_for_year) is NULL
        y_i[k, ] <- unlist(dat_for_year)
      }
    }
    model_outputs <- cbind(model_outputs, y_i)
  }
  if (M == 1) {
    model_num_species = array(model_num_species, 1)
  }
  obs_data <- observations[[1]]
  obs_data_all <- matrix(0, nrow = length(times), ncol = N)
  for (k in 1:length(times)) {
    year = times[k]
    obs_for_year <- obs_data[year == rownames(obs_data), 
    ]
    if (nrow(obs_for_year) == 1) {
      obs_data_all[k, ] <- unlist(obs_for_year)
    }
  }
  obs_covariances <- observations[[2]]
  return(list(N = N, time = length(times), M = M, model_num_species = model_num_species, 
              Ms = Ms, observation_times = observation_times, model_outputs = model_outputs, 
              model_covariances = model_covariances, observations = obs_data_all, 
              obs_covariances = obs_covariances))