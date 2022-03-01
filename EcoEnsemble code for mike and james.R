library("EcoEnsemble")

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

# fish species occurring in at least two simulators:
# sandeels, sprat, norway pout, herring, dab, whiting, sole, gurnards,
# plaice, haddock, cod, saithe, nephrops

N_species <- 13

# To avoid sending you mass amounts of files containing the other data,
# I will just simulate values to represent the data

set.seed(99)

stock_SSB <- data.frame(Sandeel = rnorm(23, mean = 14.1, sd = 0.52), Sprat = rnorm(23, mean = 14.1, sd = 0.37),
                        N.pout = rnorm(23, mean = 13.2, sd = 0.47), Herring = rnorm(23, mean = 15.2, sd = 0.22),
                        Dab = rnorm(23, mean = 10.3, sd = 0.21), Whiting = rnorm(23, mean = 13.4, sd = 0.33),
                        Sole = rnorm(23, mean = 11.0, sd = 0.33), Gurnard = rnorm(23, mean = 12.6, sd = 0.38),
                        Plaice = rnorm(23, mean = 12.7, sd = 0.34), Haddock = rnorm(23, mean = 13.5, sd = 0.47),
                        Cod = rnorm(23, mean = 12.5, sd = 0.48), Saithe = rnorm(23, mean = 13.0, sd = 0.20),
                        Nephrops = rnorm(23, mean = 2.9, sd = 0.50))
rownames(stock_SSB) <- 1991:2013


EwE_SSB <- data.frame(Sandeel = rnorm(87, mean = 13.6, sd = 0.26), Sprat = rnorm(87, mean = 12.7, sd = 0.44),
                      N.pout = rnorm(87, mean = 13.5, sd = 0.13), Herring = rnorm(87, mean = 14.3, sd = 0.22),
                      Dab = rnorm(87, mean = 14.3, sd = 0.04), Whiting = rnorm(87, mean = 6.8, sd = 2.38),
                      Sole = rnorm(87, mean = 12.1, sd = 1.02), Gurnard = rnorm(87, mean = 11.3, sd = 0.20),
                      Plaice = rnorm(87, mean = 14.1, sd = 0.55), Haddock = rnorm(87, mean = 12.0, sd = 0.27),
                      Cod = rnorm(87, mean = 12.2, sd = 0.17), Saithe = rnorm(87, mean = 12.4, sd = 0.11),
                      Nephrops = rnorm(87, mean = 12.9, sd = 0.15))
rownames(EwE_SSB) <- 1991:2077

mizer_SSB <- data.frame(Sandeel = rnorm(94, mean = 14.1, sd = 0.39), Sprat = rnorm(94, mean = 10.8, sd = 0.30),
                        N.pout = rnorm(94, mean = 12.5, sd = 0.23), Herring = rnorm(94, mean = 13.9, sd = 0.17),
                        Dab = rnorm(94, mean = 9.4, sd = 0.51), Whiting = rnorm(94, mean = 11.0, sd = 0.21),
                        Sole = rnorm(94, mean = 11.0, sd = 0.10), Gurnard = rnorm(94, mean = 7.3, sd = 0.28),
                        Plaice = rnorm(94, mean = 11.6, sd = 0.35), Haddock = rnorm(94, mean = 11.5, sd = 0.46),
                        Cod = rnorm(94, mean = 11.5, sd = 0.85), Saithe = rnorm(94, mean = 12.1, sd = 0.15))
rownames(mizer_SSB) <- 1984:2077

fishSUMS_SSB <- data.frame(Sandeel = rnorm(94, mean = 12.1, sd = 0.11), N.pout = rnorm(94, mean = 12.1, sd = 0.23),
                           Herring = rnorm(94, mean = 14.2, sd = 0.13), Dab = rnorm(94, mean = 10.4, sd = 0.29),
                           Whiting = rnorm(94, mean = 13.3, sd = 0.25), Gurnard = rnorm(94, mean = 10.4, sd = 0.07),
                        Plaice = rnorm(94, mean = 12.7, sd = 0.52), Haddock = rnorm(94, mean = 13.5, sd = 0.20),
                        Cod = rnorm(94, mean = 12.1, sd = 1.59), Saithe = rnorm(94, mean = 11.8, sd = 0.24),
                        Nephrops = rnorm(94, mean = 10.0, sd = 0.79))
rownames(fishSUMS_SSB) <- 1984:2077

LeMans_SSB <- data.frame(Sandeel = rnorm(92, mean = 13.0, sd = 0.04), N.pout = rnorm(92, mean = 13.1, sd = 0.21),
                           Herring = rnorm(92, mean = 15.7, sd = 0.06), Whiting = rnorm(92, mean = 13.4, sd = 0.06),
                         Sole = rnorm(92, mean = 11.8, sd = 0.07), Plaice = rnorm(92, mean = 13.5, sd = 0.06),
                         Haddock = rnorm(92, mean = 13.6, sd = 0.04), Cod = rnorm(92, mean = 12.1, sd = 0.06),
                         Saithe = rnorm(92, mean = 13.8, sd = 0.04))
rownames(LeMans_SSB) <- 1986:2077

# simulating parameter uncertainty covariance matrices

stock_Sigma <- matrix(rWishart(1, df = ncol(stock_SSB), Sigma = diag(ncol(stock_SSB))/ 100), ncol(stock_SSB), ncol(stock_SSB))
EwE_Sigma <- matrix(rWishart(1, df = ncol(EwE_SSB), Sigma = diag(ncol(EwE_SSB))/ 100), ncol(EwE_SSB), ncol(EwE_SSB))
mizer_Sigma <- matrix(rWishart(1, df = ncol(mizer_SSB), Sigma = diag(ncol(mizer_SSB))/ 100), ncol(mizer_SSB), ncol(mizer_SSB))
LeMans_Sigma <- matrix(rWishart(1, df = ncol(LeMans_SSB), Sigma = diag(ncol(LeMans_SSB))/ 100), ncol(LeMans_SSB), ncol(LeMans_SSB))
fishSUMS_Sigma <- matrix(rWishart(1, df = ncol(fishSUMS_SSB), Sigma = diag(ncol(fishSUMS_SSB))/ 100), ncol(fishSUMS_SSB), ncol(fishSUMS_SSB))

colnames(stock_Sigma) <- colnames(stock_SSB)
rownames(stock_Sigma) <- colnames(stock_SSB)
colnames(EwE_Sigma) <- colnames(EwE_SSB)
rownames(EwE_Sigma) <- colnames(EwE_SSB)
colnames(mizer_Sigma) <- colnames(mizer_SSB)
rownames(mizer_Sigma) <- colnames(mizer_SSB)
colnames(LeMans_Sigma) <- colnames(LeMans_SSB)
rownames(LeMans_Sigma) <- colnames(LeMans_SSB)
colnames(fishSUMS_Sigma) <- colnames(fishSUMS_SSB)
rownames(fishSUMS_Sigma) <- colnames(fishSUMS_SSB)

Ensemble_priors <- define_priors(ind_st_var_params = list(25, 0.25),
                                         ind_st_cor_form = "lkj", #Using an LKJ distribution for individual short-term discrepancies
                                         ind_st_cor_params = 30, #The parameter is 30
                                         ind_lt_var_params = list(rep(25, N_species),rep(0.25, N_species)),
                                         ind_lt_cor_form = "beta",
                                         ind_lt_cor_params = list(matrix(40, N_species, N_species), matrix(40, N_species, N_species)),
                                         sha_st_var_exp = 3,
                                         sha_st_cor_form = "lkj",
                                         sha_st_cor_params = 30,
                                         sha_lt_sd = rep(4, N_species))

Ensemble_fit <- fit_ensemble_model(observations = list(stock_SSB, stock_Sigma),
                                           simulators = list(list(EwE_SSB, EwE_Sigma, "EwE"),
                                                             list(fishSUMS_SSB,  fishSUMS_Sigma, "FishSUMS"),
                                                             list(LeMans_SSB,  LeMans_Sigma, "LeMans"),
                                                             list(mizer_SSB, mizer_Sigma, "mizer")),
                                           priors = Ensemble_priors,
                                           full_sample = FALSE)

# Ensemble fit fails with error:

# Error in sampler$call_sampler(c(args, dotlist)) : Initialization failed.

# Instead fit something with, say, 9 species instead of 13
# (have to be careful to choose the right species from each
# simulator):

Ensemble_priors_v2 <- define_priors(ind_st_var_params = list(25, 0.25),
                                 ind_st_cor_form = "lkj", #Using an LKJ distribution for individual short-term discrepancies
                                 ind_st_cor_params = 30, #The parameter is 30
                                 ind_lt_var_params = list(rep(25, 9),rep(0.25, 9)),
                                 ind_lt_cor_form = "beta",
                                 ind_lt_cor_params = list(matrix(40, 9, 9), matrix(40, 9, 9)),
                                 sha_st_var_exp = 3,
                                 sha_st_cor_form = "lkj",
                                 sha_st_cor_params = 30,
                                 sha_lt_sd = rep(4, 9))

Ensemble_fit_v2 <- fit_ensemble_model(observations = list(stock_SSB[, 1:9], stock_Sigma[1:9, 1:9]),
                                   simulators = list(list(EwE_SSB[, 1:9], EwE_Sigma[1:9, 1:9], "EwE"),
                                                     list(fishSUMS_SSB[, 1:7],  fishSUMS_Sigma[1:7, 1:7], "FishSUMS"),
                                                     list(LeMans_SSB[, 1:6],  LeMans_Sigma[1:6, 1:6], "LeMans"),
                                                     list(mizer_SSB[, 1:9], mizer_Sigma[1:9, 1:9], "mizer")),
                                   priors = Ensemble_priors_v2,
                                   full_sample = FALSE)


# Ensemble_fit_v2 runs without error
