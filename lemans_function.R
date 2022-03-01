## using raw R code as this has a function in that is not in the package

load("loadLeMans.Rdata")

## new_fs order -- 1) Sandeel, 2) N. pout, 3) Herring, 4) Whiting, 5) Sole, 6) Plaice 7) Haddock 8) Cod 9) Saithe -- 

## need to load an Rcpp function
library(Rcpp)
library(RcppArmadillo)
sourceCpp('calc_M2.cpp')

get_lemans <- function(new_fs, n_years=31){
  temp <- old_Fs
  temp[c(2, 3, 7, 12, 10, 15, 17, 20, 21)] <- as.numeric(new_fs[c(2, 1, 3, 4,5, 6, 7, 8, 9)])
  # Run the model
  LeMANS_outputs <- getting_mse(temp, lemans_runs1, n_years=n_years)
}

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

test_run_lemans <- get_lemans(new_fs = rep(0, 9), n_years = 31)

test_run_lemans$SSB
# long-term spawning stock biomass for 9 species listed above, in order, 
# from 1986 to 2016 (nyears could be post data like for mizer, in which case
# it would be 1986 to 2050, which would make more sense)
# but it only gives 31 years, so it's either 1986 to 2016, or 2020 to 2050?

test_run_lemans$Yield
# and the same for yield, which is -Inf for all species and all years (?)

# F refers to the fishing mortality for each species, so is a vector of
# length n, where n is the number of species. F lies between 0 and 2