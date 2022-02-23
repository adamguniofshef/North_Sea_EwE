## Running mizer

## Requires an old version of mizer
require(devtools)
install_version("mizer", version = "1.0.1", repos = "http://cran.us.r-project.org")

library(mizer)
load("loadmizer.Rdata")
source("multiple_gears.r")

## get_mizer takes inputs Fs for the 9 species in order -- 1) Sandeel, 2) N. pout, 3) Herring, 4) Whiting, 5) Sole, 6) Plaice 7) Haddock 8) Cod 9) Saithe -- # it returns a list of two objects annual SSB and annual yield in log tonnes.
get_mizer <- function(Fs, theta, years_post_data = 31, simulation_end_date = 2050){
  scenario <- c(effort_array[nrow(effort_array), 1], Fs[1:3], effort_array[nrow(effort_array), 5], Fs[4:5], 
                effort_array[nrow(effort_array), 8], Fs[6:9])
  temp <- rbind(effort_array, matrix(get_fut_F(scenario, multiplyer), years_post_data*1/ddt, ncol(effort_array), byrow=T))
  rownames(temp) <- seq(1984, simulation_end_date + 1 - ddt, ddt)
  get_ssb_and_landings_rmax(theta, sim_init, effort_array = temp, spinupF,
                            years = 1984:simulation_end_date)
}

test_run_mizer <- get_mizer(Fs = c(0.3, rep(0,8)), theta = theta)

test_run_mizer$SSB
# long-term spawning stock biomass for fish species (not in the order
# listed above) for sprat, sandeel, norway pout, herring, dab, whiting
# sole, gurnard, plaice, haddock, cod and saithe, from 1984 to 2050

test_run_mizer$Yield
# then the yield too, which is again -Inf

# F refers to the fishing mortality for each species, so is a vector of
# length n, where n is the number of species. F lies between 0 and 2

# theta is a vector of length 55