###

install.packages("FishSUMS8_1.0.tar.gz", repos=NULL, type="source")
install.packages("tidyverse")

library(FishSUMS8)
library(tidyverse)
library(scales)

load("loadFishSUMS.Rdata")

spec_nam <-c("SANDEEL" ,"NORWAY_POUT","HERRING","WHITING","PLAICE","HADDOCK","COD","SAITHE")

### Fs is the fising mortality in the order of spec_nam
get_fishsums <- function(Fs, theta, spec_nam, simulation_end_date = 2050){
  run_fishsums(Fs, theta, spec_nam, simulation_end_date)
}
## returns a list of SSB and yield in 000 tonnes

test_run_fishSUMS <- get_fishsums(Fs = rep(1,8), theta = theta, spec_nam = spec_nam)

test_run_fishSUMS$SSB
# the long-term spawning stock biomass for each of the 8 species in spec_nam,
# from 1984 to 2050

test_run_fishSUMS$Yield
# yield for each of the 8 species in spec_nam from 1984 to 2050
# (believe yield is catch)

# F refers to the fishing mortality for each species, so is a vector of
# length n, where n is the number of species. F lies between 0 and 2