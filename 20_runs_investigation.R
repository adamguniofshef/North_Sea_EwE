# We have 20 excel spreadsheets from Jacob, showing the outputs
# from EwE sim (under the basic scenario) for 20 different iterations.
# The iterations differ in that the estimates for the model parameters
# have been assigned some uncertainty (using data pedigree values),
# leading to upper and lower bounds for the parameters for the MCMC
# simulations.

# We firstly want to get this data from each of the 20 runs in R,
# then have a go a reproducing some output from the paper!

base_model_run_1_biomass_annual <- read.csv("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_small_model_set\\EcoSampler\\Base_model\\biomass_annual.csv",
                                            skip = 9)

# base_model_run_1_biomass_annual gives the estimates of 
# annual biomass for all functional groups for all years between 1991 and
# 2100

# would like to get a vector for the names of all the functional groups
# this can be taken from the file  
# C:\\Users\\adamg\\Google Drive\\Natural England project\\Previous report\\NorthSea_EwE_basic_estimates_functional_groups.csv

func_groups_output <- read.csv("C:\\Users\\adamg\\Google Drive\\Natural England project\\Previous report\\NorthSea_EwE_basic_estimates_functional_groups.csv")

# note that some elements are not functional groups here (there are subheadings
# like "cod", which are then split into 2 functional groups for juvenile
# and adult cod, so in this case "cod" needs to be removed)
func_groups_output <- func_groups_output[!is.na(func_groups_output[,1]),]
func_groups <- func_groups_output[,2]
# then set the column names of base_model_run_1_biomass_annual to be
# the func_groups, remembering the years also need to be included as 
# name for first column

# we want to plot from Year 0 to Year 59, like in Figure 7.1. Year 0 correspond to
# 2018, with 2018 being the 28th year of data (chronologically)


colnames(base_model_run_1_biomass_annual) <- c("year",func_groups)
plot((base_model_run_1_biomass_annual$year-2018)[28:87],
     base_model_run_1_biomass_annual$Sandeels[28:87], type = "l",
     ylab = "Sandeel biomass, base model iteration 1",
     xlab = "Project Year")
