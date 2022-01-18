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
func_groups_output <- func_groups_output[!is.na(func_groups_output[, 1]), ]
func_groups <- func_groups_output[, 2]
# then set the column names of base_model_run_1_biomass_annual to be
# the func_groups, remembering the years also need to be included as 
# name for first column

# we want to plot from Year 0 to Year 59, like in Figure 7.1. Year 0 correspond to
# 2018, with 2018 being the 28th year of data (chronologically)

# As mentioned lower down, that data for biomass need to be multiplied
# by 570,000 for post-processing (EwE outputs give biomass in terms of
# tonnes per km^2, so we want biomass in tonnes).

# Will only do this now for sandeels, as unsure which other functional
# groups this affects.

# UPDATE 18/01 --- confident this is true for all species, so will does this for all data

base_model_run_1_biomass_annual$Sandeels <- base_model_run_1_biomass_annual$Sandeels * 570000


colnames(base_model_run_1_biomass_annual) <- c("year", func_groups)
plot((base_model_run_1_biomass_annual$year - 2018)[28:87],
     base_model_run_1_biomass_annual$Sandeels[28:87], type = "l",
     ylab = "Sandeel biomass, base model iteration 1",
     xlab = "Project Year")

# Will try to plot all 20 iterations on the same plot to get an idea
# of the uncertainty on the estimate of sandeel annual biomass

dim(base_model_run_1_biomass_annual) # 110 x 70
base_model_all_runs_biomass_annual <- vector("list", length = 20)
base_model_all_runs_biomass_annual[[1]] <- base_model_run_1_biomass_annual
colnames(base_model_all_runs_biomass_annual[[1]]) <- c("year", func_groups)

for(i in 1:19){
  
  base_model_all_runs_biomass_annual[[i + 1]] <- read.csv(paste0("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_small_model_set\\EcoSampler\\Run_",i,"\\biomass_annual.csv"),
                                                            skip = 9)
  colnames(base_model_all_runs_biomass_annual[[i + 1]]) <- c("year", func_groups)
  base_model_all_runs_biomass_annual[[i + 1]]$Sandeels <- base_model_all_runs_biomass_annual[[i + 1]]$Sandeels * 570000
  
}

# have all 20 runs together in a list. want the plot limits to be sensible
# so will find the minimum and maximum annual biomass from each run.
# since I am using a list, will need to find the minimum from each list 
# element, then find the minimum of the minimums

min_biomass_annual_sandeel_Year_0_60 <- rep(NA,20)
max_biomass_annual_sandeel_Year_0_60 <- rep(NA,20)
for(i in 1:20){
  
  min_biomass_annual_sandeel_Year_0_60[i] <- min(base_model_all_runs_biomass_annual[[i]]$Sandeels[28:87])
  max_biomass_annual_sandeel_Year_0_60[i] <- max(base_model_all_runs_biomass_annual[[i]]$Sandeels[28:87])
  
}

options(scipen=10000)
plot((base_model_all_runs_biomass_annual[[1]]$year - 2018)[28:87],
     base_model_all_runs_biomass_annual[[1]]$Sandeels[28:87], type = "l",
     ylab = "Sandeel biomass, base model, all 20 iterations",
     xlab = "Project Year", ylim = c(min(min_biomass_annual_sandeel_Year_0_60),
                                     max(max_biomass_annual_sandeel_Year_0_60)))

for(i in 1:19){
  
  lines((base_model_all_runs_biomass_annual[[i + 1]]$year - 2018)[28:87],
        base_model_all_runs_biomass_annual[[i + 1]]$Sandeels[28:87])
  
}

# this data may need the post-processing of multiplying by 570000,
# according to the paper from cefas (unsure whether sandeels come under
# the category of species without annual time-series estimates of biomass
# being available)

# there looks to be other possible scaling changes when comparing with
# Figure 7.1 of the NCA report --- the annual biomass reported by the EwE
# output could be tonnes per km^2 (i.e., biomass in habitat area), instead
# of tonnes.

# UPDATE: it is the case that the biomass data (and catch data) need to 
# be multiplied by 570,000km^2. Will correct that for the data.

# It is clear from the plot of all iterations that there is plenty of
# variation between the variations for sandeel biomass. We are
# interested in seeing this more at the end points, since it seems the
# time series settles to a steady state. An histogram of the 20 endpoints
# i.e., the annual biomass of sandeels in year 59, is given below.

# Note that Project Year 59 is the 87th row of the biomass matrices)
(base_model_all_runs_biomass_annual[[1]]$year - 2018)[87] # [1] 59

base_model_all_runs_biomass_year_59 <- rep(NA,20)

for(i in 1:20){
  
  base_model_all_runs_biomass_year_59[i] <- base_model_all_runs_biomass_annual[[i]]$Sandeels[87]
  
}
hist(base_model_all_runs_biomass_year_59, #ylab = "Density",
     xlab = "Sandeel biomass in Year 59",
     main = "", breaks = seq(450000,800000,25000))

# we can also look at the effects on other species too.
# want to group together the functional groups according to a few things
# whether they have a sandeel diet, whether the fish is pelagic, demersal
# or shellfish.

# sandeel diet (>10% sandeel diet):
# pelagic --- mackerel
# demersal --- cod, gurnards, haddock, halibut, turbot, whiting, starry ray + others,
# skate + cuckoo ray, monkfish

# other fish species (<10% sandeel diet):
# pelagic --- blue whiting, horse mackerel, herring, sprat
# demersal ---  dab, sole, plaice, flounder, lemon sole, norway pout
# megrim, saithe, thornback & spotted ray, witch, spurdog, small sharks, large piscivorous sharks
# long-rough dab
# shellfish --- large crabs, nephrops, shrimp, squid & cuttlefish

# the fish groups are given in the workbook Sandeel NCA final workbook

# In order to try to plot things effectively, given the 20 iterations 
# and wanting to plot sandeels, pelagic, demersal, shellfish all on one
# plot, I think I will try to take the mean of the 20 iterations then
# plot 95% confidence intervals for the data given the 20 iterations,
# plotting annual biomass against project year

# Need to find out whether the rest of the fish need to be multiplied by
# 570,000 km^2 as post-processing

# so firstly trying to produce Figure 7.1 using the 20 iterations,
# and for each fish group plotting the mean and 95% confidence intervals

# before doing this, will add an additional row to the data to represent
# which fish group the functional group belong, and another to represent
# whether the fish group has a sandeel diet

fish_group <- c(rep(NA, 6), rep("demersal", 3), NA, rep("demersal", 11), NA, "pelagic", "demersal",
                rep(NA, 2), rep("demersal", 2), rep("pelagic", 5), "sandeel", rep("demersal", 10),
                rep(NA, 2), rep("demersal", 2), "pelagic", "shellfish", rep(NA,4), rep("shellfish", 2),
                rep(NA, 2), "shellfish", rep(NA, 11))
length(fish_group)
sandeel_diet <- c(NA, rep(F, 9), T, F, rep(T, 7), rep(F, 7), rep(T, 2), rep(F, 3),
                  T, rep(F, 9), T, F, T, rep(F, 26))
length(sandeel_diet)

for(i in 1:20){
  
  base_model_all_runs_biomass_annual[[i]] <- rbind(base_model_all_runs_biomass_annual[[i]], fish_group)
  base_model_all_runs_biomass_annual[[i]] <- rbind(base_model_all_runs_biomass_annual[[i]], sandeel_diet)

}
base_model_all_runs_biomass_annual[[4]][108:113,1:9]

# so, row 111 of each list element denotes whether the functional group is
# demersal, pelagic, sandeel, or shellfish, or if the group doesn't fit into
# any of these categories, NA

# row 112 denotes whether the functional group feeds on sandeel (T or F),
# with the year column having NA

base_model_all_runs_biomass_annual[[1]][,which(base_model_all_runs_biomass_annual[[1]][111,] == "pelagic")]

# sum of the rows for above matrix (excluding rows that refer to fish group and sandeel diet) will give
# the number of pelagic fish for that year, but also need to be rescaled according to post-processing
# which I assume will be multiplying by 570,000

# will do this for each element in the list, then for the mean over the list and
# then produce some CIs over the 20 iterations for annual biomass of pelagic fish

length(which(base_model_all_runs_biomass_annual[[1]][111,] == "pelagic")) # 7 pelagic species

base_model_all_runs_biomass_annual_pelagic <- vector("list", 20)
base_model_all_runs_biomass_annual_pelagic_array <- array(NA, dim = c(110, 7, 20)) # some issues with lists and character strings, so this is a workaround
base_model_all_runs_biomass_annual_pelagic_sum <- matrix(NA, 110, 20) # 110 years, 20 iterations


for(i in 1:20){
  
  base_model_all_runs_biomass_annual_pelagic[[i]] <- base_model_all_runs_biomass_annual[[i]][,which(base_model_all_runs_biomass_annual[[1]][111,] == "pelagic")]
  base_model_all_runs_biomass_annual_pelagic_array[, , i] <- matrix(as.numeric(as.matrix(base_model_all_runs_biomass_annual_pelagic[[i]][1:110, ])), 110, 7)
  base_model_all_runs_biomass_annual_pelagic_sum[, i] <- apply(base_model_all_runs_biomass_annual_pelagic_array[, , i], 1, sum)
  
}


base_model_all_runs_biomass_annual_pelagic_sum 
# this is the pelagic fish biomass for each year
# from 1991 to 2100 for each of the 20 iterations

# want the mean for each year over the 20 iterations and the sd
# in order to put confidence intervals on the plot

base_model_all_runs_biomass_annual_pelagic_20_iterations_mean <- apply(base_model_all_runs_biomass_annual_pelagic_sum, 1, mean)
base_model_all_runs_biomass_annual_pelagic_20_iterations_sd <- apply(base_model_all_runs_biomass_annual_pelagic_sum, 1, sd)

base_model_all_runs_biomass_annual_pelagic_20_iterations_95_lower <- base_model_all_runs_biomass_annual_pelagic_20_iterations_mean - 1.96*base_model_all_runs_biomass_annual_pelagic_20_iterations_sd
base_model_all_runs_biomass_annual_pelagic_20_iterations_95_upper <- base_model_all_runs_biomass_annual_pelagic_20_iterations_mean + 1.96*base_model_all_runs_biomass_annual_pelagic_20_iterations_sd

as.numeric(base_model_all_runs_biomass_annual[[1]]$Sandeels)[1:110]

base_model_all_runs_biomass_annual_sandeel_20_iterations <- matrix(NA, 110, 20)

for(i in 1:20){
  
  base_model_all_runs_biomass_annual_sandeel_20_iterations[, i] <- as.numeric(base_model_all_runs_biomass_annual[[i]]$Sandeels[1:110])
  
}

base_model_all_runs_biomass_annual_sandeel_20_iterations_mean <- apply(base_model_all_runs_biomass_annual_sandeel_20_iterations, 1, mean)
base_model_all_runs_biomass_annual_sandeel_20_iterations_sd <- apply(base_model_all_runs_biomass_annual_sandeel_20_iterations, 1, sd)

base_model_all_runs_biomass_annual_sandeel_20_iterations_95_lower <- base_model_all_runs_biomass_annual_sandeel_20_iterations_mean - 1.96*base_model_all_runs_biomass_annual_sandeel_20_iterations_sd
base_model_all_runs_biomass_annual_sandeel_20_iterations_95_upper <- base_model_all_runs_biomass_annual_sandeel_20_iterations_mean + 1.96*base_model_all_runs_biomass_annual_sandeel_20_iterations_sd


options(scipen=10000)
plot((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
     base_model_all_runs_biomass_annual_sandeel_20_iterations_mean[28:87], type = "l",
     ylab = "Annual biomass, base model",
     xlab = "Project Year", ylim = c(min(base_model_all_runs_biomass_annual_sandeel_20_iterations_95_lower),
                                     570000*max(base_model_all_runs_biomass_annual_pelagic_20_iterations_95_upper)))
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      base_model_all_runs_biomass_annual_sandeel_20_iterations_95_lower[28:87], lty = 2)
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      base_model_all_runs_biomass_annual_sandeel_20_iterations_95_upper[28:87], lty = 2)
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      570000*base_model_all_runs_biomass_annual_pelagic_20_iterations_mean[28:87], lty = 1, col = "red")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      570000*base_model_all_runs_biomass_annual_pelagic_20_iterations_95_lower[28:87], lty = 2, col = "red")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      570000*base_model_all_runs_biomass_annual_pelagic_20_iterations_95_upper[28:87], lty = 2, col = "red")
abline(h=3500000,col="grey")
legend("right",legend = c("pelagic mean","pelagic 95% CI","sandeel mean", "sandeel 95% CI"),
       lty = c(1, 2, 1, 2), col = c(2, 2, 1, 1))

# now to do the same for demersal and shellfish, plot these alongside sandeels 
# and pelagic.

length(which(base_model_all_runs_biomass_annual[[1]][111,] == "demersal")) # 29 demersal species
length(which(base_model_all_runs_biomass_annual[[1]][111,] == "shellfish")) # 4 shellfish species

base_model_all_runs_biomass_annual_demersal <- vector("list", 20)
base_model_all_runs_biomass_annual_demersal_array <- array(NA, dim = c(110, 29, 20)) # some issues with lists and character strings, so this is a workaround
base_model_all_runs_biomass_annual_demersal_sum <- matrix(NA, 110, 20) # 110 years, 20 iterations
base_model_all_runs_biomass_annual_shellfish <- vector("list", 20)
base_model_all_runs_biomass_annual_shellfish_array <- array(NA, dim = c(110, 4, 20)) # some issues with lists and character strings, so this is a workaround
base_model_all_runs_biomass_annual_shellfish_sum <- matrix(NA, 110, 20) # 110 years, 20 iterations


for(i in 1:20){
  
  base_model_all_runs_biomass_annual_demersal[[i]] <- base_model_all_runs_biomass_annual[[i]][,which(base_model_all_runs_biomass_annual[[1]][111,] == "demersal")]
  base_model_all_runs_biomass_annual_demersal_array[, , i] <- matrix(as.numeric(as.matrix(base_model_all_runs_biomass_annual_demersal[[i]][1:110, ])), 110, 29)
  base_model_all_runs_biomass_annual_demersal_sum[, i] <- apply(base_model_all_runs_biomass_annual_demersal_array[, , i], 1, sum)
  base_model_all_runs_biomass_annual_shellfish[[i]] <- base_model_all_runs_biomass_annual[[i]][,which(base_model_all_runs_biomass_annual[[1]][111,] == "shellfish")]
  base_model_all_runs_biomass_annual_shellfish_array[, , i] <- matrix(as.numeric(as.matrix(base_model_all_runs_biomass_annual_shellfish[[i]][1:110, ])), 110, 4)
  base_model_all_runs_biomass_annual_shellfish_sum[, i] <- apply(base_model_all_runs_biomass_annual_shellfish_array[, , i], 1, sum)
  
}


base_model_all_runs_biomass_annual_demersal_sum 
base_model_all_runs_biomass_annual_shellfish_sum 
# these are the demersal fish biomass and shellfish biomass for each year
# from 1991 to 2100 for each of the 20 iterations

# want the mean for each year over the 20 iterations and the sd
# in order to put confidence intervals on the plot

base_model_all_runs_biomass_annual_demersal_20_iterations_mean <- apply(base_model_all_runs_biomass_annual_demersal_sum, 1, mean)
base_model_all_runs_biomass_annual_demersal_20_iterations_sd <- apply(base_model_all_runs_biomass_annual_demersal_sum, 1, sd)

base_model_all_runs_biomass_annual_demersal_20_iterations_95_lower <- base_model_all_runs_biomass_annual_demersal_20_iterations_mean - 1.96*base_model_all_runs_biomass_annual_demersal_20_iterations_sd
base_model_all_runs_biomass_annual_demersal_20_iterations_95_upper <- base_model_all_runs_biomass_annual_demersal_20_iterations_mean + 1.96*base_model_all_runs_biomass_annual_demersal_20_iterations_sd

base_model_all_runs_biomass_annual_shellfish_20_iterations_mean <- apply(base_model_all_runs_biomass_annual_shellfish_sum, 1, mean)
base_model_all_runs_biomass_annual_shellfish_20_iterations_sd <- apply(base_model_all_runs_biomass_annual_shellfish_sum, 1, sd)

base_model_all_runs_biomass_annual_shellfish_20_iterations_95_lower <- base_model_all_runs_biomass_annual_shellfish_20_iterations_mean - 1.96*base_model_all_runs_biomass_annual_shellfish_20_iterations_sd
base_model_all_runs_biomass_annual_shellfish_20_iterations_95_upper <- base_model_all_runs_biomass_annual_shellfish_20_iterations_mean + 1.96*base_model_all_runs_biomass_annual_shellfish_20_iterations_sd



options(scipen=10000)
plot((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
     base_model_all_runs_biomass_annual_sandeel_20_iterations_mean[28:87], type = "l",
     ylab = "Annual biomass, base model", xlab = "Project Year",
     ylim = c(min(base_model_all_runs_biomass_annual_sandeel_20_iterations_95_lower),
                                     570000*max(base_model_all_runs_biomass_annual_demersal_20_iterations_95_upper)),
     col = "blue")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      base_model_all_runs_biomass_annual_sandeel_20_iterations_95_lower[28:87], lty = 2, col = "blue")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      base_model_all_runs_biomass_annual_sandeel_20_iterations_95_upper[28:87], lty = 2, col = "blue")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      570000*base_model_all_runs_biomass_annual_pelagic_20_iterations_mean[28:87], lty = 1, col = "red")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      570000*base_model_all_runs_biomass_annual_pelagic_20_iterations_95_lower[28:87], lty = 2, col = "red")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      570000*base_model_all_runs_biomass_annual_pelagic_20_iterations_95_upper[28:87], lty = 2, col = "red")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      570000*base_model_all_runs_biomass_annual_demersal_20_iterations_mean[28:87], lty = 1, col = "green")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      570000*base_model_all_runs_biomass_annual_demersal_20_iterations_95_lower[28:87], lty = 2, col = "green")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      570000*base_model_all_runs_biomass_annual_demersal_20_iterations_95_upper[28:87], lty = 2, col = "green")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      570000*base_model_all_runs_biomass_annual_shellfish_20_iterations_mean[28:87], lty = 1, col = "orange")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      570000*base_model_all_runs_biomass_annual_shellfish_20_iterations_95_lower[28:87], lty = 2, col = "orange")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      570000*base_model_all_runs_biomass_annual_shellfish_20_iterations_95_upper[28:87], lty = 2, col = "orange")

legend(x = 0, y = 5600000, legend = c("Sandeels", "Demersal", "Pelagic", "Shellfish"),
       col = c("blue", "green", "red", "orange"), lty = c(1, 1, 1, 1), cex = 0.7)

# from conversation with Paul --- shouldn't necessarily assume normality here
# in order to produce the credible intervals. Since the model is deterministic,
# it makes sense to just use quantiles of the data from the 20 iterations.
# can just use the summary command for this


base_model_all_runs_biomass_annual_pelagic_20_iterations_mean <- apply(base_model_all_runs_biomass_annual_pelagic_sum, 1, mean)
base_model_all_runs_biomass_annual_pelagic_20_iterations_5_95_quantiles <- apply(base_model_all_runs_biomass_annual_pelagic_sum, 1, quantile, probs = c(0.025,0.975))

base_model_all_runs_biomass_annual_demersal_20_iterations_mean <- apply(base_model_all_runs_biomass_annual_demersal_sum, 1, mean)
base_model_all_runs_biomass_annual_demersal_20_iterations_5_95_quantiles <- apply(base_model_all_runs_biomass_annual_demersal_sum, 1, quantile, probs = c(0.025,0.975))

base_model_all_runs_biomass_annual_shellfish_20_iterations_mean <- apply(base_model_all_runs_biomass_annual_shellfish_sum, 1, mean)
base_model_all_runs_biomass_annual_shellfish_20_iterations_5_95_quantiles <- apply(base_model_all_runs_biomass_annual_shellfish_sum, 1, quantile, probs = c(0.025,0.975))

base_model_all_runs_biomass_annual_sandeel_20_iterations_mean <- apply(base_model_all_runs_biomass_annual_sandeel_sum, 1, mean)
base_model_all_runs_biomass_annual_sandeel_20_iterations_5_95_quantiles <- apply(base_model_all_runs_biomass_annual_sandeel_20_iterations, 1, quantile, probs = c(0.025,0.975))

options(scipen=10000)
plot((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
     base_model_all_runs_biomass_annual_sandeel_20_iterations_mean[28:87], type = "l",
     ylab = "Annual biomass, base model", xlab = "Project Year",
     ylim = c(min(base_model_all_runs_biomass_annual_sandeel_20_iterations_95_lower[28:87]),
              570000*max(base_model_all_runs_biomass_annual_demersal_20_iterations_5_95_quantiles[2,28:87])),
     col = "blue")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      base_model_all_runs_biomass_annual_sandeel_20_iterations_5_95_quantiles[1,28:87], lty = 2, col = "blue")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      base_model_all_runs_biomass_annual_sandeel_20_iterations_5_95_quantiles[2,28:87], lty = 2, col = "blue")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      570000*base_model_all_runs_biomass_annual_pelagic_20_iterations_mean[28:87], lty = 1, col = "red")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      570000*base_model_all_runs_biomass_annual_pelagic_20_iterations_5_95_quantiles[1,28:87], lty = 2, col = "red")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      570000*base_model_all_runs_biomass_annual_pelagic_20_iterations_5_95_quantiles[2,28:87], lty = 2, col = "red")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      570000*base_model_all_runs_biomass_annual_demersal_20_iterations_mean[28:87], lty = 1, col = "green")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      570000*base_model_all_runs_biomass_annual_demersal_20_iterations_5_95_quantiles[1,28:87], lty = 2, col = "green")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      570000*base_model_all_runs_biomass_annual_demersal_20_iterations_5_95_quantiles[2,28:87], lty = 2, col = "green")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      570000*base_model_all_runs_biomass_annual_shellfish_20_iterations_mean[28:87], lty = 1, col = "orange")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      570000*base_model_all_runs_biomass_annual_shellfish_20_iterations_5_95_quantiles[1,28:87], lty = 2, col = "orange")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year) - 2018)[28:87],
      570000*base_model_all_runs_biomass_annual_shellfish_20_iterations_5_95_quantiles[2,28:87], lty = 2, col = "orange")

legend(x = 0, y = 5600000, legend = c("Sandeels", "Demersal", "Pelagic", "Shellfish"),
       col = c("blue", "green", "red", "orange"), lty = c(1, 1, 1, 1), cex = 0.7)

base_model_all_runs_biomass_annual[[1]]$Spurdog[1]


# one noticeable thing so far --- the estimates of annual biomass for pelagic and demersal
# fish are much higher here than they are in the NCA paper. Given Jacobs email on 
# 14 Jan at 13:09, he says that the model outputs (catch, biomass) are in tonnes per
# km^2, so we need to multiply these values by 570,000 km^2 (the area of the model),
# and he assume he means this for all values in the outputs. The scales of the values
# we are getting certainly don't seem too far off, and so I believe this to be the case.
# I know I have definitely included more species in the pelagic and demersal fish than
# what is included in the workbook, merely because there are functional groups called
# 'large demersal fish' and similar, which I assumed should be included, but potentially
# not.

# If excluding these makes things look closer to what we see in the report, then
# I shall email Jo and see what the right thing to do is! If this still doesn't 
# solve it all, the other thing to ask would be about what to do with the juvenile
# and adult subgroups, which are their own functional groups in the data, but I'm
# not sure whether they count as being part of the fish groups.

fish_group_v2 <- c(rep(NA, 6), rep("demersal", 3), NA, rep("demersal", 11), NA, "pelagic", "demersal",
                rep(NA, 2), rep("demersal", 2), rep("pelagic", 5), "sandeel", rep("demersal", 10),
                rep(NA, 5), "shellfish", rep(NA,4), rep("shellfish", 2),
                rep(NA, 2), "shellfish", rep(NA, 11))

# so fish_group_v2 takes away the large demersal fish and small demersal fish
# away from the demersal fish group, and takes away miscellaneous 
# filterfeeding pelagic fish from the pelagic group. The rest is left the same

base_model_run_1_biomass_annual <- read.csv("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_small_model_set\\EcoSampler\\Base_model\\biomass_annual.csv",
                                            skip = 9)
base_model_run_1_biomass_annual[,-1] <- base_model_run_1_biomass_annual[,-1] * 570000
base_model_all_runs_biomass_annual <- vector("list", length = 20)
base_model_all_runs_biomass_annual[[1]] <- base_model_run_1_biomass_annual
colnames(base_model_all_runs_biomass_annual[[1]]) <- c("year", func_groups)

for(i in 1:19){
  
  base_model_all_runs_biomass_annual[[i + 1]] <- read.csv(paste0("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_small_model_set\\EcoSampler\\Run_",i,"\\biomass_annual.csv"),
                                                          skip = 9)
  colnames(base_model_all_runs_biomass_annual[[i + 1]]) <- c("year", func_groups)
  base_model_all_runs_biomass_annual[[i + 1]][,-1] <- base_model_all_runs_biomass_annual[[i + 1]][,-1] * 570000
  
}

for(i in 1:20){
  
  base_model_all_runs_biomass_annual[[i]] <- rbind(base_model_all_runs_biomass_annual[[i]], fish_group_v2)
  base_model_all_runs_biomass_annual[[i]] <- rbind(base_model_all_runs_biomass_annual[[i]], sandeel_diet)
  
}

length(which(base_model_all_runs_biomass_annual[[1]][111,] == "demersal")) # 27 demersal species
length(which(base_model_all_runs_biomass_annual[[1]][111,] == "pelagic")) # 6 pelagic species

base_model_all_runs_biomass_annual_demersal <- vector("list", 20)
base_model_all_runs_biomass_annual_demersal_array <- array(NA, dim = c(110, 27, 20)) # some issues with lists and character strings, so this is a workaround
base_model_all_runs_biomass_annual_demersal_sum <- matrix(NA, 110, 20) # 110 years, 20 iterations
base_model_all_runs_biomass_annual_pelagic <- vector("list", 20)
base_model_all_runs_biomass_annual_pelagic_array <- array(NA, dim = c(110, 6, 20)) # some issues with lists and character strings, so this is a workaround
base_model_all_runs_biomass_annual_pelagic_sum <- matrix(NA, 110, 20) # 110 years, 20 iterations
base_model_all_runs_biomass_annual_shellfish <- vector("list", 20)
base_model_all_runs_biomass_annual_shellfish_array <- array(NA, dim = c(110, 4, 20)) # some issues with lists and character strings, so this is a workaround
base_model_all_runs_biomass_annual_shellfish_sum <- matrix(NA, 110, 20) # 110 years, 20 iterations
base_model_all_runs_biomass_annual_sandeel_20_iterations <- matrix(NA, 110, 20)

for(i in 1:20){
  
  base_model_all_runs_biomass_annual_demersal[[i]] <- base_model_all_runs_biomass_annual[[i]][,which(base_model_all_runs_biomass_annual[[1]][111,] == "demersal")]
  base_model_all_runs_biomass_annual_demersal_array[, , i] <- matrix(as.numeric(as.matrix(base_model_all_runs_biomass_annual_demersal[[i]][1:110, ])), 110, 27)
  base_model_all_runs_biomass_annual_demersal_sum[, i] <- apply(base_model_all_runs_biomass_annual_demersal_array[, , i], 1, sum)
  base_model_all_runs_biomass_annual_pelagic[[i]] <- base_model_all_runs_biomass_annual[[i]][,which(base_model_all_runs_biomass_annual[[1]][111,] == "pelagic")]
  base_model_all_runs_biomass_annual_pelagic_array[, , i] <- matrix(as.numeric(as.matrix(base_model_all_runs_biomass_annual_pelagic[[i]][1:110, ])), 110, 6)
  base_model_all_runs_biomass_annual_pelagic_sum[, i] <- apply(base_model_all_runs_biomass_annual_pelagic_array[, , i], 1, sum)
  base_model_all_runs_biomass_annual_shellfish[[i]] <- base_model_all_runs_biomass_annual[[i]][,which(base_model_all_runs_biomass_annual[[1]][111,] == "shellfish")]
  base_model_all_runs_biomass_annual_shellfish_array[, , i] <- matrix(as.numeric(as.matrix(base_model_all_runs_biomass_annual_shellfish[[i]][1:110, ])), 110, 4)
  base_model_all_runs_biomass_annual_shellfish_sum[, i] <- apply(base_model_all_runs_biomass_annual_shellfish_array[, , i], 1, sum)
  base_model_all_runs_biomass_annual_sandeel_20_iterations[, i] <- as.numeric(base_model_all_runs_biomass_annual[[i]]$Sandeels[1:110])
  
}


base_model_all_runs_biomass_annual_pelagic_20_iterations_mean <- apply(base_model_all_runs_biomass_annual_pelagic_sum, 1, mean)
base_model_all_runs_biomass_annual_pelagic_20_iterations_5_95_quantiles <- apply(base_model_all_runs_biomass_annual_pelagic_sum, 1, quantile, probs = c(0.025,0.975))

base_model_all_runs_biomass_annual_demersal_20_iterations_mean <- apply(base_model_all_runs_biomass_annual_demersal_sum, 1, mean)
base_model_all_runs_biomass_annual_demersal_20_iterations_5_95_quantiles <- apply(base_model_all_runs_biomass_annual_demersal_sum, 1, quantile, probs = c(0.025,0.975))

base_model_all_runs_biomass_annual_shellfish_20_iterations_mean <- apply(base_model_all_runs_biomass_annual_shellfish_sum, 1, mean)
base_model_all_runs_biomass_annual_shellfish_20_iterations_5_95_quantiles <- apply(base_model_all_runs_biomass_annual_shellfish_sum, 1, quantile, probs = c(0.025,0.975))

base_model_all_runs_biomass_annual_sandeel_20_iterations_mean <- apply(base_model_all_runs_biomass_annual_sandeel_20_iterations, 1, mean)
base_model_all_runs_biomass_annual_sandeel_20_iterations_5_95_quantiles <- apply(base_model_all_runs_biomass_annual_sandeel_20_iterations, 1, quantile, probs = c(0.025,0.975))

options(scipen=10000)
plot((as.numeric(base_model_all_runs_biomass_annual[[1]]$year[1:110]) - 2018)[28:87],
     base_model_all_runs_biomass_annual_sandeel_20_iterations_mean[28:87], type = "l",
     ylab = "Annual biomass, base model", xlab = "Project Year",
     ylim = c(min(base_model_all_runs_biomass_annual_sandeel_20_iterations_5_95_quantiles[1,28:87]),
              max(base_model_all_runs_biomass_annual_demersal_20_iterations_5_95_quantiles[2,28:87])),
     col = "blue")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year[1:110]) - 2018)[28:87],
      base_model_all_runs_biomass_annual_sandeel_20_iterations_5_95_quantiles[1,28:87], lty = 2, col = "blue")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year[1:110]) - 2018)[28:87],
      base_model_all_runs_biomass_annual_sandeel_20_iterations_5_95_quantiles[2,28:87], lty = 2, col = "blue")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year[1:110]) - 2018)[28:87],
      base_model_all_runs_biomass_annual_pelagic_20_iterations_mean[28:87], lty = 1, col = "red")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year[1:110]) - 2018)[28:87],
      base_model_all_runs_biomass_annual_pelagic_20_iterations_5_95_quantiles[1,28:87], lty = 2, col = "red")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year[1:110]) - 2018)[28:87],
      base_model_all_runs_biomass_annual_pelagic_20_iterations_5_95_quantiles[2,28:87], lty = 2, col = "red")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year[1:110]) - 2018)[28:87],
      base_model_all_runs_biomass_annual_demersal_20_iterations_mean[28:87], lty = 1, col = "green")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year[1:110]) - 2018)[28:87],
      base_model_all_runs_biomass_annual_demersal_20_iterations_5_95_quantiles[1,28:87], lty = 2, col = "green")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year[1:110]) - 2018)[28:87],
      base_model_all_runs_biomass_annual_demersal_20_iterations_5_95_quantiles[2,28:87], lty = 2, col = "green")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year[1:110]) - 2018)[28:87],
      base_model_all_runs_biomass_annual_shellfish_20_iterations_mean[28:87], lty = 1, col = "orange")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year[1:110]) - 2018)[28:87],
      base_model_all_runs_biomass_annual_shellfish_20_iterations_5_95_quantiles[1,28:87], lty = 2, col = "orange")
lines((as.numeric(base_model_all_runs_biomass_annual[[1]]$year[1:110]) - 2018)[28:87],
      base_model_all_runs_biomass_annual_shellfish_20_iterations_5_95_quantiles[2,28:87], lty = 2, col = "orange")

legend(x = 0, y = 5600000, legend = c("Sandeels", "Demersal", "Pelagic", "Shellfish"),
       col = c("blue", "green", "red", "orange"), lty = c(1, 1, 1, 1), cex = 0.7)


# this still doesn't quite compare to what we see in the Figures, so I will try again
# on Thursday, including only the adult subgroups where applicable (for example cod)
# and ignore the juvenile subgroups, and see if these numbers compare better
# for the pelagic and demersal fish groups.
