# First R script is 20_runs_investigation, which was mainly a starting point for
# getting to grips with the data and having a go at reproducing Figure 7.1. This
# script is for continuing with reproducing the Figures in Section 7 of the report,
# hopefully efficiently having sorted out how to work with the data.

# Firstly will include some lines from the 20 runs investigation script which could
# be needed here, in case needed

func_groups_output <- read.csv("C:\\Users\\adamg\\Google Drive\\Natural England project\\Previous report\\NorthSea_EwE_basic_estimates_functional_groups.csv")
func_groups_output <- func_groups_output[!is.na(func_groups_output[, 1]), ]
func_groups <- func_groups_output[, 2]
project_years <- 28:87
total_years <- 110
n_iter <- 20

base_model_run_1_biomass_annual <- read.csv("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_small_model_set\\EcoSampler\\Base_model\\biomass_annual.csv",
                                            skip = 9)
base_model_run_1_biomass_annual[,-1] <- base_model_run_1_biomass_annual[,-1] * 570000
base_model_all_runs_biomass_annual_v1 <- vector("list", length = n_iter)
base_model_all_runs_biomass_annual_v1[[1]] <- base_model_run_1_biomass_annual
colnames(base_model_all_runs_biomass_annual_v1[[1]]) <- c("year", func_groups)

for(i in 1:(n_iter - 1)){
  
  base_model_all_runs_biomass_annual_v1[[i + 1]] <- read.csv(paste0("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_small_model_set\\EcoSampler\\Run_",i,"\\biomass_annual.csv"),
                                                          skip = 9)
  colnames(base_model_all_runs_biomass_annual_v1[[i + 1]]) <- c("year", func_groups)
  base_model_all_runs_biomass_annual_v1[[i + 1]][,-1] <- base_model_all_runs_biomass_annual_v1[[i + 1]][,-1] * 570000
  
}

# this version of fish grouping includes all juvenile fish, and generic groups such as 'large demersal fish'

fish_group_v1 <- c(rep(NA, 6), rep("demersal", 3), NA, rep("demersal", 11), NA, "pelagic", "demersal",
                   rep(NA, 2), rep("demersal", 2), rep("pelagic", 5), "sandeel", rep("demersal", 10),
                   rep(NA, 2), rep("demersal", 2), "pelagic", "shellfish", rep(NA,4), rep("shellfish", 2),
                   rep(NA, 2), "shellfish", rep(NA, 11))
sandeel_diet <- c(NA, rep(F, 9), T, F, rep(T, 7), rep(F, 7), rep(T, 2), rep(F, 3),
                  T, rep(F, 9), T, F, T, rep(F, 26))

for(i in 1:n_iter){
  
  base_model_all_runs_biomass_annual_v1[[i]] <- rbind(base_model_all_runs_biomass_annual_v1[[i]], fish_group_v1)
  base_model_all_runs_biomass_annual_v1[[i]] <- rbind(base_model_all_runs_biomass_annual_v1[[i]], sandeel_diet)
  
}

n_dmrsl_v1 <- length(which(base_model_all_runs_biomass_annual_v1[[1]][(total_years + 1), ] == "demersal")) # 29 demersal species
n_plgc_v1 <- length(which(base_model_all_runs_biomass_annual_v1[[1]][(total_years + 1), ] == "pelagic")) # 7 pelagic species
n_sndl_v1 <- length(which(base_model_all_runs_biomass_annual_v1[[1]][(total_years + 1), ] == "sandeel")) # 1 sandeel species
n_shllfsh_v1 <- length(which(base_model_all_runs_biomass_annual_v1[[1]][(total_years + 1), ] == "shellfish")) # 4 shellfish species


base_model_all_runs_biomass_annual_demersal_v1 <- vector("list", n_iter)
base_model_all_runs_biomass_annual_demersal_v1_array <- array(NA, dim = c(total_years, n_dmrsl_v1, n_iter)) # some issues with lists and character strings, so this is a workaround
base_model_all_runs_biomass_annual_demersal_v1_sum <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations
base_model_all_runs_biomass_annual_pelagic_v1 <- vector("list", n_iter)
base_model_all_runs_biomass_annual_pelagic_v1_array <- array(NA, dim = c(total_years, n_plgc_v1, n_iter)) # some issues with lists and character strings, so this is a workaround
base_model_all_runs_biomass_annual_pelagic_v1_sum <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations
base_model_all_runs_biomass_annual_shellfish_v1 <- vector("list", n_iter)
base_model_all_runs_biomass_annual_shellfish_v1_array <- array(NA, dim = c(total_years, n_shllfsh_v1, n_iter)) # some issues with lists and character strings, so this is a workaround
base_model_all_runs_biomass_annual_shellfish_v1_sum <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations
base_model_all_runs_biomass_annual_sandeel_v1_20_iterations <- matrix(NA, total_years, n_iter)

for(i in 1:n_iter){
  
  base_model_all_runs_biomass_annual_demersal_v1[[i]] <- base_model_all_runs_biomass_annual_v1[[i]][,which(base_model_all_runs_biomass_annual_v1[[1]][111,] == "demersal")]
  base_model_all_runs_biomass_annual_demersal_v1_array[, , i] <- matrix(as.numeric(as.matrix(base_model_all_runs_biomass_annual_demersal_v1[[i]][1:total_years, ])), total_years, n_dmrsl_v1)
  base_model_all_runs_biomass_annual_demersal_v1_sum[, i] <- apply(base_model_all_runs_biomass_annual_demersal_v1_array[, , i], 1, sum)
  base_model_all_runs_biomass_annual_pelagic_v1[[i]] <- base_model_all_runs_biomass_annual_v1[[i]][,which(base_model_all_runs_biomass_annual_v1[[1]][111,] == "pelagic")]
  base_model_all_runs_biomass_annual_pelagic_v1_array[, , i] <- matrix(as.numeric(as.matrix(base_model_all_runs_biomass_annual_pelagic_v1[[i]][1:total_years, ])), total_years, n_plgc_v1)
  base_model_all_runs_biomass_annual_pelagic_v1_sum[, i] <- apply(base_model_all_runs_biomass_annual_pelagic_v1_array[, , i], 1, sum)
  base_model_all_runs_biomass_annual_shellfish_v1[[i]] <- base_model_all_runs_biomass_annual_v1[[i]][,which(base_model_all_runs_biomass_annual[[1]][111,] == "shellfish")]
  base_model_all_runs_biomass_annual_shellfish_v1_array[, , i] <- matrix(as.numeric(as.matrix(base_model_all_runs_biomass_annual_shellfish_v1[[i]][1:total_years, ])), total_years, n_shllfsh)
  base_model_all_runs_biomass_annual_shellfish_v1_sum[, i] <- apply(base_model_all_runs_biomass_annual_shellfish_v1_array[, , i], 1, sum)
  base_model_all_runs_biomass_annual_sandeel_v1_20_iterations[, i] <- as.numeric(base_model_all_runs_biomass_annual_v1[[i]]$Sandeels[1:total_years])
  
}


base_model_all_runs_biomass_annual_pelagic_v1_20_iterations_mean <- apply(base_model_all_runs_biomass_annual_pelagic_v1_sum, 1, mean)
base_model_all_runs_biomass_annual_pelagic_v1_20_iterations_5_95_quantiles <- apply(base_model_all_runs_biomass_annual_pelagic_v1_sum, 1, quantile, probs = c(0.025,0.975))

base_model_all_runs_biomass_annual_demersal_v1_20_iterations_mean <- apply(base_model_all_runs_biomass_annual_demersal_v1_sum, 1, mean)
base_model_all_runs_biomass_annual_demersal_v1_20_iterations_5_95_quantiles <- apply(base_model_all_runs_biomass_annual_demersal_v1_sum, 1, quantile, probs = c(0.025,0.975))

base_model_all_runs_biomass_annual_shellfish_v1_20_iterations_mean <- apply(base_model_all_runs_biomass_annual_shellfish_v1_sum, 1, mean)
base_model_all_runs_biomass_annual_shellfish_v1_20_iterations_5_95_quantiles <- apply(base_model_all_runs_biomass_annual_shellfish_v1_sum, 1, quantile, probs = c(0.025,0.975))

base_model_all_runs_biomass_annual_sandeel_v1_20_iterations_mean <- apply(base_model_all_runs_biomass_annual_sandeel_v1_20_iterations, 1, mean)
base_model_all_runs_biomass_annual_sandeel_v1_20_iterations_5_95_quantiles <- apply(base_model_all_runs_biomass_annual_sandeel_v1_20_iterations, 1, quantile, probs = c(0.025,0.975))

options(scipen=10000)
plot((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
     base_model_all_runs_biomass_annual_sandeel_v1_20_iterations_mean[project_years], type = "l",
     ylab = "Annual biomass, base model", xlab = "Project Year",
     ylim = c(min(base_model_all_runs_biomass_annual_sandeel_v1_20_iterations_5_95_quantiles[1,project_years]),
              max(base_model_all_runs_biomass_annual_demersal_v1_20_iterations_5_95_quantiles[2,project_years])),
     col = "blue")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_sandeel_v1_20_iterations_5_95_quantiles[1,project_years], lty = 2, col = "blue")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_sandeel_v1_20_iterations_5_95_quantiles[2,project_years], lty = 2, col = "blue")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_pelagic_v1_20_iterations_mean[project_years], lty = 1, col = "red")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_pelagic_v1_20_iterations_5_95_quantiles[1,project_years], lty = 2, col = "red")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_pelagic_v1_20_iterations_5_95_quantiles[2,project_years], lty = 2, col = "red")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_demersal_v1_20_iterations_mean[project_years], lty = 1, col = "green")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_demersal_v1_20_iterations_5_95_quantiles[1,project_years], lty = 2, col = "green")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_demersal_v1_20_iterations_5_95_quantiles[2,project_years], lty = 2, col = "green")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_shellfish_v1_20_iterations_mean[project_years], lty = 1, col = "orange")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_shellfish_v1_20_iterations_5_95_quantiles[1,project_years], lty = 2, col = "orange")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_shellfish_v1_20_iterations_5_95_quantiles[2,project_years], lty = 2, col = "orange")

legend(x = 0, y = 5200000, legend = c("Sandeels", "Demersal", "Pelagic", "Shellfish"),
       col = c("blue", "green", "red", "orange"), lty = c(1, 1, 1, 1), cex = 0.7)


# now looking at fish that feed on sandeels (so this is only demersal and pelagic fish species)

n_sndl_v1 <- length(which(base_model_all_runs_biomass_annual_v1[[1]][(total_years + 1), ] == "sandeel")) # 1 sandeel species
dmrsl_eat_sndl <- which(base_model_all_runs_biomass_annual_v1[[1]][(total_years + 1), ] == "demersal" & base_model_all_runs_biomass_annual_v1[[1]][(total_years + 2), ] == T)
n_dmrsl_eat_sndl <- length(dmrsl_eat_sndl) # 12 demersal species with sandeel diet
plgc_eat_sndl <- which(base_model_all_runs_biomass_annual_v1[[1]][(total_years + 1), ] == "pelagic" & base_model_all_runs_biomass_annual_v1[[1]][(total_years + 2), ] == T) 
n_plgc_eat_sndl <- length(plgc_eat_sndl) # 1 pelagic species with sandeel diet


base_model_all_runs_biomass_annual_demersal_eat_sndl <- vector("list", n_iter)
base_model_all_runs_biomass_annual_demersal_eat_sndl_array <- array(NA, dim = c(total_years, n_dmrsl_eat_sndl, n_iter)) # some issues with lists and character strings, so this is a workaround
base_model_all_runs_biomass_annual_demersal_eat_sndl_sum <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations
base_model_all_runs_biomass_annual_pelagic_eat_sndl_20_iterations <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations

for(i in 1:n_iter){
  
  base_model_all_runs_biomass_annual_demersal_eat_sndl[[i]] <- base_model_all_runs_biomass_annual_v1[[i]][,dmrsl_eat_sndl]
  base_model_all_runs_biomass_annual_demersal_eat_sndl_array[, , i] <- matrix(as.numeric(as.matrix(base_model_all_runs_biomass_annual_demersal_eat_sndl[[i]][1:total_years, ])), total_years, n_dmrsl_eat_sndl)
  base_model_all_runs_biomass_annual_demersal_eat_sndl_sum[, i] <- apply(base_model_all_runs_biomass_annual_demersal_eat_sndl_array[, , i], 1, sum)
  base_model_all_runs_biomass_annual_pelagic_eat_sndl_20_iterations[, i] <- as.numeric(base_model_all_runs_biomass_annual_v1[[i]][1:total_years, plgc_eat_sndl])

}


base_model_all_runs_biomass_annual_pelagic_eat_sndl_20_iterations_mean <- apply(base_model_all_runs_biomass_annual_pelagic_eat_sndl_20_iterations, 1, mean)
base_model_all_runs_biomass_annual_pelagic_eat_sndl_20_iterations_5_95_quantiles <- apply(base_model_all_runs_biomass_annual_pelagic_eat_sndl_20_iterations, 1, quantile, probs = c(0.025,0.975))

base_model_all_runs_biomass_annual_demersal_eat_sndl_20_iterations_mean <- apply(base_model_all_runs_biomass_annual_demersal_eat_sndl_sum, 1, mean)
base_model_all_runs_biomass_annual_demersal_eat_sndl_20_iterations_5_95_quantiles <- apply(base_model_all_runs_biomass_annual_demersal_eat_sndl_sum, 1, quantile, probs = c(0.025,0.975))

options(scipen=10000)
plot((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
     base_model_all_runs_biomass_annual_sandeel_v1_20_iterations_mean[project_years], type = "l",
     ylab = "Annual biomass, base model", xlab = "Project Year",
     ylim = c(min(base_model_all_runs_biomass_annual_sandeel_v1_20_iterations_5_95_quantiles[1,project_years]),
              1.05*max(base_model_all_runs_biomass_annual_pelagic_eat_sndl_20_iterations_5_95_quantiles[2,project_years])),
     col = "blue")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_sandeel_v1_20_iterations_5_95_quantiles[1,project_years], lty = 2, col = "blue")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_sandeel_v1_20_iterations_5_95_quantiles[2,project_years], lty = 2, col = "blue")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_pelagic_eat_sndl_20_iterations_mean[project_years], lty = 1, col = "red")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_pelagic_eat_sndl_20_iterations_5_95_quantiles[1,project_years], lty = 2, col = "red")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_pelagic_eat_sndl_20_iterations_5_95_quantiles[2,project_years], lty = 2, col = "red")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_demersal_eat_sndl_20_iterations_mean[project_years], lty = 1, col = "green")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_demersal_eat_sndl_20_iterations_5_95_quantiles[1,project_years], lty = 2, col = "green")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_demersal_eat_sndl_20_iterations_5_95_quantiles[2,project_years], lty = 2, col = "green")

legend(x = -2.2, y = 1255000, legend = c("Sandeels", "Demersal fish w/ sandeel diet", "Pelagic fish w/ sandeel diet"),
       col = c("blue", "green", "red"), lty = c(1, 1, 1), cex = 0.6)

# one final plot, looking at the annual biomass of surface-feeding birds and diving seabirds
# plotted on same graph, but not particularly for comparison, just for minimising the number
# of plots. Once we have more data on the other sandeel fishing scenarios, they will likely
# be separated into 2 plots, so we can compare the fishing scenarios and their impact
# on these species

base_model_all_runs_biomass_annual_surface_feeding_birds_v1_20_iterations <- matrix(NA, total_years, n_iter)
base_model_all_runs_biomass_annual_diving_seabirds_v1_20_iterations <- matrix(NA, total_years, n_iter)

for(i in 1:n_iter){
  
  base_model_all_runs_biomass_annual_surface_feeding_birds_v1_20_iterations[, i] <- as.numeric(base_model_all_runs_biomass_annual_v1[[i]]$`Surface-feeding seabirds`[1:total_years])
  base_model_all_runs_biomass_annual_diving_seabirds_v1_20_iterations[, i] <- as.numeric(base_model_all_runs_biomass_annual_v1[[i]]$`Diving seabirds`[1:total_years])
  
}

base_model_all_runs_biomass_annual_surface_feeding_birds_v1_20_iterations_mean <- apply(base_model_all_runs_biomass_annual_surface_feeding_birds_v1_20_iterations, 1, mean)
base_model_all_runs_biomass_annual_surface_feeding_birds_v1_20_iterations_2.5_97.5_quantiles <- apply(base_model_all_runs_biomass_annual_surface_feeding_birds_v1_20_iterations, 1, quantile, probs = c(0.025,0.975))

base_model_all_runs_biomass_annual_diving_seabirds_v1_20_iterations_mean <- apply(base_model_all_runs_biomass_annual_diving_seabirds_v1_20_iterations, 1, mean)
base_model_all_runs_biomass_annual_diving_seabirds_v1_20_iterations_2.5_97.5_quantiles <- apply(base_model_all_runs_biomass_annual_diving_seabirds_v1_20_iterations, 1, quantile, probs = c(0.025,0.975))


options(scipen=10000)
plot((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
     base_model_all_runs_biomass_annual_surface_feeding_birds_v1_20_iterations_mean[project_years], type = "l",
     ylab = "Annual biomass, base model", xlab = "Project Year",
     ylim = c(min(base_model_all_runs_biomass_annual_surface_feeding_birds_v1_20_iterations_2.5_97.5_quantiles[1,project_years]),
              max(base_model_all_runs_biomass_annual_diving_seabirds_v1_20_iterations_2.5_97.5_quantiles[2,project_years])),
     col = "purple")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_surface_feeding_birds_v1_20_iterations_2.5_97.5_quantiles[1,project_years], lty = 2, col = "purple")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_surface_feeding_birds_v1_20_iterations_2.5_97.5_quantiles[2,project_years], lty = 2, col = "purple")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_diving_seabirds_v1_20_iterations_mean[project_years], lty = 1, col = "brown")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_diving_seabirds_v1_20_iterations_2.5_97.5_quantiles[1,project_years], lty = 2, col = "brown")
lines((as.numeric(base_model_all_runs_biomass_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_biomass_annual_diving_seabirds_v1_20_iterations_2.5_97.5_quantiles[2,project_years], lty = 2, col = "brown")

legend(x = 30, y = 2600, legend = c("Surface-feeding birds", "Diving seabirds"),
       col = c("purple", "brown"), lty = c(1, 1), cex = 0.6)


# now want to have a little look at catch data
# don't really have plots to compare the catch data with (maybe just for sandeels),
# so will generally look at similar things to what we have seen here

base_model_run_1_catch_annual <- read.csv("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_small_model_set\\EcoSampler\\Base_model\\catch_annual.csv",
                                            skip = 9)
base_model_run_1_catch_annual[,-1] <- base_model_run_1_catch_annual[,-1] * 570000
base_model_all_runs_catch_annual_v1 <- vector("list", length = n_iter)
base_model_all_runs_catch_annual_v1[[1]] <- base_model_run_1_catch_annual
colnames(base_model_all_runs_catch_annual_v1[[1]]) <- c("year", func_groups)

for(i in 1:(n_iter - 1)){
  
  base_model_all_runs_catch_annual_v1[[i + 1]] <- read.csv(paste0("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_small_model_set\\EcoSampler\\Run_",i,"\\catch_annual.csv"),
                                                             skip = 9)
  colnames(base_model_all_runs_catch_annual_v1[[i + 1]]) <- c("year", func_groups)
  base_model_all_runs_catch_annual_v1[[i + 1]][,-1] <- base_model_all_runs_catch_annual_v1[[i + 1]][,-1] * 570000
  
}

# this version of fish grouping includes all juvenile fish, and generic groups such as 'large demersal fish'

fish_group_v1 <- c(rep(NA, 6), rep("demersal", 3), NA, rep("demersal", 11), NA, "pelagic", "demersal",
                   rep(NA, 2), rep("demersal", 2), rep("pelagic", 5), "sandeel", rep("demersal", 10),
                   rep(NA, 2), rep("demersal", 2), "pelagic", "shellfish", rep(NA,4), rep("shellfish", 2),
                   rep(NA, 2), "shellfish", rep(NA, 11))
sandeel_diet <- c(NA, rep(F, 9), T, F, rep(T, 7), rep(F, 7), rep(T, 2), rep(F, 3),
                  T, rep(F, 9), T, F, T, rep(F, 26))

for(i in 1:n_iter){
  
  base_model_all_runs_catch_annual_v1[[i]] <- rbind(base_model_all_runs_catch_annual_v1[[i]], fish_group_v1)
  base_model_all_runs_catch_annual_v1[[i]] <- rbind(base_model_all_runs_catch_annual_v1[[i]], sandeel_diet)
  
}


base_model_all_runs_catch_annual_demersal_v1 <- vector("list", n_iter)
base_model_all_runs_catch_annual_demersal_v1_array <- array(NA, dim = c(total_years, n_dmrsl_v1, n_iter)) # some issues with lists and character strings, so this is a workaround
base_model_all_runs_catch_annual_demersal_v1_sum <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations
base_model_all_runs_catch_annual_pelagic_v1 <- vector("list", n_iter)
base_model_all_runs_catch_annual_pelagic_v1_array <- array(NA, dim = c(total_years, n_plgc_v1, n_iter)) # some issues with lists and character strings, so this is a workaround
base_model_all_runs_catch_annual_pelagic_v1_sum <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations
base_model_all_runs_catch_annual_shellfish_v1 <- vector("list", n_iter)
base_model_all_runs_catch_annual_shellfish_v1_array <- array(NA, dim = c(total_years, n_shllfsh_v1, n_iter)) # some issues with lists and character strings, so this is a workaround
base_model_all_runs_catch_annual_shellfish_v1_sum <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations
base_model_all_runs_catch_annual_sandeel_v1_20_iterations <- matrix(NA, total_years, n_iter)

for(i in 1:n_iter){
  
  base_model_all_runs_catch_annual_demersal_v1[[i]] <- base_model_all_runs_catch_annual_v1[[i]][,which(base_model_all_runs_catch_annual_v1[[1]][111,] == "demersal")]
  base_model_all_runs_catch_annual_demersal_v1_array[, , i] <- matrix(as.numeric(as.matrix(base_model_all_runs_catch_annual_demersal_v1[[i]][1:total_years, ])), total_years, n_dmrsl_v1)
  base_model_all_runs_catch_annual_demersal_v1_sum[, i] <- apply(base_model_all_runs_catch_annual_demersal_v1_array[, , i], 1, sum)
  base_model_all_runs_catch_annual_pelagic_v1[[i]] <- base_model_all_runs_catch_annual_v1[[i]][,which(base_model_all_runs_catch_annual_v1[[1]][111,] == "pelagic")]
  base_model_all_runs_catch_annual_pelagic_v1_array[, , i] <- matrix(as.numeric(as.matrix(base_model_all_runs_catch_annual_pelagic_v1[[i]][1:total_years, ])), total_years, n_plgc_v1)
  base_model_all_runs_catch_annual_pelagic_v1_sum[, i] <- apply(base_model_all_runs_catch_annual_pelagic_v1_array[, , i], 1, sum)
  base_model_all_runs_catch_annual_shellfish_v1[[i]] <- base_model_all_runs_catch_annual_v1[[i]][,which(base_model_all_runs_catch_annual_v1[[1]][111,] == "shellfish")]
  base_model_all_runs_catch_annual_shellfish_v1_array[, , i] <- matrix(as.numeric(as.matrix(base_model_all_runs_catch_annual_shellfish_v1[[i]][1:total_years, ])), total_years, n_shllfsh)
  base_model_all_runs_catch_annual_shellfish_v1_sum[, i] <- apply(base_model_all_runs_catch_annual_shellfish_v1_array[, , i], 1, sum)
  base_model_all_runs_catch_annual_sandeel_v1_20_iterations[, i] <- as.numeric(base_model_all_runs_catch_annual_v1[[i]]$Sandeels[1:total_years])
  
}


base_model_all_runs_catch_annual_pelagic_v1_20_iterations_mean <- apply(base_model_all_runs_catch_annual_pelagic_v1_sum, 1, mean)
base_model_all_runs_catch_annual_pelagic_v1_20_iterations_2.5_97.5_quantiles <- apply(base_model_all_runs_catch_annual_pelagic_v1_sum, 1, quantile, probs = c(0.025,0.975))

base_model_all_runs_catch_annual_demersal_v1_20_iterations_mean <- apply(base_model_all_runs_catch_annual_demersal_v1_sum, 1, mean)
base_model_all_runs_catch_annual_demersal_v1_20_iterations_2.5_97.5_quantiles <- apply(base_model_all_runs_catch_annual_demersal_v1_sum, 1, quantile, probs = c(0.025,0.975))

base_model_all_runs_catch_annual_shellfish_v1_20_iterations_mean <- apply(base_model_all_runs_catch_annual_shellfish_v1_sum, 1, mean)
base_model_all_runs_catch_annual_shellfish_v1_20_iterations_2.5_97.5_quantiles <- apply(base_model_all_runs_catch_annual_shellfish_v1_sum, 1, quantile, probs = c(0.025,0.975))

base_model_all_runs_catch_annual_sandeel_v1_20_iterations_mean <- apply(base_model_all_runs_catch_annual_sandeel_v1_20_iterations, 1, mean)
base_model_all_runs_catch_annual_sandeel_v1_20_iterations_2.5_97.5_quantiles <- apply(base_model_all_runs_catch_annual_sandeel_v1_20_iterations, 1, quantile, probs = c(0.025,0.975))

options(scipen=10000)
plot((as.numeric(base_model_all_runs_catch_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
     base_model_all_runs_catch_annual_sandeel_v1_20_iterations_mean[project_years], type = "l",
     ylab = "Annual catch, base model", xlab = "Project Year",
     ylim = c(min(base_model_all_runs_catch_annual_shellfish_v1_20_iterations_2.5_97.5_quantiles[1,project_years]),
              max(base_model_all_runs_catch_annual_demersal_v1_20_iterations_2.5_97.5_quantiles[2,project_years])),
     col = "blue")
lines((as.numeric(base_model_all_runs_catch_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_catch_annual_sandeel_v1_20_iterations_2.5_97.5_quantiles[1,project_years], lty = 2, col = "blue")
lines((as.numeric(base_model_all_runs_catch_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_catch_annual_sandeel_v1_20_iterations_2.5_97.5_quantiles[2,project_years], lty = 2, col = "blue")
lines((as.numeric(base_model_all_runs_catch_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_catch_annual_pelagic_v1_20_iterations_mean[project_years], lty = 1, col = "red")
lines((as.numeric(base_model_all_runs_catch_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_catch_annual_pelagic_v1_20_iterations_2.5_97.5_quantiles[1,project_years], lty = 2, col = "red")
lines((as.numeric(base_model_all_runs_catch_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_catch_annual_pelagic_v1_20_iterations_2.5_97.5_quantiles[2,project_years], lty = 2, col = "red")
lines((as.numeric(base_model_all_runs_catch_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_catch_annual_demersal_v1_20_iterations_mean[project_years], lty = 1, col = "green")
lines((as.numeric(base_model_all_runs_catch_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_catch_annual_demersal_v1_20_iterations_2.5_97.5_quantiles[1,project_years], lty = 2, col = "green")
lines((as.numeric(base_model_all_runs_catch_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_catch_annual_demersal_v1_20_iterations_2.5_97.5_quantiles[2,project_years], lty = 2, col = "green")
lines((as.numeric(base_model_all_runs_catch_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_catch_annual_shellfish_v1_20_iterations_mean[project_years], lty = 1, col = "orange")
lines((as.numeric(base_model_all_runs_catch_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_catch_annual_shellfish_v1_20_iterations_2.5_97.5_quantiles[1,project_years], lty = 2, col = "orange")
lines((as.numeric(base_model_all_runs_catch_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_catch_annual_shellfish_v1_20_iterations_2.5_97.5_quantiles[2,project_years], lty = 2, col = "orange")

legend(x = 45, y = 600000, legend = c("Sandeels", "Demersal", "Pelagic", "Shellfish"),
       col = c("blue", "green", "red", "orange"), lty = c(1, 1, 1, 1), cex = 0.7)

# more catch data plots, for fish that feed on sandeels

base_model_all_runs_catch_annual_demersal_eat_sndl <- vector("list", n_iter)
base_model_all_runs_catch_annual_demersal_eat_sndl_array <- array(NA, dim = c(total_years, n_dmrsl_eat_sndl, n_iter)) # some issues with lists and character strings, so this is a workaround
base_model_all_runs_catch_annual_demersal_eat_sndl_sum <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations
base_model_all_runs_catch_annual_pelagic_eat_sndl_20_iterations <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations

for(i in 1:n_iter){
  
  base_model_all_runs_catch_annual_demersal_eat_sndl[[i]] <- base_model_all_runs_catch_annual_v1[[i]][,dmrsl_eat_sndl]
  base_model_all_runs_catch_annual_demersal_eat_sndl_array[, , i] <- matrix(as.numeric(as.matrix(base_model_all_runs_catch_annual_demersal_eat_sndl[[i]][1:total_years, ])), total_years, n_dmrsl_eat_sndl)
  base_model_all_runs_catch_annual_demersal_eat_sndl_sum[, i] <- apply(base_model_all_runs_catch_annual_demersal_eat_sndl_array[, , i], 1, sum)
  base_model_all_runs_catch_annual_pelagic_eat_sndl_20_iterations[, i] <- as.numeric(base_model_all_runs_catch_annual_v1[[i]][1:total_years, plgc_eat_sndl])
  
}


base_model_all_runs_catch_annual_pelagic_eat_sndl_20_iterations_mean <- apply(base_model_all_runs_catch_annual_pelagic_eat_sndl_20_iterations, 1, mean)
base_model_all_runs_catch_annual_pelagic_eat_sndl_20_iterations_2.5_97.5_quantiles <- apply(base_model_all_runs_catch_annual_pelagic_eat_sndl_20_iterations, 1, quantile, probs = c(0.025,0.975))

base_model_all_runs_catch_annual_demersal_eat_sndl_20_iterations_mean <- apply(base_model_all_runs_catch_annual_demersal_eat_sndl_sum, 1, mean)
base_model_all_runs_catch_annual_demersal_eat_sndl_20_iterations_2.5_97.5_quantiles <- apply(base_model_all_runs_catch_annual_demersal_eat_sndl_sum, 1, quantile, probs = c(0.025,0.975))

options(scipen=10000)
plot((as.numeric(base_model_all_runs_catch_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
     base_model_all_runs_catch_annual_sandeel_v1_20_iterations_mean[project_years], type = "l",
     ylab = "Annual catch, base model", xlab = "Project Year",
     ylim = c(min(base_model_all_runs_catch_annual_pelagic_eat_sndl_20_iterations_2.5_97.5_quantiles[1,project_years]),
              max(base_model_all_runs_catch_annual_sandeel_v1_20_iterations_2.5_97.5_quantiles[2,project_years])),
     col = "blue")
lines((as.numeric(base_model_all_runs_catch_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_catch_annual_sandeel_v1_20_iterations_2.5_97.5_quantiles[1,project_years], lty = 2, col = "blue")
lines((as.numeric(base_model_all_runs_catch_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_catch_annual_sandeel_v1_20_iterations_2.5_97.5_quantiles[2,project_years], lty = 2, col = "blue")
lines((as.numeric(base_model_all_runs_catch_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_catch_annual_pelagic_eat_sndl_20_iterations_mean[project_years], lty = 1, col = "red")
lines((as.numeric(base_model_all_runs_catch_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_catch_annual_pelagic_eat_sndl_20_iterations_2.5_97.5_quantiles[1,project_years], lty = 2, col = "red")
lines((as.numeric(base_model_all_runs_catch_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_catch_annual_pelagic_eat_sndl_20_iterations_2.5_97.5_quantiles[2,project_years], lty = 2, col = "red")
lines((as.numeric(base_model_all_runs_catch_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_catch_annual_demersal_eat_sndl_20_iterations_mean[project_years], lty = 1, col = "green")
lines((as.numeric(base_model_all_runs_catch_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_catch_annual_demersal_eat_sndl_20_iterations_2.5_97.5_quantiles[1,project_years], lty = 2, col = "green")
lines((as.numeric(base_model_all_runs_catch_annual_v1[[1]]$year[1:total_years]) - 2018)[project_years],
      base_model_all_runs_catch_annual_demersal_eat_sndl_20_iterations_2.5_97.5_quantiles[2,project_years], lty = 2, col = "green")

legend(x = 35, y = 400000, legend = c("Sandeels", "Demersal fish w/ sandeel diet", "Pelagic fish w/ sandeel diet"),
       col = c("blue", "green", "red"), lty = c(1, 1, 1), cex = 0.6)

# assessing the variability of the fish group species in the final levels of these 
# quantities

base_model_all_runs_biomass_annual_pelagic_v1_20_iterations_sd <- apply(base_model_all_runs_biomass_annual_pelagic_v1_sum, 1, sd)
base_model_all_runs_biomass_annual_demersal_v1_20_iterations_sd <- apply(base_model_all_runs_biomass_annual_demersal_v1_sum, 1, sd)
base_model_all_runs_biomass_annual_shellfish_v1_20_iterations_sd <- apply(base_model_all_runs_biomass_annual_shellfish_v1_sum, 1, sd)
base_model_all_runs_biomass_annual_sandeel_v1_20_iterations_sd <- apply(base_model_all_runs_biomass_annual_sandeel_v1_20_iterations, 1, sd)

base_model_all_runs_biomass_annual_pelagic_v1_20_iterations_CV_Year_59 <- base_model_all_runs_biomass_annual_pelagic_v1_20_iterations_sd[87] / base_model_all_runs_biomass_annual_pelagic_v1_20_iterations_mean[87]
base_model_all_runs_biomass_annual_demersal_v1_20_iterations_CV_Year_59 <- base_model_all_runs_biomass_annual_demersal_v1_20_iterations_sd[87] / base_model_all_runs_biomass_annual_demersal_v1_20_iterations_mean[87]
base_model_all_runs_biomass_annual_shellfish_v1_20_iterations_CV_Year_59 <- base_model_all_runs_biomass_annual_shellfish_v1_20_iterations_sd[87] / base_model_all_runs_biomass_annual_shellfish_v1_20_iterations_mean[87]
base_model_all_runs_biomass_annual_sandeel_v1_20_iterations_CV_Year_59 <- base_model_all_runs_biomass_annual_sandeel_v1_20_iterations_sd[87] / base_model_all_runs_biomass_annual_sandeel_v1_20_iterations_mean[87]

# given there are some fluctuations in the time-series (in a lot of cases,
# the predictions for future years do not settle to a steady state), it 
# is worth taking an average of the final 10 years for each of the 20 runs, 
# then take the coefficient of variation over the 20 runs for each of the 
# final-ten-year averages

base_model_all_runs_biomass_annual_pelagic_v1_sum

# this matrix is 110 x 20, containing the annual biomass for all pelagic fish
# for each year of data, for each of the 20 iterations

# we want to take the final 10 project years, years 50-59, and take the average
# of these, for each iteration, and each fish group

base_model_all_runs_catch_annual_demersal_v1_20_iterations_final_10_years_mean <- apply(base_model_all_runs_biomass_annual_demersal_v1_sum[78:87,], 2, mean)
base_model_all_runs_catch_annual_demersal_v1_20_iterations_final_10_years_20_iterations_mean <- mean(base_model_all_runs_catch_annual_demersal_v1_20_iterations_final_10_years_mean)
base_model_all_runs_catch_annual_demersal_v1_20_iterations_final_10_years_20_iterations_sd <- sd(base_model_all_runs_catch_annual_demersal_v1_20_iterations_final_10_years_mean)
base_model_all_runs_catch_annual_demersal_v1_20_iterations_final_10_years_20_iterations_CV <- base_model_all_runs_catch_annual_demersal_v1_20_iterations_final_10_years_20_iterations_sd / base_model_all_runs_catch_annual_demersal_v1_20_iterations_final_10_years_20_iterations_mean

base_model_all_runs_catch_annual_shellfish_v1_20_iterations_final_10_years_mean <- apply(base_model_all_runs_biomass_annual_shellfish_v1_sum[78:87,], 2, mean)
base_model_all_runs_catch_annual_shellfish_v1_20_iterations_final_10_years_20_iterations_mean <- mean(base_model_all_runs_catch_annual_shellfish_v1_20_iterations_final_10_years_mean)
base_model_all_runs_catch_annual_shellfish_v1_20_iterations_final_10_years_20_iterations_sd <- sd(base_model_all_runs_catch_annual_shellfish_v1_20_iterations_final_10_years_mean)
base_model_all_runs_catch_annual_shellfish_v1_20_iterations_final_10_years_20_iterations_CV <- base_model_all_runs_catch_annual_shellfish_v1_20_iterations_final_10_years_20_iterations_sd / base_model_all_runs_catch_annual_shellfish_v1_20_iterations_final_10_years_20_iterations_mean

base_model_all_runs_catch_annual_pelagic_v1_20_iterations_final_10_years_mean <- apply(base_model_all_runs_biomass_annual_pelagic_v1_sum[78:87,], 2, mean)
base_model_all_runs_catch_annual_pelagic_v1_20_iterations_final_10_years_20_iterations_mean <- mean(base_model_all_runs_catch_annual_pelagic_v1_20_iterations_final_10_years_mean)
base_model_all_runs_catch_annual_pelagic_v1_20_iterations_final_10_years_20_iterations_sd <- sd(base_model_all_runs_catch_annual_pelagic_v1_20_iterations_final_10_years_mean)
base_model_all_runs_catch_annual_pelagic_v1_20_iterations_final_10_years_20_iterations_CV <- base_model_all_runs_catch_annual_pelagic_v1_20_iterations_final_10_years_20_iterations_sd / base_model_all_runs_catch_annual_pelagic_v1_20_iterations_final_10_years_20_iterations_mean

base_model_all_runs_catch_annual_sandeel_v1_20_iterations_final_10_years_mean <- apply(base_model_all_runs_biomass_annual_sandeel_v1_20_iterations[78:87,], 2, mean)
base_model_all_runs_catch_annual_sandeel_v1_20_iterations_final_10_years_20_iterations_mean <- mean(base_model_all_runs_catch_annual_sandeel_v1_20_iterations_final_10_years_mean)
base_model_all_runs_catch_annual_sandeel_v1_20_iterations_final_10_years_20_iterations_sd <- sd(base_model_all_runs_catch_annual_sandeel_v1_20_iterations_final_10_years_mean)
base_model_all_runs_catch_annual_sandeel_v1_20_iterations_final_10_years_20_iterations_CV <- base_model_all_runs_catch_annual_sandeel_v1_20_iterations_final_10_years_20_iterations_sd / base_model_all_runs_catch_annual_sandeel_v1_20_iterations_final_10_years_20_iterations_mean

base_model_all_runs_catch_annual_pelagic_eat_sndl_v1_20_iterations_final_10_years_mean <- apply(base_model_all_runs_biomass_annual_pelagic_eat_sndl_20_iterations[78:87,], 2, mean)
base_model_all_runs_catch_annual_pelagic_eat_sndl_v1_20_iterations_final_10_years_20_iterations_mean <- mean(base_model_all_runs_catch_annual_pelagic_eat_sndl_v1_20_iterations_final_10_years_mean)
base_model_all_runs_catch_annual_pelagic_eat_sndl_v1_20_iterations_final_10_years_20_iterations_sd <- sd(base_model_all_runs_catch_annual_pelagic_eat_sndl_v1_20_iterations_final_10_years_mean)
base_model_all_runs_catch_annual_pelagic_eat_sndl_v1_20_iterations_final_10_years_20_iterations_CV <- base_model_all_runs_catch_annual_pelagic_eat_sndl_v1_20_iterations_final_10_years_20_iterations_sd / base_model_all_runs_catch_annual_pelagic_eat_sndl_v1_20_iterations_final_10_years_20_iterations_mean

base_model_all_runs_catch_annual_demersal_eat_sndl_v1_20_iterations_final_10_years_mean <- apply(base_model_all_runs_biomass_annual_demersal_eat_sndl_sum[78:87,], 2, mean)
base_model_all_runs_catch_annual_demersal_eat_sndl_v1_20_iterations_final_10_years_20_iterations_mean <- mean(base_model_all_runs_catch_annual_demersal_eat_sndl_v1_20_iterations_final_10_years_mean)
base_model_all_runs_catch_annual_demersal_eat_sndl_v1_20_iterations_final_10_years_20_iterations_sd <- sd(base_model_all_runs_catch_annual_demersal_eat_sndl_v1_20_iterations_final_10_years_mean)
base_model_all_runs_catch_annual_demersal_eat_sndl_v1_20_iterations_final_10_years_20_iterations_CV <- base_model_all_runs_catch_annual_demersal_eat_sndl_v1_20_iterations_final_10_years_20_iterations_sd / base_model_all_runs_catch_annual_demersal_eat_sndl_v1_20_iterations_final_10_years_20_iterations_mean

base_model_all_runs_catch_annual_surface_feeding_birds_v1_20_iterations_final_10_years_mean <- apply(base_model_all_runs_biomass_annual_surface_feeding_birds_v1_20_iterations[78:87,], 2, mean)
base_model_all_runs_catch_annual_surface_feeding_birds_v1_20_iterations_final_10_years_20_iterations_mean <- mean(base_model_all_runs_catch_annual_surface_feeding_birds_v1_20_iterations_final_10_years_mean)
base_model_all_runs_catch_annual_surface_feeding_birds_v1_20_iterations_final_10_years_20_iterations_sd <- sd(base_model_all_runs_catch_annual_surface_feeding_birds_v1_20_iterations_final_10_years_mean)
base_model_all_runs_catch_annual_surface_feeding_birds_v1_20_iterations_final_10_years_20_iterations_CV <- base_model_all_runs_catch_annual_surface_feeding_birds_v1_20_iterations_final_10_years_20_iterations_sd / base_model_all_runs_catch_annual_surface_feeding_birds_v1_20_iterations_final_10_years_20_iterations_mean

base_model_all_runs_catch_annual_diving_seabirds_v1_20_iterations_final_10_years_mean <- apply(base_model_all_runs_biomass_annual_diving_seabirds_v1_20_iterations[78:87,], 2, mean)
base_model_all_runs_catch_annual_diving_seabirds_v1_20_iterations_final_10_years_20_iterations_mean <- mean(base_model_all_runs_catch_annual_diving_seabirds_v1_20_iterations_final_10_years_mean)
base_model_all_runs_catch_annual_diving_seabirds_v1_20_iterations_final_10_years_20_iterations_sd <- sd(base_model_all_runs_catch_annual_diving_seabirds_v1_20_iterations_final_10_years_mean)
base_model_all_runs_catch_annual_diving_seabirds_v1_20_iterations_final_10_years_20_iterations_CV <- base_model_all_runs_catch_annual_diving_seabirds_v1_20_iterations_final_10_years_20_iterations_sd / base_model_all_runs_catch_annual_diving_seabirds_v1_20_iterations_final_10_years_20_iterations_mean

