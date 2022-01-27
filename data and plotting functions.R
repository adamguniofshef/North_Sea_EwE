# a separate script for writing a function for setting up
# the data to be plotted, then another function for the plotting

# trying to automate the process as much as possible

North_Sea_EwE_data <- function(output, n_iter = 20, total_years = 110, project_years = 28:87){
  
  # outputs we have considered: "biomass_annual", "catch_annual"
  
  func_groups_output <- read.csv("C:\\Users\\adamg\\Google Drive\\Natural England project\\Previous report\\NorthSea_EwE_basic_estimates_functional_groups.csv")
  func_groups_output <- func_groups_output[!is.na(func_groups_output[, 1]), ]
  func_groups <- func_groups_output[, 2]
  
  data <- read.csv(paste0("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_small_model_set\\EcoSampler\\Base_model\\", output, ".csv"),
                                                      skip = 9)
  data[,-1] <- data[,-1] * 570000 # for both catch and biomass, the data need to be rescaled by area of North Sea considered, 570000km^2
  data_list <- vector("list", length = n_iter)
  data_list[[1]] <- data
  colnames(data_list[[1]]) <- c("year", func_groups)
  
  for(i in 1:(n_iter - 1)){
    
    data_list[[i + 1]] <- read.csv(paste0("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_small_model_set\\EcoSampler\\Run_", i, "\\", output, ".csv"),
                                                               skip = 9)
    colnames(data_list[[i + 1]]) <- c("year", func_groups)
    data_list[[i + 1]][,-1] <- data_list[[i + 1]][,-1] * 570000
    
  }
  
  fish_group <- c(rep(NA, 6), rep("demersal", 3), NA, rep("demersal", 11), NA, "pelagic", "demersal",
                     rep(NA, 2), rep("demersal", 2), rep("pelagic", 5), "sandeel", rep("demersal", 10),
                     rep(NA, 2), rep("demersal", 2), "pelagic", "shellfish", rep(NA,4), rep("shellfish", 2),
                     rep(NA, 2), "shellfish", rep(NA, 11))
  sandeel_diet <- c(NA, rep(F, 9), T, F, rep(T, 7), rep(F, 7), rep(T, 2), rep(F, 3),
                    T, rep(F, 9), T, F, T, rep(F, 26))
  
  for(i in 1:n_iter){
    
    data_list[[i]] <- rbind(data_list[[i]], fish_group)
    data_list[[i]] <- rbind(data_list[[i]], sandeel_diet)
    
  }
  
  return(list(data_list = data_list))
  
}

North_Sea_EwE_plot <- function(data_list, groups = c("fish_groups", "sandeel_diet", "seabirds",
                                                " whales", "seals"),
                               n_iter = 20, total_years = 110, project_years = 28:87,
                               output){
  
  if(sum(groups == "fish_groups") == 1){
    
    n_dmrsl <- length(which(data_list[[1]][(total_years + 1), ] == "demersal"))
    n_plgc <- length(which(data_list[[1]][(total_years + 1), ] == "pelagic"))
    n_sndl <- length(which(data_list[[1]][(total_years + 1), ] == "sandeel"))
    n_shllfsh <- length(which(data_list[[1]][(total_years + 1), ] == "shellfish"))
    
    data_list_dmrsl <- vector("list", n_iter)
    data_array_dmrsl <- array(NA, dim = c(total_years, n_dmrsl, n_iter)) # some issues with lists and character strings, so this is a workaround
    data_sum_dmrsl <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations
    data_list_plgc <- vector("list", n_iter)
    data_array_plgc <- array(NA, dim = c(total_years, n_plgc, n_iter)) # some issues with lists and character strings, so this is a workaround
    data_sum_plgc <- matrix(NA, total_years, n_iter) # 110 years, 20 iterationsbase_model_all_runs_biomass_annual_shellfish_v1 <- vector("list", n_iter)
    data_list_shllfsh <- vector("list", n_iter)
    data_array_shllfsh <- array(NA, dim = c(total_years, n_shllfsh, n_iter)) # some issues with lists and character strings, so this is a workaround
    data_sum_shllfsh <- matrix(NA, total_years, n_iter) # 110 years, 20 iterationsbase_model_all_runs_biomass_annual_sandeel_v1_20_iterations <- matrix(NA, total_years, n_iter)
    data_sum_sndl <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations (only one sandeel functional group)
    
    for(i in 1:n_iter){
      
      data_list_dmrsl[[i]] <- data_list[[i]][,which(data_list[[1]][111,] == "demersal")]
      data_array_dmrsl[, , i] <- matrix(as.numeric(as.matrix(data_list_dmrsl[[i]][1:total_years, ])), total_years, n_dmrsl)
      data_sum_dmrsl[, i] <- apply(data_array_dmrsl[, , i], 1, sum)
      
      data_list_plgc[[i]] <- data_list[[i]][,which(data_list[[1]][111,] == "pelagic")]
      data_array_plgc[, , i] <- matrix(as.numeric(as.matrix(data_list_plgc[[i]][1:total_years, ])), total_years, n_plgc)
      data_sum_plgc[, i] <- apply(data_array_plgc[, , i], 1, sum)
      
      data_list_shllfsh[[i]] <- data_list[[i]][,which(data_list[[1]][111,] == "shellfish")]
      data_array_shllfsh[, , i] <- matrix(as.numeric(as.matrix(data_list_shllfsh[[i]][1:total_years, ])), total_years, n_shllfsh)
      data_sum_shllfsh[, i] <- apply(data_array_shllfsh[, , i], 1, sum)
      
      data_sum_sndl[, i] <- as.numeric(data_list[[i]]$Sandeels[1:total_years])
      
    }
    
    data_mean_dmrsl <- apply(data_sum_dmrsl, 1, mean)
    data_quantiles_dmrsl <- apply(data_sum_dmrsl, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_plgc <- apply(data_sum_plgc, 1, mean)
    data_quantiles_plgc <- apply(data_sum_plgc, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_shllfsh <- apply(data_sum_shllfsh, 1, mean)
    data_quantiles_shllfsh <- apply(data_sum_shllfsh, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_sndl <- apply(data_sum_sndl, 1, mean)
    data_quantiles_sndl <- apply(data_sum_sndl, 1, quantile, probs = c(0.025,0.975))
    
    plot_min <- min(data_quantiles_dmrsl[1, project_years], data_quantiles_plgc[1, project_years],
                    data_quantiles_shllfsh[1, project_years], data_quantiles_sndl[1, project_years])
    plot_max <- max(data_quantiles_dmrsl[2, project_years], data_quantiles_plgc[2, project_years],
                    data_quantiles_shllfsh[2, project_years], data_quantiles_sndl[2, project_years])
    
    options(scipen=10000)
    plot(as.numeric(data_list[[1]]$year[project_years]) - 2018,
         data_mean_sndl[project_years], type = "l",
         ylab = output, xlab = "Project Year",
         ylim = c(plot_min, plot_max),
         col = "blue")
    lines(as.numeric(data_list[[1]]$year[project_years]) - 2018,
          data_quantiles_sndl[1,project_years], lty = 2, col = "blue")
    lines(as.numeric(data_list[[1]]$year[project_years]) - 2018,
          data_quantiles_sndl[2,project_years], lty = 2, col = "blue")
    lines(as.numeric(data_list[[1]]$year[project_years]) - 2018,
          data_mean_plgc[project_years], lty = 1, col = "red")
    lines(as.numeric(data_list[[1]]$year[project_years]) - 2018,
          data_quantiles_plgc[1,project_years], lty = 2, col = "red")
    lines(as.numeric(data_list[[1]]$year[project_years]) - 2018,
          data_quantiles_plgc[2,project_years], lty = 2, col = "red")
    lines(as.numeric(data_list[[1]]$year[project_years]) - 2018,
          data_mean_dmrsl[project_years], lty = 1, col = "green")
    lines(as.numeric(data_list[[1]]$year[project_years]) - 2018,
          data_quantiles_dmrsl[1,project_years], lty = 2, col = "green")
    lines(as.numeric(data_list[[1]]$year[project_years]) - 2018,
          data_quantiles_dmrsl[2,project_years], lty = 2, col = "green")
    lines(as.numeric(data_list[[1]]$year[project_years]) - 2018,
          data_mean_shllfsh[project_years], lty = 1, col = "orange")
    lines(as.numeric(data_list[[1]]$year[project_years]) - 2018,
          data_quantiles_shllfsh[1,project_years], lty = 2, col = "orange")
    lines(as.numeric(data_list[[1]]$year[project_years]) - 2018,
          data_quantiles_shllfsh[2,project_years], lty = 2, col = "orange")
    
    
  }
  
  if(sum(groups == "sandeel_diet") == 1){
    
    dmrsl_eat_sndl <- which(data_list[[1]][(total_years + 1), ] == "demersal" & data_list[[1]][(total_years + 2), ] == T)
    n_dmrsl_eat_sndl <- length(dmrsl_eat_sndl)
    plgc_eat_sndl <- which(data_list[[1]][(total_years + 1), ] == "pelagic" & data_list[[1]][(total_years + 2), ] == T)
    n_plgc_eat_sndl <- length(plgc_eat_sndl)
    n_sndl <- length(which(data_list[[1]][(total_years + 1), ] == "sandeel"))
    
    data_list_dmrsl_eat_sndl <- vector("list", n_iter)
    data_array_dmrsl_eat_sndl <- array(NA, dim = c(total_years, n_dmrsl_eat_sndl, n_iter)) # some issues with lists and character strings, so this is a workaround
    data_sum_dmrsl_eat_sndl <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations
    data_list_plgc_eat_sndl <- vector("list", n_iter)
    data_array_plgc_eat_sndl <- array(NA, dim = c(total_years, n_plgc_eat_sndl, n_iter)) # some issues with lists and character strings, so this is a workaround
    data_sum_plgc_eat_sndl <- matrix(NA, total_years, n_iter) # 110 years, 20 iterationsbase_model_all_runs_biomass_annual_shellfish_v1 <- vector("list", n_iter)
    data_sum_sndl <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations (only one sandeel functional group)
    
    for(i in 1:n_iter){
      
      data_list_dmrsl_eat_sndl[[i]] <- data_list[[i]][,dmrsl_eat_sndl]
      data_array_dmrsl_eat_sndl[, , i] <- matrix(as.numeric(as.matrix(data_list_dmrsl_eat_sndl[[i]][1:total_years, ])), total_years, n_dmrsl_eat_sndl)
      data_sum_dmrsl_eat_sndl[, i] <- apply(data_array_dmrsl_eat_sndl[, , i], 1, sum)
      
      data_list_plgc_eat_sndl[[i]] <- data_list[[i]][,plgc_eat_sndl]
      data_array_plgc_eat_sndl[, , i] <- matrix(as.numeric(as.matrix(data_list_plgc[[i]][1:total_years, ])), total_years, n_plgc)
      data_sum_plgc_eat_sndl[, i] <- apply(data_array_plgc[, , i], 1, sum)
      
      data_sum_sndl[, i] <- as.numeric(data_list[[i]]$Sandeels[1:total_years])
      
    }
    
    data_mean_dmrsl_eat_sndl <- apply(data_sum_dmrsl_eat_sndl, 1, mean)
    data_quantiles_dmrsl_eat_sndl <- apply(data_sum_dmrsl_eat_sndl, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_plgc_eat_sndl <- apply(data_sum_plgc_eat_sndl, 1, mean)
    data_quantiles_plgc_eat_sndl <- apply(data_sum_plgc_eat_sndl, 1, quantile, probs = c(0.025,0.975))
  
    data_mean_sndl <- apply(data_sum_sndl, 1, mean)
    data_quantiles_sndl <- apply(data_sum_sndl, 1, quantile, probs = c(0.025,0.975))
    
    plot_min <- min(data_quantiles_dmrsl_eat_sndl[1, project_years], data_quantiles_plgc_eat_sndl[1, project_years],
                    data_quantiles_sndl[1, project_years])
    plot_max <- max(data_quantiles_dmrsl_eat_sndl[2, project_years], data_quantiles_plgc_eat_sndl[2, project_years],
                    data_quantiles_sndl[2, project_years])
    
    options(scipen=10000)
    plot(as.numeric(data_list[[1]]$year[project_years]) - 2018,
         data_mean_sndl[project_years], type = "l",
         ylab = output, xlab = "Project Year",
         ylim = c(plot_min, plot_max),
         col = "blue")
    lines(as.numeric(data_list[[1]]$year[project_years]) - 2018,
          data_quantiles_sndl[1,project_years], lty = 2, col = "blue")
    lines(as.numeric(data_list[[1]]$year[project_years]) - 2018,
          data_quantiles_sndl[2,project_years], lty = 2, col = "blue")
    lines(as.numeric(data_list[[1]]$year[project_years]) - 2018,
          data_mean_plgc_eat_sndl[project_years], lty = 1, col = "red")
    lines(as.numeric(data_list[[1]]$year[project_years]) - 2018,
          data_quantiles_plgc_eat_sndl[1,project_years], lty = 2, col = "red")
    lines(as.numeric(data_list[[1]]$year[project_years]) - 2018,
          data_quantiles_plgc_eat_sndl[2,project_years], lty = 2, col = "red")
    lines(as.numeric(data_list[[1]]$year[project_years]) - 2018,
          data_mean_dmrsl_eat_sndl[project_years], lty = 1, col = "green")
    lines(as.numeric(data_list[[1]]$year[project_years]) - 2018,
          data_quantiles_dmrsl_eat_sndl[1,project_years], lty = 2, col = "green")
    lines(as.numeric(data_list[[1]]$year[project_years]) - 2018,
          data_quantiles_dmrsl_eat_sndl[2,project_years], lty = 2, col = "green")

    
  }
  
  
}