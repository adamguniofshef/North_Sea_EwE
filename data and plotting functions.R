# a separate script for writing a function for setting up
# the data to be plotted, then another function for the plotting

# trying to automate the process as much as possible

North_Sea_EwE_data <- function(output, n_iter = 40, total_years = 110, project_years = 28:87, amalgamate = T,
                               all_scenarios = T){
  
  # outputs we have considered: "biomass_annual", "catch_annual"
  if(all_scenarios){
    
    func_groups_output <- read.csv("C:\\Users\\adamg\\Google Drive\\Natural England project\\Previous report\\NorthSea_EwE_basic_estimates_functional_groups.csv")
    func_groups_output <- func_groups_output[!is.na(func_groups_output[, 1]), ]
    func_groups <- func_groups_output[, 2]
    
    data_MSY <- read.csv(paste0("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_3_scenarios\\NSea_sandeel_MSY\\Sample_baseline\\ecosim_Basic setup\\", output, ".csv"),
                         skip = 9)
    data_half_MSY <- read.csv(paste0("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_3_scenarios\\NSea_sandeel_half_MSY\\Sample_baseline\\ecosim_Basic setup\\", output, ".csv"),
                              skip = 9)
    data_no_F <- read.csv(paste0("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_3_scenarios\\NSea_sandeel_no_F\\Sample_baseline\\ecosim_Basic setup\\", output, ".csv"),
                          skip = 9)
    data_MSY[,-1] <- data_MSY[,-1] * 570000 # for both catch and biomass, the data need to be rescaled by area of North Sea considered, 570000km^2
    data_half_MSY[,-1] <- data_half_MSY[,-1] * 570000
    data_no_F[,-1] <- data_no_F[,-1] * 570000
    data_list_MSY <- vector("list", length = n_iter)
    data_list_half_MSY <- vector("list", length = n_iter)
    data_list_no_F <- vector("list", length = n_iter)
    data_list_MSY[[1]] <- data_MSY
    data_list_half_MSY[[1]] <- data_half_MSY
    data_list_no_F[[1]] <- data_no_F
    colnames(data_list_MSY[[1]]) <- c("year", func_groups)
    colnames(data_list_half_MSY[[1]]) <- c("year", func_groups)
    colnames(data_list_no_F[[1]]) <- c("year", func_groups)
    
    for(i in 1:(n_iter - 1)){
      
      data_list_MSY[[i + 1]] <- read.csv(paste0("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_3_scenarios\\NSea_sandeel_MSY\\Sample_", i, "\\ecosim_Basic setup\\", output, ".csv"),
                                         skip = 9)
      colnames(data_list_MSY[[i + 1]]) <- c("year", func_groups)
      data_list_MSY[[i + 1]][,-1] <- data_list_MSY[[i + 1]][,-1] * 570000
      
      data_list_half_MSY[[i + 1]] <- read.csv(paste0("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_3_scenarios\\NSea_sandeel_half_MSY\\Sample_", i, "\\ecosim_Basic setup\\", output, ".csv"),
                                              skip = 9)
      colnames(data_list_half_MSY[[i + 1]]) <- c("year", func_groups)
      data_list_half_MSY[[i + 1]][,-1] <- data_list_half_MSY[[i + 1]][,-1] * 570000
      
      data_list_no_F[[i + 1]] <- read.csv(paste0("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_3_scenarios\\NSea_sandeel_no_F\\Sample_", i, "\\ecosim_Basic setup\\", output, ".csv"),
                                          skip = 9)
      colnames(data_list_no_F[[i + 1]]) <- c("year", func_groups)
      data_list_no_F[[i + 1]][,-1] <- data_list_no_F[[i + 1]][,-1] * 570000
      
    }
    
    if(amalgamate){ # want to combine some functional groups given split into juvenile and adult 
      
      # want 13+14, 15+16, 17+18, 19+20, 28+29. remember column 1 is year, so need to add on one to each column
      for(i in 1:n_iter){
        
        data_list_MSY[[i]][, 14] <- data_list_MSY[[i]][, 14] + data_list_MSY[[i]][, 15]
        data_list_MSY[[i]][, 16] <- data_list_MSY[[i]][, 16] + data_list_MSY[[i]][, 17]
        data_list_MSY[[i]][, 18] <- data_list_MSY[[i]][, 18] + data_list_MSY[[i]][, 19]
        data_list_MSY[[i]][, 20] <- data_list_MSY[[i]][, 20] + data_list_MSY[[i]][, 21]
        data_list_MSY[[i]][, 29] <- data_list_MSY[[i]][, 29] + data_list_MSY[[i]][, 30]
        colnames(data_list_MSY[[i]])[c(14, 16, 18, 20, 29)] <- c("Cod", "Whiting", "Haddock", "Saithe", "Herring")
        data_list_MSY[[i]] <- data_list_MSY[[i]][, -c(15, 17, 19, 21, 30)]
        
        data_list_half_MSY[[i]][, 14] <- data_list_half_MSY[[i]][, 14] + data_list_half_MSY[[i]][, 15]
        data_list_half_MSY[[i]][, 16] <- data_list_half_MSY[[i]][, 16] + data_list_half_MSY[[i]][, 17]
        data_list_half_MSY[[i]][, 18] <- data_list_half_MSY[[i]][, 18] + data_list_half_MSY[[i]][, 19]
        data_list_half_MSY[[i]][, 20] <- data_list_half_MSY[[i]][, 20] + data_list_half_MSY[[i]][, 21]
        data_list_half_MSY[[i]][, 29] <- data_list_half_MSY[[i]][, 29] + data_list_half_MSY[[i]][, 30]
        colnames(data_list_half_MSY[[i]])[c(14, 16, 18, 20, 29)] <- c("Cod", "Whiting", "Haddock", "Saithe", "Herring")
        data_list_half_MSY[[i]] <- data_list_half_MSY[[i]][, -c(15, 17, 19, 21, 30)]
        
        data_list_no_F[[i]][, 14] <- data_list_no_F[[i]][, 14] + data_list_no_F[[i]][, 15]
        data_list_no_F[[i]][, 16] <- data_list_no_F[[i]][, 16] + data_list_no_F[[i]][, 17]
        data_list_no_F[[i]][, 18] <- data_list_no_F[[i]][, 18] + data_list_no_F[[i]][, 19]
        data_list_no_F[[i]][, 20] <- data_list_no_F[[i]][, 20] + data_list_no_F[[i]][, 21]
        data_list_no_F[[i]][, 29] <- data_list_no_F[[i]][, 29] + data_list_no_F[[i]][, 30]
        colnames(data_list_no_F[[i]])[c(14, 16, 18, 20, 29)] <- c("Cod", "Whiting", "Haddock", "Saithe", "Herring")
        data_list_no_F[[i]] <- data_list_no_F[[i]][, -c(15, 17, 19, 21, 30)]
        
      }
      
      fish_group <- c(rep(NA, 6), rep("demersal", 3), NA, rep("demersal", 7), NA, "pelagic", "demersal",
                      rep(NA, 2), rep("demersal", 2), rep("pelagic", 4), "sandeel", rep("demersal", 10),
                      rep(NA, 2),
                      #rep("demersal", 2), "pelagic",
                      rep(NA,3),
                      "shellfish", rep(NA,4), rep("shellfish", 2),
                      rep(NA, 2), "shellfish", rep(NA, 11))
      sandeel_diet <- c(NA, rep(F, 9), T, F, rep(T, 4), rep(F, 6), rep(T, 2), rep(F, 2),
                        T, rep(F, 9), T, F, T, rep(F, 26))
      
    }else{
      
      fish_group <- c(rep(NA, 6), rep("demersal", 3), NA, rep("demersal", 11), NA, "pelagic", "demersal",
                      rep(NA, 2), rep("demersal", 2), rep("pelagic", 5), "sandeel", rep("demersal", 10),
                      rep(NA, 2),
                      #rep("demersal", 2), "pelagic",
                      rep(NA,3),
                      "shellfish", rep(NA,4), rep("shellfish", 2),
                      rep(NA, 2), "shellfish", rep(NA, 11))
      sandeel_diet <- c(NA, rep(F, 9), T, F, rep(T, 7), rep(F, 7), rep(T, 2), rep(F, 3),
                        T, rep(F, 9), T, F, T, rep(F, 26))
      
    }
    
    for(i in 1:n_iter){
      
      data_list_MSY[[i]] <- rbind(data_list_MSY[[i]], fish_group)
      data_list_MSY[[i]] <- rbind(data_list_MSY[[i]], sandeel_diet)
      
      data_list_half_MSY[[i]] <- rbind(data_list_half_MSY[[i]], fish_group)
      data_list_half_MSY[[i]] <- rbind(data_list_half_MSY[[i]], sandeel_diet)
      
      data_list_no_F[[i]] <- rbind(data_list_no_F[[i]], fish_group)
      data_list_no_F[[i]] <- rbind(data_list_no_F[[i]], sandeel_diet)
      
    }
    
    return(list(data_list_MSY = data_list_MSY, data_list_half_MSY = data_list_half_MSY,
                data_list_no_F = data_list_no_F))
    
  }else{
    
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
    
    if(amalgamate){ # want to combine some functional groups given split into juvenile and adult 
      
      # want 13+14, 15+16, 17+18, 19+20, 28+29. remember column 1 is year, so need to add on one to each column
      for(i in 1:n_iter){
        
        data_list[[i]][, 14] <- data_list[[i]][, 14] + data_list[[i]][, 15]
        data_list[[i]][, 16] <- data_list[[i]][, 16] + data_list[[i]][, 17]
        data_list[[i]][, 18] <- data_list[[i]][, 18] + data_list[[i]][, 19]
        data_list[[i]][, 20] <- data_list[[i]][, 20] + data_list[[i]][, 21]
        data_list[[i]][, 29] <- data_list[[i]][, 29] + data_list[[i]][, 30]
        colnames(data_list[[i]])[c(14, 16, 18, 20, 29)] <- c("Cod", "Whiting", "Haddock", "Saithe", "Herring")
        data_list[[i]] <- data_list[[i]][, -c(15, 17, 19, 21, 30)]
        
      }
      
      fish_group <- c(rep(NA, 6), rep("demersal", 3), NA, rep("demersal", 7), NA, "pelagic", "demersal",
                      rep(NA, 2), rep("demersal", 2), rep("pelagic", 4), "sandeel", rep("demersal", 10),
                      rep(NA, 2),
                      #rep("demersal", 2), "pelagic",
                      rep(NA,3),
                      "shellfish", rep(NA,4), rep("shellfish", 2),
                      rep(NA, 2), "shellfish", rep(NA, 11))
      sandeel_diet <- c(NA, rep(F, 9), T, F, rep(T, 4), rep(F, 6), rep(T, 2), rep(F, 2),
                        T, rep(F, 9), T, F, T, rep(F, 26))
      
    }else{
      
      fish_group <- c(rep(NA, 6), rep("demersal", 3), NA, rep("demersal", 11), NA, "pelagic", "demersal",
                      rep(NA, 2), rep("demersal", 2), rep("pelagic", 5), "sandeel", rep("demersal", 10),
                      rep(NA, 2),
                      #rep("demersal", 2), "pelagic",
                      rep(NA,3),
                      "shellfish", rep(NA,4), rep("shellfish", 2),
                      rep(NA, 2), "shellfish", rep(NA, 11))
      sandeel_diet <- c(NA, rep(F, 9), T, F, rep(T, 7), rep(F, 7), rep(T, 2), rep(F, 3),
                        T, rep(F, 9), T, F, T, rep(F, 26))
      
    }
    
    for(i in 1:n_iter){
      
      data_list[[i]] <- rbind(data_list[[i]], fish_group)
      data_list[[i]] <- rbind(data_list[[i]], sandeel_diet)
      
    }
    
    
    return(data_list = data_list)
    
  }
  
}

biomass_annual_test_3_scenarios <- North_Sea_EwE_data(output = "biomass_annual", n_iter = 40, total_years = 110,
                                                      project_years = 28:87)

North_Sea_EwE_plot <- function(data_list_MSY, data_list_half_MSY, 
                               data_list_no_F, groups = c("fish_groups", "sandeel_diet", "seabirds",
                                                "whales", "seals"),
                               n_iter = 40, total_years = 110, project_years = 28:87,
                               output){
  
  if(sum(groups == "fish_groups") == 1){
    
    n_dmrsl <- length(which(data_list_MSY[[1]][(total_years + 1), ] == "demersal"))
    n_plgc <- length(which(data_list_MSY[[1]][(total_years + 1), ] == "pelagic"))
    n_sndl <- length(which(data_list_MSY[[1]][(total_years + 1), ] == "sandeel"))
    n_shllfsh <- length(which(data_list_MSY[[1]][(total_years + 1), ] == "shellfish"))
    
    data_list_MSY_dmrsl <- vector("list", n_iter)
    data_array_MSY_dmrsl <- array(NA, dim = c(total_years, n_dmrsl, n_iter)) # some issues with lists and character strings, so this is a workaround
    data_sum_MSY_dmrsl <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations
    data_list_MSY_plgc <- vector("list", n_iter)
    data_array_MSY_plgc <- array(NA, dim = c(total_years, n_plgc, n_iter)) # some issues with lists and character strings, so this is a workaround
    data_sum_MSY_plgc <- matrix(NA, total_years, n_iter) # 110 years, 20 iterationsbase_model_all_runs_biomass_annual_shellfish_v1 <- vector("list", n_iter)
    data_list_MSY_shllfsh <- vector("list", n_iter)
    data_array_MSY_shllfsh <- array(NA, dim = c(total_years, n_shllfsh, n_iter)) # some issues with lists and character strings, so this is a workaround
    data_sum_MSY_shllfsh <- matrix(NA, total_years, n_iter) # 110 years, 20 iterationsbase_model_all_runs_biomass_annual_sandeel_v1_20_iterations <- matrix(NA, total_years, n_iter)
    data_sum_MSY_sndl <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations (only one sandeel functional group)
    
    data_list_half_MSY_dmrsl <- vector("list", n_iter)
    data_array_half_MSY_dmrsl <- array(NA, dim = c(total_years, n_dmrsl, n_iter)) # some issues with lists and character strings, so this is a workaround
    data_sum_half_MSY_dmrsl <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations
    data_list_half_MSY_plgc <- vector("list", n_iter)
    data_array_half_MSY_plgc <- array(NA, dim = c(total_years, n_plgc, n_iter)) # some issues with lists and character strings, so this is a workaround
    data_sum_half_MSY_plgc <- matrix(NA, total_years, n_iter) # 110 years, 20 iterationsbase_model_all_runs_biomass_annual_shellfish_v1 <- vector("list", n_iter)
    data_list_half_MSY_shllfsh <- vector("list", n_iter)
    data_array_half_MSY_shllfsh <- array(NA, dim = c(total_years, n_shllfsh, n_iter)) # some issues with lists and character strings, so this is a workaround
    data_sum_half_MSY_shllfsh <- matrix(NA, total_years, n_iter) # 110 years, 20 iterationsbase_model_all_runs_biomass_annual_sandeel_v1_20_iterations <- matrix(NA, total_years, n_iter)
    data_sum_half_MSY_sndl <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations (only one sandeel functional group)
    
    data_list_no_F_dmrsl <- vector("list", n_iter)
    data_array_no_F_dmrsl <- array(NA, dim = c(total_years, n_dmrsl, n_iter)) # some issues with lists and character strings, so this is a workaround
    data_sum_no_F_dmrsl <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations
    data_list_no_F_plgc <- vector("list", n_iter)
    data_array_no_F_plgc <- array(NA, dim = c(total_years, n_plgc, n_iter)) # some issues with lists and character strings, so this is a workaround
    data_sum_no_F_plgc <- matrix(NA, total_years, n_iter) # 110 years, 20 iterationsbase_model_all_runs_biomass_annual_shellfish_v1 <- vector("list", n_iter)
    data_list_no_F_shllfsh <- vector("list", n_iter)
    data_array_no_F_shllfsh <- array(NA, dim = c(total_years, n_shllfsh, n_iter)) # some issues with lists and character strings, so this is a workaround
    data_sum_no_F_shllfsh <- matrix(NA, total_years, n_iter) # 110 years, 20 iterationsbase_model_all_runs_biomass_annual_sandeel_v1_20_iterations <- matrix(NA, total_years, n_iter)
    data_sum_no_F_sndl <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations (only one sandeel functional group)
    
    for(i in 1:n_iter){
      
      data_list_MSY_dmrsl[[i]] <- data_list_MSY[[i]][,which(data_list_MSY[[1]][111,] == "demersal")]
      data_array_MSY_dmrsl[, , i] <- matrix(as.numeric(as.matrix(data_list_MSY_dmrsl[[i]][1:total_years, ])), total_years, n_dmrsl)
      data_sum_MSY_dmrsl[, i] <- apply(data_array_MSY_dmrsl[, , i], 1, sum)
      
      data_list_MSY_plgc[[i]] <- data_list_MSY[[i]][,which(data_list_MSY[[1]][111,] == "pelagic")]
      data_array_MSY_plgc[, , i] <- matrix(as.numeric(as.matrix(data_list_MSY_plgc[[i]][1:total_years, ])), total_years, n_plgc)
      data_sum_MSY_plgc[, i] <- apply(data_array_MSY_plgc[, , i], 1, sum)
      
      data_list_MSY_shllfsh[[i]] <- data_list_MSY[[i]][,which(data_list_MSY[[1]][111,] == "shellfish")]
      data_array_MSY_shllfsh[, , i] <- matrix(as.numeric(as.matrix(data_list_MSY_shllfsh[[i]][1:total_years, ])), total_years, n_shllfsh)
      data_sum_MSY_shllfsh[, i] <- apply(data_array_MSY_shllfsh[, , i], 1, sum)
      
      data_sum_MSY_sndl[, i] <- as.numeric(data_list_MSY[[i]]$Sandeels[1:total_years])
      
      
      data_list_half_MSY_dmrsl[[i]] <- data_list_half_MSY[[i]][,which(data_list_half_MSY[[1]][111,] == "demersal")]
      data_array_half_MSY_dmrsl[, , i] <- matrix(as.numeric(as.matrix(data_list_half_MSY_dmrsl[[i]][1:total_years, ])), total_years, n_dmrsl)
      data_sum_half_MSY_dmrsl[, i] <- apply(data_array_half_MSY_dmrsl[, , i], 1, sum)
      
      data_list_half_MSY_plgc[[i]] <- data_list_half_MSY[[i]][,which(data_list_half_MSY[[1]][111,] == "pelagic")]
      data_array_half_MSY_plgc[, , i] <- matrix(as.numeric(as.matrix(data_list_half_MSY_plgc[[i]][1:total_years, ])), total_years, n_plgc)
      data_sum_half_MSY_plgc[, i] <- apply(data_array_half_MSY_plgc[, , i], 1, sum)
      
      data_list_half_MSY_shllfsh[[i]] <- data_list_half_MSY[[i]][,which(data_list_half_MSY[[1]][111,] == "shellfish")]
      data_array_half_MSY_shllfsh[, , i] <- matrix(as.numeric(as.matrix(data_list_half_MSY_shllfsh[[i]][1:total_years, ])), total_years, n_shllfsh)
      data_sum_half_MSY_shllfsh[, i] <- apply(data_array_half_MSY_shllfsh[, , i], 1, sum)
      
      data_sum_half_MSY_sndl[, i] <- as.numeric(data_list_half_MSY[[i]]$Sandeels[1:total_years])
      
      
      data_list_no_F_dmrsl[[i]] <- data_list_no_F[[i]][,which(data_list_no_F[[1]][111,] == "demersal")]
      data_array_no_F_dmrsl[, , i] <- matrix(as.numeric(as.matrix(data_list_no_F_dmrsl[[i]][1:total_years, ])), total_years, n_dmrsl)
      data_sum_no_F_dmrsl[, i] <- apply(data_array_no_F_dmrsl[, , i], 1, sum)
      
      data_list_no_F_plgc[[i]] <- data_list_no_F[[i]][,which(data_list_no_F[[1]][111,] == "pelagic")]
      data_array_no_F_plgc[, , i] <- matrix(as.numeric(as.matrix(data_list_no_F_plgc[[i]][1:total_years, ])), total_years, n_plgc)
      data_sum_no_F_plgc[, i] <- apply(data_array_no_F_plgc[, , i], 1, sum)
      
      data_list_no_F_shllfsh[[i]] <- data_list_no_F[[i]][,which(data_list_no_F[[1]][111,] == "shellfish")]
      data_array_no_F_shllfsh[, , i] <- matrix(as.numeric(as.matrix(data_list_no_F_shllfsh[[i]][1:total_years, ])), total_years, n_shllfsh)
      data_sum_no_F_shllfsh[, i] <- apply(data_array_no_F_shllfsh[, , i], 1, sum)
      
      data_sum_no_F_sndl[, i] <- as.numeric(data_list_no_F[[i]]$Sandeels[1:total_years])
      
    }
    
    data_mean_MSY_dmrsl <- apply(data_sum_MSY_dmrsl, 1, mean)
    data_quantiles_MSY_dmrsl <- apply(data_sum_MSY_dmrsl, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_MSY_plgc <- apply(data_sum_MSY_plgc, 1, mean)
    data_quantiles_MSY_plgc <- apply(data_sum_MSY_plgc, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_MSY_shllfsh <- apply(data_sum_MSY_shllfsh, 1, mean)
    data_quantiles_MSY_shllfsh <- apply(data_sum_MSY_shllfsh, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_MSY_sndl <- apply(data_sum_MSY_sndl, 1, mean)
    data_quantiles_MSY_sndl <- apply(data_sum_MSY_sndl, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_half_MSY_dmrsl <- apply(data_sum_half_MSY_dmrsl, 1, mean)
    data_quantiles_half_MSY_dmrsl <- apply(data_sum_half_MSY_dmrsl, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_half_MSY_plgc <- apply(data_sum_half_MSY_plgc, 1, mean)
    data_quantiles_half_MSY_plgc <- apply(data_sum_half_MSY_plgc, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_half_MSY_shllfsh <- apply(data_sum_half_MSY_shllfsh, 1, mean)
    data_quantiles_half_MSY_shllfsh <- apply(data_sum_half_MSY_shllfsh, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_half_MSY_sndl <- apply(data_sum_half_MSY_sndl, 1, mean)
    data_quantiles_half_MSY_sndl <- apply(data_sum_half_MSY_sndl, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_no_F_dmrsl <- apply(data_sum_no_F_dmrsl, 1, mean)
    data_quantiles_no_F_dmrsl <- apply(data_sum_no_F_dmrsl, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_no_F_plgc <- apply(data_sum_no_F_plgc, 1, mean)
    data_quantiles_no_F_plgc <- apply(data_sum_no_F_plgc, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_no_F_shllfsh <- apply(data_sum_no_F_shllfsh, 1, mean)
    data_quantiles_no_F_shllfsh <- apply(data_sum_no_F_shllfsh, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_no_F_sndl <- apply(data_sum_no_F_sndl, 1, mean)
    data_quantiles_no_F_sndl <- apply(data_sum_no_F_sndl, 1, quantile, probs = c(0.025,0.975))
    
    # First plot, Figure 7.1 --- baseline trends with uncertainty, so _MSY data
    
    plot_min_MSY <- min(data_quantiles_MSY_dmrsl[1, project_years], data_quantiles_MSY_plgc[1, project_years],
                    data_quantiles_MSY_shllfsh[1, project_years], data_quantiles_MSY_sndl[1, project_years])
    plot_max_MSY <- max(data_quantiles_MSY_dmrsl[2, project_years], data_quantiles_MSY_plgc[2, project_years],
                    data_quantiles_MSY_shllfsh[2, project_years], data_quantiles_MSY_sndl[2, project_years])
    
    options(scipen=10000)
    plot(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
         data_mean_MSY_sndl[project_years], type = "l",
         ylab = output, xlab = "Project Year",
         ylim = c(plot_min_MSY, plot_max_MSY),
         col = "blue", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_sndl[1,project_years], lty = 2, col = "blue", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_sndl[2,project_years], lty = 2, col = "blue", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_mean_MSY_plgc[project_years], lty = 1, col = "red", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_plgc[1,project_years], lty = 2, col = "red", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_plgc[2,project_years], lty = 2, col = "red", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_mean_MSY_dmrsl[project_years], lty = 1, col = "green", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_dmrsl[1,project_years], lty = 2, col = "green", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_dmrsl[2,project_years], lty = 2, col = "green", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_mean_MSY_shllfsh[project_years], lty = 1, col = "orange", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_shllfsh[1,project_years], lty = 2, col = "orange", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_shllfsh[2,project_years], lty = 2, col = "orange", lwd = 2)
    
    # Second plot, Figure 7.2 --- Baseline vs no sandeel fishing
    
    plot_min_no_F <- min(data_quantiles_no_F_dmrsl[1, project_years], data_quantiles_no_F_plgc[1, project_years],
                    data_quantiles_no_F_shllfsh[1, project_years], data_quantiles_no_F_sndl[1, project_years])
    plot_max_no_F <- max(data_quantiles_no_F_dmrsl[2, project_years], data_quantiles_no_F_plgc[2, project_years],
                    data_quantiles_no_F_shllfsh[2, project_years], data_quantiles_no_F_sndl[2, project_years])
    
    options(scipen=10000)
    plot(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
         data_mean_MSY_sndl[project_years], type = "l",
         ylab = output, xlab = "Project Year",
         ylim = c(min(plot_min_MSY, plot_min_no_F), max(plot_max_MSY, plot_max_no_F)),
         col = "blue", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_sndl[1,project_years], lty = 2, col = "blue", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_sndl[2,project_years], lty = 2, col = "blue", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_mean_MSY_plgc[project_years], lty = 1, col = "red", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_plgc[1,project_years], lty = 2, col = "red", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_plgc[2,project_years], lty = 2, col = "red", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_mean_MSY_dmrsl[project_years], lty = 1, col = "green", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_dmrsl[1,project_years], lty = 2, col = "green", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_dmrsl[2,project_years], lty = 2, col = "green", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_mean_MSY_shllfsh[project_years], lty = 1, col = "orange", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_shllfsh[1,project_years], lty = 2, col = "orange", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_shllfsh[2,project_years], lty = 2, col = "orange", lwd = 2)
    
    lines(as.numeric(data_list_no_F[[1]]$year[project_years]) - 2018,
          data_mean_no_F_sndl[project_years], lty = 3, col = "blue", lwd = 2)
    lines(as.numeric(data_list_no_F[[1]]$year[project_years]) - 2018,
          data_quantiles_no_F_sndl[1,project_years], lty = 4, col = "blue", lwd = 2)
    lines(as.numeric(data_list_no_F[[1]]$year[project_years]) - 2018,
          data_quantiles_no_F_sndl[2,project_years], lty = 4, col = "blue", lwd = 2)
    lines(as.numeric(data_list_no_F[[1]]$year[project_years]) - 2018,
          data_mean_no_F_plgc[project_years], lty = 3, col = "red", lwd = 2)
    lines(as.numeric(data_list_no_F[[1]]$year[project_years]) - 2018,
          data_quantiles_no_F_plgc[1,project_years], lty = 4, col = "red", lwd = 2)
    lines(as.numeric(data_list_no_F[[1]]$year[project_years]) - 2018,
          data_quantiles_no_F_plgc[2,project_years], lty = 4, col = "red", lwd = 2)
    lines(as.numeric(data_list_no_F[[1]]$year[project_years]) - 2018,
          data_mean_no_F_dmrsl[project_years], lty = 3, col = "green", lwd = 2)
    lines(as.numeric(data_list_no_F[[1]]$year[project_years]) - 2018,
          data_quantiles_no_F_dmrsl[1,project_years], lty = 4, col = "green", lwd = 2)
    lines(as.numeric(data_list_no_F[[1]]$year[project_years]) - 2018,
          data_quantiles_no_F_dmrsl[2,project_years], lty = 4, col = "green", lwd = 2)
    lines(as.numeric(data_list_no_F[[1]]$year[project_years]) - 2018,
          data_mean_no_F_shllfsh[project_years], lty = 3, col = "orange", lwd = 2)
    lines(as.numeric(data_list_no_F[[1]]$year[project_years]) - 2018,
          data_quantiles_no_F_shllfsh[1,project_years], lty = 4, col = "orange", lwd = 2)
    lines(as.numeric(data_list_no_F[[1]]$year[project_years]) - 2018,
          data_quantiles_no_F_shllfsh[2,project_years], lty = 4, col = "orange", lwd = 2)
    
    # Third plot, but Figure 7.4 (Figure 7.3 in sandeel_diet) --- baseline vs reduced
    
    plot_min_half_MSY <- min(data_quantiles_half_MSY_dmrsl[1, project_years], data_quantiles_half_MSY_plgc[1, project_years],
                         data_quantiles_half_MSY_shllfsh[1, project_years], data_quantiles_half_MSY_sndl[1, project_years])
    plot_max_half_MSY <- max(data_quantiles_half_MSY_dmrsl[2, project_years], data_quantiles_half_MSY_plgc[2, project_years],
                         data_quantiles_half_MSY_shllfsh[2, project_years], data_quantiles_half_MSY_sndl[2, project_years])
    
    options(scipen=10000)
    plot(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
         data_mean_MSY_sndl[project_years], type = "l",
         ylab = output, xlab = "Project Year",
         ylim = c(min(plot_min_MSY, plot_min_half_MSY), max(plot_max_MSY, plot_max_half_MSY)),
         col = "blue", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_sndl[1,project_years], lty = 2, col = "blue", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_sndl[2,project_years], lty = 2, col = "blue", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_mean_MSY_plgc[project_years], lty = 1, col = "red", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_plgc[1,project_years], lty = 2, col = "red", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_plgc[2,project_years], lty = 2, col = "red", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_mean_MSY_dmrsl[project_years], lty = 1, col = "green", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_dmrsl[1,project_years], lty = 2, col = "green", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_dmrsl[2,project_years], lty = 2, col = "green", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_mean_MSY_shllfsh[project_years], lty = 1, col = "orange", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_shllfsh[1,project_years], lty = 2, col = "orange", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_shllfsh[2,project_years], lty = 2, col = "orange", lwd = 2)
    
    lines(as.numeric(data_list_half_MSY[[1]]$year[project_years]) - 2018,
          data_mean_half_MSY_sndl[project_years], lty = 3, col = "blue", lwd = 2)
    lines(as.numeric(data_list_half_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_half_MSY_sndl[1,project_years], lty = 4, col = "blue", lwd = 2)
    lines(as.numeric(data_list_half_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_half_MSY_sndl[2,project_years], lty = 4, col = "blue", lwd = 2)
    lines(as.numeric(data_list_half_MSY[[1]]$year[project_years]) - 2018,
          data_mean_half_MSY_plgc[project_years], lty = 3, col = "red", lwd = 2)
    lines(as.numeric(data_list_half_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_half_MSY_plgc[1,project_years], lty = 4, col = "red", lwd = 2)
    lines(as.numeric(data_list_half_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_half_MSY_plgc[2,project_years], lty = 4, col = "red", lwd = 2)
    lines(as.numeric(data_list_half_MSY[[1]]$year[project_years]) - 2018,
          data_mean_half_MSY_dmrsl[project_years], lty = 3, col = "green", lwd = 2)
    lines(as.numeric(data_list_half_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_half_MSY_dmrsl[1,project_years], lty = 4, col = "green", lwd = 2)
    lines(as.numeric(data_list_half_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_half_MSY_dmrsl[2,project_years], lty = 4, col = "green", lwd = 2)
    lines(as.numeric(data_list_half_MSY[[1]]$year[project_years]) - 2018,
          data_mean_half_MSY_shllfsh[project_years], lty = 3, col = "orange", lwd = 2)
    lines(as.numeric(data_list_half_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_half_MSY_shllfsh[1,project_years], lty = 4, col = "orange", lwd = 2)
    lines(as.numeric(data_list_half_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_half_MSY_shllfsh[2,project_years], lty = 4, col = "orange", lwd = 2)
    
  }
  
  if(sum(groups == "sandeel_diet") == 1){
    
    dmrsl_eat_sndl <- which(data_list_MSY[[1]][(total_years + 1), ] == "demersal" & data_list_MSY[[1]][(total_years + 2), ] == T)
    n_dmrsl_eat_sndl <- length(dmrsl_eat_sndl)
    plgc_eat_sndl <- which(data_list_MSY[[1]][(total_years + 1), ] == "pelagic" & data_list_MSY[[1]][(total_years + 2), ] == T)
    n_plgc_eat_sndl <- length(plgc_eat_sndl)
    n_sndl <- length(which(data_list_MSY[[1]][(total_years + 1), ] == "sandeel"))
    
    data_list_MSY_dmrsl_eat_sndl <- vector("list", n_iter)
    data_array_MSY_dmrsl_eat_sndl <- array(NA, dim = c(total_years, n_dmrsl_eat_sndl, n_iter)) # some issues with lists and character strings, so this is a workaround
    data_sum_MSY_dmrsl_eat_sndl <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations
    
    data_sum_MSY_plgc_eat_sndl <- matrix(NA, total_years, n_iter) # 110 years, 20 iterationsbase_model_all_runs_biomass_annual_shellfish_v1 <- vector("list", n_iter)
    
    data_sum_MSY_sndl <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations (only one sandeel functional group)
    
    
    data_list_half_MSY_dmrsl_eat_sndl <- vector("list", n_iter)
    data_array_half_MSY_dmrsl_eat_sndl <- array(NA, dim = c(total_years, n_dmrsl_eat_sndl, n_iter)) # some issues with lists and character strings, so this is a workaround
    data_sum_half_MSY_dmrsl_eat_sndl <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations
    
    data_sum_half_MSY_plgc_eat_sndl <- matrix(NA, total_years, n_iter) # 110 years, 20 iterationsbase_model_all_runs_biomass_annual_shellfish_v1 <- vector("list", n_iter)
    
    data_sum_half_MSY_sndl <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations (only one sandeel functional group)
    
    
    data_list_no_F_dmrsl_eat_sndl <- vector("list", n_iter)
    data_array_no_F_dmrsl_eat_sndl <- array(NA, dim = c(total_years, n_dmrsl_eat_sndl, n_iter)) # some issues with lists and character strings, so this is a workaround
    data_sum_no_F_dmrsl_eat_sndl <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations
    
    data_sum_no_F_plgc_eat_sndl <- matrix(NA, total_years, n_iter) # 110 years, 20 iterationsbase_model_all_runs_biomass_annual_shellfish_v1 <- vector("list", n_iter)
    
    data_sum_no_F_sndl <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations (only one sandeel functional group)
    
    
    for(i in 1:n_iter){
      
      data_list_MSY_dmrsl_eat_sndl[[i]] <- data_list_MSY[[i]][,dmrsl_eat_sndl]
      data_array_MSY_dmrsl_eat_sndl[, , i] <- matrix(as.numeric(as.matrix(data_list_MSY_dmrsl_eat_sndl[[i]][1:total_years, ])), total_years, n_dmrsl_eat_sndl)
      data_sum_MSY_dmrsl_eat_sndl[, i] <- apply(data_array_MSY_dmrsl_eat_sndl[, , i], 1, sum)
      
      data_sum_MSY_plgc_eat_sndl[, i] <- as.numeric(data_list_MSY[[i]][1:total_years, plgc_eat_sndl])
      
      data_sum_MSY_sndl[, i] <- as.numeric(data_list_MSY[[i]]$Sandeels[1:total_years])
      
      
      data_list_half_MSY_dmrsl_eat_sndl[[i]] <- data_list_half_MSY[[i]][,dmrsl_eat_sndl]
      data_array_half_MSY_dmrsl_eat_sndl[, , i] <- matrix(as.numeric(as.matrix(data_list_half_MSY_dmrsl_eat_sndl[[i]][1:total_years, ])), total_years, n_dmrsl_eat_sndl)
      data_sum_half_MSY_dmrsl_eat_sndl[, i] <- apply(data_array_half_MSY_dmrsl_eat_sndl[, , i], 1, sum)
      
      data_sum_half_MSY_plgc_eat_sndl[, i] <- as.numeric(data_list_half_MSY[[i]][1:total_years, plgc_eat_sndl])
      
      data_sum_half_MSY_sndl[, i] <- as.numeric(data_list_half_MSY[[i]]$Sandeels[1:total_years])
      
      
      data_list_no_F_dmrsl_eat_sndl[[i]] <- data_list_no_F[[i]][,dmrsl_eat_sndl]
      data_array_no_F_dmrsl_eat_sndl[, , i] <- matrix(as.numeric(as.matrix(data_list_no_F_dmrsl_eat_sndl[[i]][1:total_years, ])), total_years, n_dmrsl_eat_sndl)
      data_sum_no_F_dmrsl_eat_sndl[, i] <- apply(data_array_no_F_dmrsl_eat_sndl[, , i], 1, sum)
      
      data_sum_no_F_plgc_eat_sndl[, i] <- as.numeric(data_list_no_F[[i]][1:total_years, plgc_eat_sndl])
      
      data_sum_no_F_sndl[, i] <- as.numeric(data_list_no_F[[i]]$Sandeels[1:total_years])
      
    }
    
    data_mean_MSY_dmrsl_eat_sndl <- apply(data_sum_MSY_dmrsl_eat_sndl, 1, mean)
    data_quantiles_MSY_dmrsl_eat_sndl <- apply(data_sum_MSY_dmrsl_eat_sndl, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_MSY_plgc_eat_sndl <- apply(data_sum_MSY_plgc_eat_sndl, 1, mean)
    data_quantiles_MSY_plgc_eat_sndl <- apply(data_sum_MSY_plgc_eat_sndl, 1, quantile, probs = c(0.025,0.975))
  
    data_mean_MSY_sndl <- apply(data_sum_MSY_sndl, 1, mean)
    data_quantiles_MSY_sndl <- apply(data_sum_MSY_sndl, 1, quantile, probs = c(0.025,0.975))
    
    
    data_mean_half_MSY_dmrsl_eat_sndl <- apply(data_sum_half_MSY_dmrsl_eat_sndl, 1, mean)
    data_quantiles_half_MSY_dmrsl_eat_sndl <- apply(data_sum_half_MSY_dmrsl_eat_sndl, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_half_MSY_plgc_eat_sndl <- apply(data_sum_half_MSY_plgc_eat_sndl, 1, mean)
    data_quantiles_half_MSY_plgc_eat_sndl <- apply(data_sum_half_MSY_plgc_eat_sndl, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_half_MSY_sndl <- apply(data_sum_half_MSY_sndl, 1, mean)
    data_quantiles_half_MSY_sndl <- apply(data_sum_half_MSY_sndl, 1, quantile, probs = c(0.025,0.975))
    
    
    data_mean_no_F_dmrsl_eat_sndl <- apply(data_sum_no_F_dmrsl_eat_sndl, 1, mean)
    data_quantiles_no_F_dmrsl_eat_sndl <- apply(data_sum_no_F_dmrsl_eat_sndl, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_no_F_plgc_eat_sndl <- apply(data_sum_no_F_plgc_eat_sndl, 1, mean)
    data_quantiles_no_F_plgc_eat_sndl <- apply(data_sum_no_F_plgc_eat_sndl, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_no_F_sndl <- apply(data_sum_no_F_sndl, 1, mean)
    data_quantiles_no_F_sndl <- apply(data_sum_no_F_sndl, 1, quantile, probs = c(0.025,0.975))
    
    # First plot, Figure 7.3 --- baseline vs no sandeel fishing
    
    plot_min_MSY <- min(data_quantiles_MSY_dmrsl_eat_sndl[1, project_years], data_quantiles_MSY_plgc_eat_sndl[1, project_years],
                    data_quantiles_MSY_sndl[1, project_years])
    plot_max_MSY <- max(data_quantiles_MSY_dmrsl_eat_sndl[2, project_years], data_quantiles_MSY_plgc_eat_sndl[2, project_years],
                    data_quantiles_MSY_sndl[2, project_years])
    
    plot_min_no_F <- min(data_quantiles_no_F_dmrsl_eat_sndl[1, project_years], data_quantiles_no_F_plgc_eat_sndl[1, project_years],
                        data_quantiles_no_F_sndl[1, project_years])
    plot_max_no_F <- max(data_quantiles_no_F_dmrsl_eat_sndl[2, project_years], data_quantiles_no_F_plgc_eat_sndl[2, project_years],
                        data_quantiles_no_F_sndl[2, project_years])
    
    options(scipen=10000)
    plot(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
         data_mean_MSY_sndl[project_years], type = "l",
         ylab = output, xlab = "Project Year",
         ylim = c(min(plot_min_MSY, plot_min_no_F), max(plot_max_MSY, plot_max_no_F)),
         col = "blue", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_sndl[1,project_years], lty = 2, col = "blue", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_sndl[2,project_years], lty = 2, col = "blue", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_mean_MSY_plgc_eat_sndl[project_years], lty = 1, col = "red", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_plgc_eat_sndl[1,project_years], lty = 2, col = "red", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_plgc_eat_sndl[2,project_years], lty = 2, col = "red", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_mean_MSY_dmrsl_eat_sndl[project_years], lty = 1, col = "green", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_dmrsl_eat_sndl[1,project_years], lty = 2, col = "green", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_dmrsl_eat_sndl[2,project_years], lty = 2, col = "green", lwd = 2)
    
    lines(as.numeric(data_list_no_F[[1]]$year[project_years]) - 2018,
          data_mean_no_F_sndl[project_years], lty = 3, col = "blue", lwd = 2)
    lines(as.numeric(data_list_no_F[[1]]$year[project_years]) - 2018,
          data_quantiles_no_F_sndl[1,project_years], lty = 4, col = "blue", lwd = 2)
    lines(as.numeric(data_list_no_F[[1]]$year[project_years]) - 2018,
          data_quantiles_no_F_sndl[2,project_years], lty = 4, col = "blue", lwd = 2)
    lines(as.numeric(data_list_no_F[[1]]$year[project_years]) - 2018,
          data_mean_no_F_plgc_eat_sndl[project_years], lty = 3, col = "red", lwd = 2)
    lines(as.numeric(data_list_no_F[[1]]$year[project_years]) - 2018,
          data_quantiles_no_F_plgc_eat_sndl[1,project_years], lty = 4, col = "red", lwd = 2)
    lines(as.numeric(data_list_no_F[[1]]$year[project_years]) - 2018,
          data_quantiles_no_F_plgc_eat_sndl[2,project_years], lty = 4, col = "red", lwd = 2)
    lines(as.numeric(data_list_no_F[[1]]$year[project_years]) - 2018,
          data_mean_no_F_dmrsl_eat_sndl[project_years], lty = 3, col = "green", lwd = 2)
    lines(as.numeric(data_list_no_F[[1]]$year[project_years]) - 2018,
          data_quantiles_no_F_dmrsl_eat_sndl[1,project_years], lty = 4, col = "green", lwd = 2)
    lines(as.numeric(data_list_no_F[[1]]$year[project_years]) - 2018,
          data_quantiles_no_F_dmrsl_eat_sndl[2,project_years], lty = 4, col = "green", lwd = 2)
    
    # Second plot, Figure 7.5 --- baseline vs reduced sandeel fishing
    
    plot_min_half_MSY <- min(data_quantiles_half_MSY_dmrsl_eat_sndl[1, project_years], data_quantiles_half_MSY_plgc_eat_sndl[1, project_years],
                         data_quantiles_half_MSY_sndl[1, project_years])
    plot_max_half_MSY <- max(data_quantiles_half_MSY_dmrsl_eat_sndl[2, project_years], data_quantiles_half_MSY_plgc_eat_sndl[2, project_years],
                         data_quantiles_half_MSY_sndl[2, project_years])
    
    options(scipen=10000)
    plot(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
         data_mean_MSY_sndl[project_years], type = "l",
         ylab = output, xlab = "Project Year",
         ylim = c(min(plot_min_MSY, plot_min_half_MSY), max(plot_max_MSY, plot_max_half_MSY)),
         col = "blue", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_sndl[1,project_years], lty = 2, col = "blue", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_sndl[2,project_years], lty = 2, col = "blue", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_mean_MSY_plgc_eat_sndl[project_years], lty = 1, col = "red", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_plgc_eat_sndl[1,project_years], lty = 2, col = "red", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_plgc_eat_sndl[2,project_years], lty = 2, col = "red", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_mean_MSY_dmrsl_eat_sndl[project_years], lty = 1, col = "green", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_dmrsl_eat_sndl[1,project_years], lty = 2, col = "green", lwd = 2)
    lines(as.numeric(data_list_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_MSY_dmrsl_eat_sndl[2,project_years], lty = 2, col = "green", lwd = 2)
    
    lines(as.numeric(data_list_half_MSY[[1]]$year[project_years]) - 2018,
          data_mean_half_MSY_sndl[project_years], lty = 3, col = "blue", lwd = 2)
    lines(as.numeric(data_list_half_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_half_MSY_sndl[1,project_years], lty = 4, col = "blue", lwd = 2)
    lines(as.numeric(data_list_half_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_half_MSY_sndl[2,project_years], lty = 4, col = "blue", lwd = 2)
    lines(as.numeric(data_list_half_MSY[[1]]$year[project_years]) - 2018,
          data_mean_half_MSY_plgc_eat_sndl[project_years], lty = 3, col = "red", lwd = 2)
    lines(as.numeric(data_list_half_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_half_MSY_plgc_eat_sndl[1,project_years], lty = 4, col = "red", lwd = 2)
    lines(as.numeric(data_list_half_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_half_MSY_plgc_eat_sndl[2,project_years], lty = 4, col = "red", lwd = 2)
    lines(as.numeric(data_list_half_MSY[[1]]$year[project_years]) - 2018,
          data_mean_half_MSY_dmrsl_eat_sndl[project_years], lty = 3, col = "green", lwd = 2)
    lines(as.numeric(data_list_half_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_half_MSY_dmrsl_eat_sndl[1,project_years], lty = 4, col = "green", lwd = 2)
    lines(as.numeric(data_list_half_MSY[[1]]$year[project_years]) - 2018,
          data_quantiles_half_MSY_dmrsl_eat_sndl[2,project_years], lty = 4, col = "green", lwd = 2)
    
    
  }
  
  if(sum(groups == "seabirds") == 1){
    
    data_sum_MSY_sf_sbrds <- matrix(NA, total_years, n_iter)
    data_sum_MSY_dvng_sbrds <- matrix(NA, total_years, n_iter)
    
    data_sum_half_MSY_sf_sbrds <- matrix(NA, total_years, n_iter)
    data_sum_half_MSY_dvng_sbrds <- matrix(NA, total_years, n_iter)
    
    data_sum_no_F_sf_sbrds <- matrix(NA, total_years, n_iter)
    data_sum_no_F_dvng_sbrds <- matrix(NA, total_years, n_iter)
    
    for(i in 1:n_iter){
      
      data_sum_MSY_sf_sbrds[, i] <- as.numeric(data_list_MSY[[i]]$`Surface-feeding seabirds`[1:total_years])
      data_sum_MSY_dvng_sbrds[, i] <- as.numeric(data_list_MSY[[i]]$`Diving seabirds`[1:total_years])
      
      data_sum_half_MSY_sf_sbrds[, i] <- as.numeric(data_list_half_MSY[[i]]$`Surface-feeding seabirds`[1:total_years])
      data_sum_half_MSY_dvng_sbrds[, i] <- as.numeric(data_list_half_MSY[[i]]$`Diving seabirds`[1:total_years])
      
      data_sum_no_F_sf_sbrds[, i] <- as.numeric(data_list_no_F[[i]]$`Surface-feeding seabirds`[1:total_years])
      data_sum_no_F_dvng_sbrds[, i] <- as.numeric(data_list_no_F[[i]]$`Diving seabirds`[1:total_years])
      
    }
    
    data_mean_MSY_sf_sbrds <- apply(data_sum_MSY_sf_sbrds, 1, mean)
    data_quantiles_MSY_sf_sbrds <- apply(data_sum_MSY_sf_sbrds, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_MSY_dvng_sbrds <- apply(data_sum_MSY_dvng_sbrds, 1, mean)
    data_quantiles_MSY_dvng_sbrds <- apply(data_sum_MSY_dvng_sbrds, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_half_MSY_sf_sbrds <- apply(data_sum_half_MSY_sf_sbrds, 1, mean)
    data_quantiles_half_MSY_sf_sbrds <- apply(data_sum_half_MSY_sf_sbrds, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_half_MSY_dvng_sbrds <- apply(data_sum_half_MSY_dvng_sbrds, 1, mean)
    data_quantiles_half_MSY_dvng_sbrds <- apply(data_sum_half_MSY_dvng_sbrds, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_no_F_sf_sbrds <- apply(data_sum_no_F_sf_sbrds, 1, mean)
    data_quantiles_no_F_sf_sbrds <- apply(data_sum_no_F_sf_sbrds, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_no_F_dvng_sbrds <- apply(data_sum_no_F_dvng_sbrds, 1, mean)
    data_quantiles_no_F_dvng_sbrds <- apply(data_sum_no_F_dvng_sbrds, 1, quantile, probs = c(0.025,0.975))
    
    # First plot, Figure 7.6a) surface-feeding seabirds all scenarios
    
    plot_min_sf_sbrds <- min(data_quantiles_MSY_sf_sbrds[1, project_years], data_quantiles_half_MSY_sf_sbrds[1, project_years],
                             data_quantiles_no_F_sf_sbrds[1, project_years])
    
    plot_max_sf_sbrds <- max(data_quantiles_MSY_sf_sbrds[2, project_years], data_quantiles_half_MSY_sf_sbrds[2, project_years],
                             data_quantiles_no_F_sf_sbrds[2, project_years])
    
    options(scipen=10000)
    plot((as.numeric(data_list_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
         data_mean_MSY_sf_sbrds[project_years], type = "l",
         ylab = paste0(output, ", surface-feeding seabirds"), xlab = "Project Year",
         ylim = c(plot_min_sf_sbrds, plot_max_sf_sbrds),
         col = "darkgreen", lwd = 2)
    lines((as.numeric(data_list_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_MSY_sf_sbrds[1,project_years], lty = 2, col = "darkgreen", lwd = 2)
    lines((as.numeric(data_list_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_MSY_sf_sbrds[2,project_years], lty = 2, col = "darkgreen", lwd = 2)
    
    lines((as.numeric(data_list_half_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_mean_half_MSY_sf_sbrds[project_years], lty = 1, col = "purple", lwd = 2)
    lines((as.numeric(data_list_half_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_half_MSY_sf_sbrds[1,project_years], lty = 2, col = "purple", lwd = 2)
    lines((as.numeric(data_list_half_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_half_MSY_sf_sbrds[2,project_years], lty = 2, col = "purple", lwd = 2)
    
    lines((as.numeric(data_list_no_F[[1]]$year[1:total_years]) - 2018)[project_years],
          data_mean_no_F_sf_sbrds[project_years], lty = 1, col = "deepskyblue4", lwd = 2)
    lines((as.numeric(data_list_no_F[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_no_F_sf_sbrds[1,project_years], lty = 2, col = "deepskyblue4", lwd = 2)
    lines((as.numeric(data_list_no_F[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_no_F_sf_sbrds[2,project_years], lty = 2, col = "deepskyblue4", lwd = 2)
    
    # Second plot, Figure 7.6b) diving seabirds all scenarios
    
    plot_min_dvng_sbrds <- min(data_quantiles_MSY_dvng_sbrds[1, project_years], data_quantiles_half_MSY_dvng_sbrds[1, project_years],
                             data_quantiles_no_F_dvng_sbrds[1, project_years])
    
    plot_max_dvng_sbrds <- max(data_quantiles_MSY_dvng_sbrds[2, project_years], data_quantiles_half_MSY_dvng_sbrds[2, project_years],
                             data_quantiles_no_F_dvng_sbrds[2, project_years])
    
    options(scipen=10000)
    plot((as.numeric(data_list_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
         data_mean_MSY_dvng_sbrds[project_years], type = "l",
         ylab = paste0(output, ", diving seabirds"), xlab = "Project Year",
         ylim = c(plot_min_dvng_sbrds, plot_max_dvng_sbrds),
         col = "darkgreen", lwd = 2)
    lines((as.numeric(data_list_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_MSY_dvng_sbrds[1,project_years], lty = 2, col = "darkgreen", lwd = 2)
    lines((as.numeric(data_list_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_MSY_dvng_sbrds[2,project_years], lty = 2, col = "darkgreen", lwd = 2)
    
    lines((as.numeric(data_list_half_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_mean_half_MSY_dvng_sbrds[project_years], lty = 1, col = "purple", lwd = 2)
    lines((as.numeric(data_list_half_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_half_MSY_dvng_sbrds[1,project_years], lty = 2, col = "purple", lwd = 2)
    lines((as.numeric(data_list_half_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_half_MSY_dvng_sbrds[2,project_years], lty = 2, col = "purple", lwd = 2)
    
    lines((as.numeric(data_list_no_F[[1]]$year[1:total_years]) - 2018)[project_years],
          data_mean_no_F_dvng_sbrds[project_years], lty = 1, col = "deepskyblue4", lwd = 2)
    lines((as.numeric(data_list_no_F[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_no_F_dvng_sbrds[1,project_years], lty = 2, col = "deepskyblue4", lwd = 2)
    lines((as.numeric(data_list_no_F[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_no_F_dvng_sbrds[2,project_years], lty = 2, col = "deepskyblue4", lwd = 2)
    
    
  }
  
  if(sum(groups == "whales") == 1){
    
    data_sum_MSY_bln_whls <- matrix(NA, total_years, n_iter)
    data_sum_MSY_tthd_whls <- matrix(NA, total_years, n_iter)
    
    data_sum_half_MSY_bln_whls <- matrix(NA, total_years, n_iter)
    data_sum_half_MSY_tthd_whls <- matrix(NA, total_years, n_iter)
    
    data_sum_no_F_bln_whls <- matrix(NA, total_years, n_iter)
    data_sum_no_F_tthd_whls <- matrix(NA, total_years, n_iter)
    
    for(i in 1:n_iter){
      
      data_sum_MSY_bln_whls[, i] <- as.numeric(data_list_MSY[[i]]$`Baleen whales`[1:total_years])
      data_sum_MSY_tthd_whls[, i] <- as.numeric(data_list_MSY[[i]]$`Toothed whales`[1:total_years])
      
      data_sum_half_MSY_bln_whls[, i] <- as.numeric(data_list_half_MSY[[i]]$`Baleen whales`[1:total_years])
      data_sum_half_MSY_tthd_whls[, i] <- as.numeric(data_list_half_MSY[[i]]$`Toothed whales`[1:total_years])
      
      data_sum_no_F_bln_whls[, i] <- as.numeric(data_list_no_F[[i]]$`Baleen whales`[1:total_years])
      data_sum_no_F_tthd_whls[, i] <- as.numeric(data_list_no_F[[i]]$`Toothed whales`[1:total_years])
      
    }
    
    data_mean_MSY_bln_whls <- apply(data_sum_MSY_bln_whls, 1, mean)
    data_quantiles_MSY_bln_whls <- apply(data_sum_MSY_bln_whls, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_MSY_tthd_whls <- apply(data_sum_MSY_tthd_whls, 1, mean)
    data_quantiles_MSY_tthd_whls <- apply(data_sum_MSY_tthd_whls, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_half_MSY_bln_whls <- apply(data_sum_half_MSY_bln_whls, 1, mean)
    data_quantiles_half_MSY_bln_whls <- apply(data_sum_half_MSY_bln_whls, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_half_MSY_tthd_whls <- apply(data_sum_half_MSY_tthd_whls, 1, mean)
    data_quantiles_half_MSY_tthd_whls <- apply(data_sum_half_MSY_tthd_whls, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_no_F_bln_whls <- apply(data_sum_no_F_bln_whls, 1, mean)
    data_quantiles_no_F_bln_whls <- apply(data_sum_no_F_bln_whls, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_no_F_tthd_whls <- apply(data_sum_no_F_tthd_whls, 1, mean)
    data_quantiles_no_F_tthd_whls <- apply(data_sum_no_F_tthd_whls, 1, quantile, probs = c(0.025,0.975))
    
    # First plot, Figure 7.6c) --- baleen whales, all scenarios
    
    plot_min_bln_whls <- min(data_quantiles_MSY_bln_whls[1, project_years], data_quantiles_half_MSY_bln_whls[1, project_years],
                               data_quantiles_no_F_bln_whls[1, project_years])
    
    plot_min_tthd_whls <- min(data_quantiles_MSY_tthd_whls[1, project_years], data_quantiles_half_MSY_tthd_whls[1, project_years],
                               data_quantiles_no_F_tthd_whls[1, project_years])
    
    plot_max_bln_whls <- max(data_quantiles_MSY_bln_whls[2, project_years], data_quantiles_half_MSY_bln_whls[2, project_years],
                             data_quantiles_no_F_bln_whls[2, project_years])
    
    plot_max_tthd_whls <- max(data_quantiles_MSY_tthd_whls[2, project_years], data_quantiles_half_MSY_tthd_whls[2, project_years],
                              data_quantiles_no_F_tthd_whls[2, project_years])
    
    options(scipen=10000)
    plot((as.numeric(data_list_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
         data_mean_MSY_bln_whls[project_years], type = "l",
         ylab = paste0(output, ", baleen whales"), xlab = "Project Year",
         ylim = c(plot_min_bln_whls, plot_max_bln_whls),
         col = "darkgreen", lwd = 2)
    lines((as.numeric(data_list_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_MSY_bln_whls[1,project_years], lty = 2, col = "darkgreen", lwd = 2)
    lines((as.numeric(data_list_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_MSY_bln_whls[2,project_years], lty = 2, col = "darkgreen", lwd = 2)
    
    lines((as.numeric(data_list_half_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_mean_half_MSY_bln_whls[project_years], lty = 1, col = "purple", lwd = 2)
    lines((as.numeric(data_list_half_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_half_MSY_bln_whls[1,project_years], lty = 2, col = "purple", lwd = 2)
    lines((as.numeric(data_list_half_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_half_MSY_bln_whls[2,project_years], lty = 2, col = "purple", lwd = 2)
    
    lines((as.numeric(data_list_no_F[[1]]$year[1:total_years]) - 2018)[project_years],
          data_mean_no_F_bln_whls[project_years], lty = 1, col = "deepskyblue4", lwd = 2)
    lines((as.numeric(data_list_no_F[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_no_F_bln_whls[1,project_years], lty = 2, col = "deepskyblue4", lwd = 2)
    lines((as.numeric(data_list_no_F[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_no_F_bln_whls[2,project_years], lty = 2, col = "deepskyblue4", lwd = 2)
    
    
    plot((as.numeric(data_list_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
         data_mean_MSY_tthd_whls[project_years], type = "l",
         ylab = paste0(output, ", toothed whales"), xlab = "Project Year",
         ylim = c(plot_min_tthd_whls, plot_max_tthd_whls),
         col = "darkgreen", lwd = 2)
    lines((as.numeric(data_list_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_MSY_tthd_whls[1,project_years], lty = 2, col = "darkgreen", lwd = 2)
    lines((as.numeric(data_list_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_MSY_tthd_whls[2,project_years], lty = 2, col = "darkgreen", lwd = 2)
    
    lines((as.numeric(data_list_half_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_mean_half_MSY_tthd_whls[project_years], lty = 1, col = "purple", lwd = 2)
    lines((as.numeric(data_list_half_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_half_MSY_tthd_whls[1,project_years], lty = 2, col = "purple", lwd = 2)
    lines((as.numeric(data_list_half_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_half_MSY_tthd_whls[2,project_years], lty = 2, col = "purple", lwd = 2)
    
    lines((as.numeric(data_list_no_F[[1]]$year[1:total_years]) - 2018)[project_years],
          data_mean_no_F_tthd_whls[project_years], lty = 1, col = "deepskyblue4", lwd = 2)
    lines((as.numeric(data_list_no_F[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_no_F_tthd_whls[1,project_years], lty = 2, col = "deepskyblue4", lwd = 2)
    lines((as.numeric(data_list_no_F[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_no_F_tthd_whls[2,project_years], lty = 2, col = "deepskyblue4", lwd = 2)
    
    
  }
  
  if(sum(groups == "seals") == 1){
    
    data_sum_MSY_seals <- matrix(NA, total_years, n_iter)
    data_sum_half_MSY_seals <- matrix(NA, total_years, n_iter)
    data_sum_no_F_seals <- matrix(NA, total_years, n_iter)
    
    for(i in 1:n_iter){
      
      data_sum_MSY_seals[, i] <- as.numeric(data_list_MSY[[i]]$Seals[1:total_years])
      data_sum_half_MSY_seals[, i] <- as.numeric(data_list_half_MSY[[i]]$Seals[1:total_years])
      data_sum_no_F_seals[, i] <- as.numeric(data_list_no_F[[i]]$Seals[1:total_years])
      
    }
    
    data_mean_MSY_seals <- apply(data_sum_MSY_seals, 1, mean)
    data_quantiles_MSY_seals <- apply(data_sum_MSY_seals, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_half_MSY_seals <- apply(data_sum_half_MSY_seals, 1, mean)
    data_quantiles_half_MSY_seals <- apply(data_sum_half_MSY_seals, 1, quantile, probs = c(0.025,0.975))
    
    data_mean_no_F_seals <- apply(data_sum_no_F_seals, 1, mean)
    data_quantiles_no_F_seals <- apply(data_sum_no_F_seals, 1, quantile, probs = c(0.025,0.975))
    
    # FIgure 7.6e), Seals, all scenarios

    plot_min_seals <- min(data_quantiles_MSY_seals[1, project_years], data_quantiles_half_MSY_seals[1, project_years],
                             data_quantiles_no_F_seals[1, project_years])
    plot_max_seals <- max(data_quantiles_MSY_seals[2, project_years], data_quantiles_half_MSY_seals[2, project_years],
                          data_quantiles_no_F_seals[2, project_years])
    
    options(scipen=10000)
    plot((as.numeric(data_list_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
         data_mean_MSY_seals[project_years], type = "l",
         ylab = paste0(output, ", seals"), xlab = "Project Year",
         ylim = c(plot_min_seals, plot_max_seals),
         col = "darkgreen", lwd = 2)
    lines((as.numeric(data_list_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_MSY_seals[1,project_years], lty = 2, col = "darkgreen", lwd = 2)
    lines((as.numeric(data_list_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_MSY_seals[2,project_years], lty = 2, col = "darkgreen", lwd = 2)
    
    lines((as.numeric(data_list_half_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_mean_half_MSY_seals[project_years], lty = 1, col = "purple", lwd = 2)
    lines((as.numeric(data_list_half_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_half_MSY_seals[1,project_years], lty = 2, col = "purple", lwd = 2)
    lines((as.numeric(data_list_half_MSY[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_half_MSY_seals[2,project_years], lty = 2, col = "purple", lwd = 2)
    
    lines((as.numeric(data_list_no_F[[1]]$year[1:total_years]) - 2018)[project_years],
          data_mean_no_F_seals[project_years], lty = 1, col = "deepskyblue4", lwd = 2)
    lines((as.numeric(data_list_no_F[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_no_F_seals[1,project_years], lty = 2, col = "deepskyblue4", lwd = 2)
    lines((as.numeric(data_list_no_F[[1]]$year[1:total_years]) - 2018)[project_years],
          data_quantiles_no_F_seals[2,project_years], lty = 2, col = "deepskyblue4", lwd = 2)
  }
  
}


North_Sea_EwE_plot(data_list_MSY = biomass_annual_test_3_scenarios$data_list_MSY,
                   data_list_half_MSY = biomass_annual_test_3_scenarios$data_list_half_MSY,
                   data_list_no_F = biomass_annual_test_3_scenarios$data_list_no_F,
                   groups = "fish_groups", n_iter = 40,
                   total_years = 110, project_years = 28:87, output = "biomass_annual")
North_Sea_EwE_plot(data_list = biomass_annual_test$data_list, groups = "sandeel_diet", n_iter = 20,
                   total_years = 110, project_years = 28:87, output = "biomass_annual")
North_Sea_EwE_plot(data_list = biomass_annual_test$data_list, groups = "seals", n_iter = 20,
                   total_years = 110, project_years = 28:87, output = "biomass_annual")


North_Sea_EwE_CV <- function(data_list, n_iter = 20, groups = c("fish_groups", "sandeel_diet", "seabirds",
                                                           "whales", "seals"),  total_years = 110,
                             project_years = 28:87, n_years = 10){
  
  last_n_years <- (tail(project_years, n = 1) - (n_years - 1)):tail(project_years, n = 1)
  CV <- vector("list", length(groups))
  # taking an average of the final n years, in case there is too much fluctuation 
  # (i.e., the time-series hasn't settled to a steady state)
  
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
    
    data_last_n_years_mean_sndl_each_iteration <- apply(data_sum_sndl[last_n_years,], 2, mean)
    data_last_n_years_mean_sndl_over_all_iteration <- mean(data_last_n_years_mean_sndl_each_iteration)
    data_last_n_years_sd_sndl_over_all_iteration <- sd(data_last_n_years_mean_sndl_each_iteration)
    data_last_n_years_CV_sndl_over_all_iteration <- data_last_n_years_sd_sndl_over_all_iteration / data_last_n_years_mean_sndl_over_all_iteration
    
    data_last_n_years_mean_dmrsl_each_iteration <- apply(data_sum_dmrsl[last_n_years,], 2, mean)
    data_last_n_years_mean_dmrsl_over_all_iteration <- mean(data_last_n_years_mean_dmrsl_each_iteration)
    data_last_n_years_sd_dmrsl_over_all_iteration <- sd(data_last_n_years_mean_dmrsl_each_iteration)
    data_last_n_years_CV_dmrsl_over_all_iteration <- data_last_n_years_sd_dmrsl_over_all_iteration / data_last_n_years_mean_dmrsl_over_all_iteration
    
    data_last_n_years_mean_plgc_each_iteration <- apply(data_sum_plgc[last_n_years,], 2, mean)
    data_last_n_years_mean_plgc_over_all_iteration <- mean(data_last_n_years_mean_plgc_each_iteration)
    data_last_n_years_sd_plgc_over_all_iteration <- sd(data_last_n_years_mean_plgc_each_iteration)
    data_last_n_years_CV_plgc_over_all_iteration <- data_last_n_years_sd_plgc_over_all_iteration / data_last_n_years_mean_plgc_over_all_iteration
    
    data_last_n_years_mean_shllfsh_each_iteration <- apply(data_sum_shllfsh[last_n_years,], 2, mean)
    data_last_n_years_mean_shllfsh_over_all_iteration <- mean(data_last_n_years_mean_shllfsh_each_iteration)
    data_last_n_years_sd_shllfsh_over_all_iteration <- sd(data_last_n_years_mean_shllfsh_each_iteration)
    data_last_n_years_CV_shllfsh_over_all_iteration <- data_last_n_years_sd_shllfsh_over_all_iteration / data_last_n_years_mean_shllfsh_over_all_iteration
    
    CV_fish_groups <- data.frame(c(data_last_n_years_CV_sndl_over_all_iteration, data_last_n_years_CV_dmrsl_over_all_iteration,
            data_last_n_years_CV_plgc_over_all_iteration, data_last_n_years_CV_shllfsh_over_all_iteration))
    
    rownames(CV_fish_groups) <- c("Sandeels", "Demersal", "Pelagic", "Shellfish")
    colnames(CV_fish_groups) <- "Coefficient of variation"
    CV[[1]] <- CV_fish_groups
    
  }
  
  if(sum(groups == "sandeel_diet") == 1){
    
    dmrsl_eat_sndl <- which(data_list[[1]][(total_years + 1), ] == "demersal" & data_list[[1]][(total_years + 2), ] == T)
    n_dmrsl_eat_sndl <- length(dmrsl_eat_sndl)
    plgc_eat_sndl <- which(data_list[[1]][(total_years + 1), ] == "pelagic" & data_list[[1]][(total_years + 2), ] == T)
    n_plgc_eat_sndl <- length(plgc_eat_sndl)
    
    data_list_dmrsl_eat_sndl <- vector("list", n_iter)
    data_array_dmrsl_eat_sndl <- array(NA, dim = c(total_years, n_dmrsl_eat_sndl, n_iter)) # some issues with lists and character strings, so this is a workaround
    data_sum_dmrsl_eat_sndl <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations
  
    data_sum_plgc_eat_sndl <- matrix(NA, total_years, n_iter) # 110 years, 20 iterationsbase_model_all_runs_biomass_annual_shellfish_v1 <- vector("list", n_iter)
  
    data_sum_sndl <- matrix(NA, total_years, n_iter) # 110 years, 20 iterations (only one sandeel functional group)
    
    for(i in 1:n_iter){
      
      data_list_dmrsl_eat_sndl[[i]] <- data_list[[i]][,dmrsl_eat_sndl]
      data_array_dmrsl_eat_sndl[, , i] <- matrix(as.numeric(as.matrix(data_list_dmrsl_eat_sndl[[i]][1:total_years, ])), total_years, n_dmrsl_eat_sndl)
      data_sum_dmrsl_eat_sndl[, i] <- apply(data_array_dmrsl_eat_sndl[, , i], 1, sum)
      
      data_sum_plgc_eat_sndl[, i] <- as.numeric(data_list[[i]][1:total_years, plgc_eat_sndl])
      
    }
    
    data_last_n_years_mean_dmrsl_eat_sndl_each_iteration <- apply(data_sum_dmrsl_eat_sndl[last_n_years,], 2, mean)
    data_last_n_years_mean_dmrsl_eat_sndl_over_all_iteration <- mean(data_last_n_years_mean_dmrsl_eat_sndl_each_iteration)
    data_last_n_years_sd_dmrsl_eat_sndl_over_all_iteration <- sd(data_last_n_years_mean_dmrsl_eat_sndl_each_iteration)
    data_last_n_years_CV_dmrsl_eat_sndl_over_all_iteration <- data_last_n_years_sd_dmrsl_eat_sndl_over_all_iteration / data_last_n_years_mean_dmrsl_eat_sndl_over_all_iteration
    
    data_last_n_years_mean_plgc_eat_sndl_each_iteration <- apply(data_sum_plgc_eat_sndl[last_n_years,], 2, mean)
    data_last_n_years_mean_plgc_eat_sndl_over_all_iteration <- mean(data_last_n_years_mean_plgc_eat_sndl_each_iteration)
    data_last_n_years_sd_plgc_eat_sndl_over_all_iteration <- sd(data_last_n_years_mean_plgc_eat_sndl_each_iteration)
    data_last_n_years_CV_plgc_eat_sndl_over_all_iteration <- data_last_n_years_sd_plgc_eat_sndl_over_all_iteration / data_last_n_years_mean_plgc_eat_sndl_over_all_iteration
    
    CV_sndl_diet <- data.frame(c(data_last_n_years_CV_dmrsl_eat_sndl_over_all_iteration,
                               data_last_n_years_CV_plgc_eat_sndl_over_all_iteration))
    
    rownames(CV_sndl_diet) <- c("Demersal fish w/ sandeel diet", "Pelagic fish w/ sandeel diet")
    colnames(CV_sndl_diet) <- "Coefficient of variation"
    
    if(sum(groups == "fish_groups") == 1){
      
      CV[[2]] <- CV_sndl_diet
      
    }else{
      
      CV[[1]] <- CV_sndl_diet
      
    }
    
  }
  
  if(sum(groups == "seabirds") == 1){
    
    data_sum_sf_sbrds <- matrix(NA, total_years, n_iter)
    data_sum_dvng_sbrds <- matrix(NA, total_years, n_iter)
    
    for(i in 1:n_iter){
      
      data_sum_sf_sbrds[, i] <- as.numeric(data_list[[i]]$`Surface-feeding seabirds`[1:total_years])
      data_sum_dvng_sbrds[, i] <- as.numeric(data_list[[i]]$`Diving seabirds`[1:total_years])
      
    }
    
    data_last_n_years_mean_sf_sbrds_each_iteration <- apply(data_sum_sf_sbrds[last_n_years,], 2, mean)
    data_last_n_years_mean_sf_sbrds_over_all_iteration <- mean(data_last_n_years_mean_sf_sbrds_each_iteration)
    data_last_n_years_sd_sf_sbrds_over_all_iteration <- sd(data_last_n_years_mean_sf_sbrds_each_iteration)
    data_last_n_years_CV_sf_sbrds_over_all_iteration <- data_last_n_years_sd_sf_sbrds_over_all_iteration / data_last_n_years_mean_sf_sbrds_over_all_iteration
    
    data_last_n_years_mean_dvng_sbrds_each_iteration <- apply(data_sum_dvng_sbrds[last_n_years,], 2, mean)
    data_last_n_years_mean_dvng_sbrds_over_all_iteration <- mean(data_last_n_years_mean_dvng_sbrds_each_iteration)
    data_last_n_years_sd_dvng_sbrds_over_all_iteration <- sd(data_last_n_years_mean_dvng_sbrds_each_iteration)
    data_last_n_years_CV_dvng_sbrds_over_all_iteration <- data_last_n_years_sd_dvng_sbrds_over_all_iteration / data_last_n_years_mean_dvng_sbrds_over_all_iteration
    
    CV_sbrds <- data.frame(c(data_last_n_years_CV_sf_sbrds_over_all_iteration,
                           data_last_n_years_CV_dvng_sbrds_over_all_iteration))
    
    rownames(CV_sbrds) <- c("Surface-feeding seabirds", "Diving seabirds")
    colnames(CV_sbrds) <- "Coefficient of variation"
    
    if(sum(groups == "fish_groups") == 1 & sum(groups == "sandeel_diet") == 1){
        CV[[3]] <- CV_sbrds
    }else{
      if(sum(groups == "fish_groups") == 1 | sum(groups == "sandeel_diet") == 1){
        CV[[2]] <- CV_sbrds
      }else{
        CV[[1]] <- CV_sbrds
      }
    }
    
  }
  
  if(sum(groups == "whales") == 1){
    
    data_sum_bln_whls <- matrix(NA, total_years, n_iter)
    data_sum_tthd_whls <- matrix(NA, total_years, n_iter)
    
    for(i in 1:n_iter){
      
      data_sum_bln_whls[, i] <- as.numeric(data_list[[i]]$`Baleen whales`[1:total_years])
      data_sum_tthd_whls[, i] <- as.numeric(data_list[[i]]$`Toothed whales`[1:total_years])
      
    }
    
    data_last_n_years_mean_bln_whls_each_iteration <- apply(data_sum_bln_whls[last_n_years,], 2, mean)
    data_last_n_years_mean_bln_whls_over_all_iteration <- mean(data_last_n_years_mean_bln_whls_each_iteration)
    data_last_n_years_sd_bln_whls_over_all_iteration <- sd(data_last_n_years_mean_bln_whls_each_iteration)
    data_last_n_years_CV_bln_whls_over_all_iteration <- data_last_n_years_sd_bln_whls_over_all_iteration / data_last_n_years_mean_bln_whls_over_all_iteration
    
    data_last_n_years_mean_tthd_whls_each_iteration <- apply(data_sum_tthd_whls[last_n_years,], 2, mean)
    data_last_n_years_mean_tthd_whls_over_all_iteration <- mean(data_last_n_years_mean_tthd_whls_each_iteration)
    data_last_n_years_sd_tthd_whls_over_all_iteration <- sd(data_last_n_years_mean_tthd_whls_each_iteration)
    data_last_n_years_CV_tthd_whls_over_all_iteration <- data_last_n_years_sd_tthd_whls_over_all_iteration / data_last_n_years_mean_tthd_whls_over_all_iteration
    
    CV_whls <- data.frame(c(data_last_n_years_CV_bln_whls_over_all_iteration,
                          data_last_n_years_CV_tthd_whls_over_all_iteration))
    
    rownames(CV_whls) <- c("Baleen whales", "Toothed whales")
    colnames(CV_whls) <- "Coefficient of variation"
    
    if(length(groups) == 1){ 
      CV[[1]] <- CV_whls # 1 group = 1 element in list
    }
    if(sum(groups == "fish_groups") == 1 & sum(groups == "sandeel_diet") == 1 & sum(groups == "seabirds") == 1){
      CV[[4]] <- CV_whls # first 3 groups => whales is 4th
    }else{
      if(sum(groups == "seals") == 1){
        if(length(groups) == 4){
        CV[[3]] <- CV_whls # 4 groups and 1 of them is seals => whales is 3rd
        }
        if(length(groups) == 3){
          CV[[2]] <- CV_whls # 3 groups and 1 of them is seals => whales is 2nd
        }
        if(length(groups) == 2){
          CV[[1]] <- CV_whls # 2 groups and 1 of them is seals => whales is 1st
        }
      }else{
        if(length(groups) == 3){
          CV[[3]] <- CV_whls # 3 groups and no seals => whales is 3rd
        }
        if(length(groups) == 2){
          CV[[2]] <- CV_whls # 2 groups and no seals => whales is 2nd
        }
      }
      }
    
  }
  
  if(sum(groups == "seals") == 1){
    
    data_sum_seals <- matrix(NA, total_years, n_iter)
    
    for(i in 1:n_iter){
      
      data_sum_seals[, i] <- as.numeric(data_list[[i]]$Seals[1:total_years])
      
    }
   
    data_last_n_years_mean_seals_each_iteration <- apply(data_sum_seals[last_n_years,], 2, mean)
    data_last_n_years_mean_seals_over_all_iteration <- mean(data_last_n_years_mean_seals_each_iteration)
    data_last_n_years_sd_seals_over_all_iteration <- sd(data_last_n_years_mean_seals_each_iteration)
    data_last_n_years_CV_seals_over_all_iteration <- data_last_n_years_sd_seals_over_all_iteration / data_last_n_years_mean_seals_over_all_iteration
    
    CV_seals <- data_last_n_years_CV_seals_over_all_iteration
    
    if(length(groups) == 5){
      CV[[5]] <- CV_seals
    }else{
      if(length(groups) == 4){
        CV[[4]] <- CV_seals
      }else{
        if(length(groups) == 3){
          CV[[3]] <- CV_seals
        }else{
          if(length(groups) == 2){
            CV[[2]] <- CV_seals
          }else{
            CV[[1]] <- CV_seals
          }
        }
      }
    }
    
  }
  
  return(CV)
  
}

North_Sea_EwE_CV(data_list = biomass_annual_test$data_list, groups = c("sandeel_diet", "whales"),
                 n_years = 10)
North_Sea_EwE_CV(data_list = biomass_annual_test$data_list, groups = c("fish_groups", "sandeel_diet", "whales",
                                                                       "seals", "seabirds"),
                 n_years = 10)
