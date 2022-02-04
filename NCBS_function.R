# creating a function to be able to produce a natural capital balance sheet for any catch, biomass data from EwE

biomass_annual_test <- North_Sea_EwE_data(output = "biomass_annual", n_iter = 20, total_years = 110,
                                          project_years = 28:87, amalgamate = T)
catch_annual_test <- North_Sea_EwE_data(output = "catch_annual", n_iter = 20, total_years = 110,
                                          project_years = 28:87, amalgamate = T)
colnames(biomass_annual_test$data_list[[2]])[11]
NCBS <- function(data_list_biomass, data_list_catch, n_iter = 20, project_years = 28:87){
  
  # firstly the asset baseline stuff
  
  fish_w_sndl_diet <- which(data_list_biomass[[1]][(total_years + 2), ] == T)
  n_fish_w_sndl_diet <- length(fish_w_sndl_diet)
  other_fish <- which(data_list_biomass[[1]][(total_years + 2), ] == F & (data_list_biomass[[1]][(total_years + 1), ] == "demersal" | data_list_biomass[[1]][(total_years + 1), ] == "pelagic" | data_list_biomass[[1]][(total_years + 1), ] == "shellfish"))
  n_other_fish <- length(other_fish)
  
  sndl_biomass_project_years <- matrix(NA, length(project_years), n_iter)
  
  fish_w_sndl_diet_biomass_project_years <- matrix(NA, length(project_years), n_iter)
  fish_w_sndl_diet_biomass_project_years_list <- vector("list", n_iter)
  fish_w_sndl_diet_biomass_project_years_array <- array(NA, dim = c(length(project_years), n_fish_w_sndl_diet, n_iter)) # some issues with lists and character strings, so this is a workaround
  
  other_fish_biomass_project_years <- matrix(NA, length(project_years), n_iter)
  other_fish_biomass_project_years_list <- vector("list", n_iter)
  other_fish_biomass_project_years_array <- array(NA, dim = c(length(project_years), n_other_fish, n_iter)) # some issues with lists and character strings, so this is a workaround
  
  bln_whls_biomass_project_years <- matrix(NA, length(project_years), n_iter)
  tthd_whls_biomass_project_years <- matrix(NA, length(project_years), n_iter)
  seals_biomass_project_years <- matrix(NA, length(project_years), n_iter)
  sf_sbrds_biomass_project_years <- matrix(NA, length(project_years), n_iter)
  dvng_sbrds_biomass_project_years <- matrix(NA, length(project_years), n_iter)
  
  sndl_catch_project_years <- matrix(NA, length(project_years), n_iter)
  
  fish_w_sndl_diet_catch_project_years <- matrix(NA, length(project_years), n_iter)
  fish_w_sndl_diet_catch_project_years_list <- vector("list", n_iter)
  fish_w_sndl_diet_catch_project_years_array <- array(NA, dim = c(length(project_years), n_fish_w_sndl_diet, n_iter)) # some issues with lists and character strings, so this is a workaround
  
  other_fish_catch_project_years <- matrix(NA, length(project_years), n_iter)
  other_fish_catch_project_years_list <- vector("list", n_iter)
  other_fish_catch_project_years_array <- array(NA, dim = c(length(project_years), n_other_fish, n_iter)) # some issues with lists and character strings, so this is a workaround
  
  for(i in 1:n_iter){
    
    sndl_biomass_project_years[, i] <- as.numeric(data_list_biomass[[i]]$Sandeels[project_years])
    
    fish_w_sndl_diet_biomass_project_years_list[[i]] <- data_list_biomass[[i]][, fish_w_sndl_diet]
    fish_w_sndl_diet_biomass_project_years_array[, , i] <- matrix(as.numeric(as.matrix(fish_w_sndl_diet_biomass_project_years_list[[i]][project_years, ])),
                                                                       length(project_years), n_fish_w_sndl_diet)
    fish_w_sndl_diet_biomass_project_years[, i] <- apply(fish_w_sndl_diet_biomass_project_years_array[, , i], 1, sum)
    
    other_fish_biomass_project_years_list[[i]] <- data_list_biomass[[i]][, other_fish]
    other_fish_biomass_project_years_array[, , i] <- matrix(as.numeric(as.matrix(other_fish_biomass_project_years_list[[i]][project_years, ])),
                                                                      length(project_years), n_other_fish)
    other_fish_biomass_project_years[, i] <- apply(other_fish_biomass_project_years_array[, , i], 1, sum)
    
    bln_whls_biomass_project_years[, i] <- as.numeric(data_list_biomass[[i]]$`Baleen whales`[project_years])
    tthd_whls_biomass_project_years[, i] <- as.numeric(data_list_biomass[[i]]$`Toothed whales`[project_years])
    seals_biomass_project_years[, i] <- as.numeric(data_list_biomass[[i]]$Seals[project_years])
    sf_sbrds_biomass_project_years[, i] <- as.numeric(data_list_biomass[[i]]$`Surface-feeding seabirds`[project_years])
    dvng_sbrds_biomass_project_years[, i] <- as.numeric(data_list_biomass[[i]]$`Diving seabirds`[project_years])
    
    sndl_catch_project_years[, i] <- as.numeric(data_list_catch[[i]]$Sandeels[project_years])
    
    fish_w_sndl_diet_catch_project_years_list[[i]] <- data_list_catch[[i]][, fish_w_sndl_diet]
    fish_w_sndl_diet_catch_project_years_array[, , i] <- matrix(as.numeric(as.matrix(fish_w_sndl_diet_catch_project_years_list[[i]][project_years, ])),
                                                                 length(project_years), n_fish_w_sndl_diet)
    fish_w_sndl_diet_catch_project_years[, i] <- apply(fish_w_sndl_diet_catch_project_years_array[, , i], 1, sum)
    
    other_fish_catch_project_years_list[[i]] <- data_list_catch[[i]][, other_fish]
    other_fish_catch_project_years_array[, , i] <- matrix(as.numeric(as.matrix(other_fish_catch_project_years_list[[i]][project_years, ])),
                                                           length(project_years), n_other_fish)
    other_fish_catch_project_years[, i] <- apply(other_fish_catch_project_years_array[, , i], 1, sum)
    
  }
  
  # now need to work out the annual benefit for each iteration given the catch and biomass data
  # need a vector of the £/tonne for each fish
  library(readxl)
  caught_fish_sheet <- read_excel("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_small_model_set\\EcoSampler\\Base_model\\ANNEX B_Sandeel NCA final_workbook Aug 2021 base model v2.xlsx",
                                   sheet = "1. Caught fish", skip = 275)
  pound_per_tonne <- caught_fish_sheet[1:33, c(5, 8)]
  discount_factor <- as.numeric(caught_fish_sheet[79,3:62])
  
  # value for each fish is different
  # j^th row of, say, sndl_catch_project_years must also be multiplied by the j^th discount factor for the PV60

  sndl_value <- sndl_catch_project_years * as.numeric(pound_per_tonne[pound_per_tonne$Species=="Sandeels", 2])
  
  sndl_discounted_value <- matrix(NA, length(project_years), n_iter)
  
  for(j in 1:length(project_years)){
    
    sndl_discounted_value[j, ] <- sndl_value[j, ] * discount_factor[j]
    
  }
  
  fish_w_sndl_diet_value_project_years_array <- array(NA, dim = c(length(project_years), n_fish_w_sndl_diet, n_iter))
  
  other_fish_value_project_years_array <- array(NA, dim = c(length(project_years), n_other_fish, n_iter))
  
  for(i in 1:n_iter){
    
    fish_w_sndl_diet_value_project_years_array[, , i] <- fish_w_sndl_diet_catch_project_years_array[, , i]
    
    for(k_1 in 1:n_fish_w_sndl_diet){
      
      fish_w_sndl_diet_value_project_years_array[, k_1, i] <- fish_w_sndl_diet_value_project_years_array[, k_1, i] * as.numeric(pound_per_tonne[pound_per_tonne$Species == colnames(data_list_catch[[1]])[fish_w_sndl_diet][k_1], 2])
    
    }
    
    other_fish_value_project_years_array[, , i] <- other_fish_catch_project_years_array[, , i]
    
    for(k_2 in 1:n_other_fish){
      
      other_fish_value_project_years_array[, k_2, i] <- other_fish_value_project_years_array[, k_2, i] * as.numeric(pound_per_tonne[pound_per_tonne$Species == colnames(data_list_catch[[1]])[other_fish][k_2], 2])
      
    }
    
  }
  
  fish_w_sndl_diet_total_value_project_years <- matrix(NA, length(project_years), n_iter)
  other_fish_total_value_project_years <- matrix(NA, length(project_years), n_iter)
  
  fish_w_sndl_diet_discounted_value_project_years <- matrix(NA, length(project_years), n_iter)
  other_fish_discounted_value_project_years <- matrix(NA, length(project_years), n_iter)
  
  for(i in 1:n_iter){
    
    fish_w_sndl_diet_total_value_project_years[, i] <- apply(fish_w_sndl_diet_value_project_years_array[, , i], 1, sum)
    other_fish_total_value_project_years[, i] <- apply(other_fish_value_project_years_array[, , i], 1, sum)
    
    for(j in 1:length(project_years)){
      
      fish_w_sndl_diet_discounted_value_project_years[j, i] <- fish_w_sndl_diet_total_value_project_years[j, i] * discount_factor[j]
      other_fish_discounted_value_project_years[j, i] <- other_fish_total_value_project_years[j, i] * discount_factor[j]
      
    }
    
  }
  
  sndl_value_year0 <- rep(NA, n_iter)
  sndl_value_year59 <- rep(NA, n_iter)
  sndl_PV60 <- rep(NA, n_iter)
  
  fish_w_sndl_diet_value_year0 <- rep(NA, n_iter)
  fish_w_sndl_diet_value_year59 <- rep(NA, n_iter)
  fish_w_sndl_diet_PV60 <- rep(NA, n_iter)
  
  other_fish_value_year0 <- rep(NA, n_iter)
  other_fish_value_year59 <- rep(NA, n_iter)
  other_fish_PV60 <- rep(NA, n_iter)
  
  total_monetary_benefit_year0 <- rep(NA, n_iter)
  total_monetary_benefit_year59 <- rep(NA, n_iter)
  total_monetary_benefit_PV60 <- rep(NA, n_iter)
  
  tourism <- read_excel("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_small_model_set\\EcoSampler\\Base_model\\ANNEX B_Sandeel NCA final_workbook Aug 2021 base model v2.xlsx",
                        sheet = "3. Tourism", skip = 8)[1,11:13]
  offshore_wind <- read_excel("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_small_model_set\\EcoSampler\\Base_model\\ANNEX B_Sandeel NCA final_workbook Aug 2021 base model v2.xlsx",
                              sheet = "5. Offshore wind", skip = 8)[1,11:13]
  
  nature_based_tourism_year0 <- as.numeric(tourism[1])
  nature_based_tourism_year59 <- as.numeric(tourism[2])
  nature_based_tourism_PV60 <- as.numeric(tourism[3])
  
  resource_rent_value_OW_year0 <- as.numeric(offshore_wind[1])
  resource_rent_value_OW_year59 <- as.numeric(offshore_wind[2])
  resource_rent_value_OW_PV60 <- as.numeric(offshore_wind[3])
    
  for(i in 1:n_iter){
    
    sndl_value_year0[i] <- sndl_value[1, i]
    sndl_value_year59[i] <- sndl_value[60, i]  
    sndl_PV60[i] <- sum(sndl_discounted_value[, i])
    
    fish_w_sndl_diet_value_year0[i] <- fish_w_sndl_diet_total_value_project_years[1, i]
    fish_w_sndl_diet_value_year59[i] <- fish_w_sndl_diet_total_value_project_years[60, i]
    fish_w_sndl_diet_PV60[i] <- sum(fish_w_sndl_diet_discounted_value_project_years[, i])
    
    other_fish_value_year0[i] <- other_fish_total_value_project_years[1, i]
    other_fish_value_year59[i] <- other_fish_total_value_project_years[60, i]
    other_fish_PV60[i] <- sum(other_fish_discounted_value_project_years[, i])
    
    total_monetary_benefit_year0[i] <- sndl_value_year0[i] + fish_w_sndl_diet_value_year0[i] + other_fish_value_year0[i] + nature_based_tourism_year0 + resource_rent_value_OW_year0
    total_monetary_benefit_year59[i] <- sndl_value_year59[i] + fish_w_sndl_diet_value_year59[i] + other_fish_value_year59[i] + nature_based_tourism_year59 + resource_rent_value_OW_year59
    total_monetary_benefit_PV60[i] <- sndl_PV60[i] + fish_w_sndl_diet_PV60[i] + other_fish_PV60[i] + nature_based_tourism_PV60 + resource_rent_value_OW_PV60
      
  }
  
  return(list(sndl_biomass_project_years = sndl_biomass_project_years, fish_w_sndl_diet_biomass_project_years = fish_w_sndl_diet_biomass_project_years,
              other_fish_biomass_project_years = other_fish_biomass_project_years, sndl_catch_project_years = sndl_catch_project_years,
              fish_w_sndl_diet_catch_project_years = fish_w_sndl_diet_catch_project_years, other_fish_catch_project_years = other_fish_catch_project_years,
              bln_whls_biomass_project_years = bln_whls_biomass_project_years, tthd_whls_biomass_project_years = tthd_whls_biomass_project_years,
              seals_biomass_project_years = seals_biomass_project_years, sf_sbrds_biomass_project_years = sf_sbrds_biomass_project_years,
              dvng_sbrds_biomass_project_years = dvng_sbrds_biomass_project_years, sndl_value = sndl_value, sndl_discounted_value = sndl_discounted_value,
              fish_w_sndl_diet_value_project_years_array = fish_w_sndl_diet_value_project_years_array,
              fish_w_sndl_diet_discounted_value_project_years = fish_w_sndl_diet_discounted_value_project_years,
              other_fish_value_project_years_array = other_fish_value_project_years_array,
              other_fish_discounted_value_project_years = other_fish_discounted_value_project_years,
              sndl_value_year0 = sndl_value_year0, sndl_value_year59 = sndl_value_year59, sndl_PV60 = sndl_PV60,
              fish_w_sndl_diet_value_year0 = fish_w_sndl_diet_value_year0, fish_w_sndl_diet_value_year59 = fish_w_sndl_diet_value_year59,
              fish_w_sndl_diet_PV60 = fish_w_sndl_diet_PV60, other_fish_value_year0 = other_fish_value_year0,
              other_fish_value_year59 = other_fish_value_year59, other_fish_PV60 = other_fish_PV60,
              total_monetary_benefit_year0 = total_monetary_benefit_year0, total_monetary_benefit_year59 = total_monetary_benefit_year59,
              total_monetary_benefit_PV60 = total_monetary_benefit_PV60))
  
}

NCBS_test <- NCBS(data_list_biomass = biomass_annual_test$data_list, data_list_catch = catch_annual_test$data_list)
NCBS_test$sndl_biomass_project_years[, 1] # correct
NCBS_test$fish_w_sndl_diet_biomass_project_years[, 1] # correct
NCBS_test$other_fish_biomass_project_years[, 1] # correct
NCBS_test$sndl_catch_project_years[, 1] # correct
NCBS_test$fish_w_sndl_diet_catch_project_years[, 1] # correct
NCBS_test$other_fish_catch_project_years[, 1] # incorrect --- remove blue whiting?
NCBS_test$bln_whls_biomass_project_years[, 1] # correct
NCBS_test$tthd_whls_biomass_project_years[, 1] # correct
NCBS_test$seals_biomass_project_years[, 1] # correct
NCBS_test$sf_sbrds_biomass_project_years[, 1] # correct
NCBS_test$dvng_sbrds_biomass_project_years[, 1] # correct
NCBS_test$sndl_value[, 1] # correct
NCBS_test$sndl_discounted_value[, 1] # correct
NCBS_test$fish_w_sndl_diet_value_project_years_array[, , 1] # incorrect
NCBS_test$fish_w_sndl_diet_discounted_value_project_years_array[, , 1] # incorrect
NCBS_test$other_fish_value_project_years_array[, , 1] # incorrect
NCBS_test$other_fish_discounted_value_project_years_array[, , 1] # incorrect

# the values for fish w sndl diet and other fish are incorrect, but I am 99%
# confident that I have done the right thing, and the issue is in the excel
# file, which seems to calculate the annual value for each fish in these 
# categories by multiplying the catch of the fish by its corresponding
# price per tonne in euros (2020), instead of in £.
# My numbers for sandeels are correct, and this uses £/tonne in my code 
# and in the Excel spreadsheet, so this gives me more confidence in this
# claim. 
 
NCBS_test$total_monetary_benefit_year0[1]
