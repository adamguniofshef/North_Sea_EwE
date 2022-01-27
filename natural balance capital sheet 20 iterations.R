# looking at reproducing Table 7.2 from the NCA final report. Doing this using
# ANNEX B spreadsheet, which has formulae for automatically calculating the figures
# in the table based on the estimates of the catch for the project years.

# What we then need to do is simply reproduce the spreadsheet, just altering the
# catch estimates from EwE by copy and pasting those estimates from each of the 20
# iterations. So, we end up with 20 spreadsheets, and 20 versions of Table 7.2,
# given the catch estimates from each iteration, and we can then summarise the
# our 20 tables with mean and standard deviation estimates, etc.

# tracking the order of the functional groups
# in the BAU-ecopath_catch_tonnes worksheet

# 1, 2, 3, 4, 65, 6, 7, 8, 10, 11, 12, 13+14, 15+16, 17+18, 19+20,
# 22, 23, 26, 27, 28+29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41,
# 42, 43, 44, 49, 51, 52, 53, 54, 55, 58

# In letters according to Excel lettering (first column being year in catch/biomass):

# B, C, D, E, BN, G, H, I, K, L, M, BS, BT, BU, BV, W, X, AA, AB, BW,
# AE, AF, AG, AH, AI, AJ, AK, AL, AM, AN, AO, AP, AQ, AR, AS, AX, AZ, BA,
# BB, BC, BD, BG

# I have all 20 spreadsheets now, just need to get them into R. Each are located
# in a worksheet of the Annex B spreadsheet, so need to figure out how to import
# just the worksheet into R.

install.packages("xlsx")
library(rJava)
library(xlsxjars)
library(xlsx)

library(readxl)

NC_balance_sheet_1 <- read_excel("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_small_model_set\\EcoSampler\\Base_model\\ANNEX B_Sandeel NCA final_workbook Aug 2021 base model v2.xlsx",
                                sheet = "7.2 BAU Extended NCBS", skip = 16)
NC_balance_sheet_1[,5]

# columns 5 and 6 gives the values for the LHS of the first part of Table 7.2
# which includes fixed values for subareas of North Sea, biomasses of notable
# fish in the North Sea and fixed values for renewable energy
# columns 10 and 11 give us the values for the volume of landings and the biomass
# of porpoises, seals and seabirds and fixed values for offshore wind generation in ICES IV

NC_balance_sheet_1[17:25, 9:11]

NC_asset_baseline_1 <- matrix(NA, 8, 2)

for(i in 1:2){
  for(j in 1:8){
  
  NC_asset_baseline_1[j, i] <- as.numeric(NC_balance_sheet_1[j+7, i+4])
  
  }
}

NC_ecosystem_service_1 <- matrix(NA, 4, 2)

for(i in 1:2){
  for(j in 1:3){
    
    NC_ecosystem_service_1[j, i] <- as.numeric(NC_balance_sheet_1[j+1, i+9])
    
  }
  
  NC_ecosystem_service_1[4, i] <- as.numeric(NC_balance_sheet_1[20, i+9])
  
}

NC_benefits_and_values_1 <- matrix(NA, 4, 3)

for(i in 1:3){
  for(j in 1:3){
    
    NC_benefits_and_values_1[j, i] <- as.numeric(NC_balance_sheet_1[j+1, i+16])
    
  }
  
  NC_benefits_and_values_1[4, i] <- as.numeric(NC_balance_sheet_1[23, i+16])
  
}

NC_balance_sheet_20_iterations <- vector("list", 20)
NC_balance_sheet_20_iterations[[1]] <- NC_balance_sheet_1

for(i in 1:19){
  
  NC_balance_sheet_20_iterations[[i+1]] <- read_excel(paste0("C:\\Users\\adamg\\Google Drive\\Natural England project\\N.Sea_EwE_small_model_set\\EcoSampler\\Run_", i, "\\ANNEX B_Sandeel NCA final_workbook Aug 2021 Run_", i, " v2.xlsx"),
                                                        sheet = "7.2 BAU Extended NCBS", skip = 16)
  
}

NC_asset_baseline <- array(NA, dim = c(8, 2, 20))
NC_asset_baseline[, , 1] <- NC_asset_baseline_1

for(k in 1:19){
  for(i in 1:2){
    for(j in 1:8){
    
    NC_asset_baseline[j, i, k+1] <- as.numeric(NC_balance_sheet_20_iterations[[k+1]][j+7, i+4])
    
    
    }
  }
}

NC_ecosystem_service <- array(NA, dim = c(4, 2, 20))
NC_ecosystem_service[, , 1] <- NC_ecosystem_service_1

for(k in 1:19){
  for(i in 1:2){
    for(j in 1:3){
    
    NC_ecosystem_service[j, i, k+1] <- as.numeric(NC_balance_sheet_20_iterations[[k+1]][j+1, i+9])
    
    }
  
  NC_ecosystem_service[4, i, k+1] <- as.numeric(NC_balance_sheet_20_iterations[[k+1]][20, i+9])
  
  }
}

NC_benefits_and_values <- array(NA, dim = c(4, 3, 20))
NC_benefits_and_values[, , 1] <- NC_benefits_and_values_1

for(k in 1:19){
  for(i in 1:3){
    for(j in 1:3){
      
      NC_benefits_and_values[j, i, k+1] <- as.numeric(NC_balance_sheet_20_iterations[[k+1]][j+1, i+16])
      
    }
    
    NC_benefits_and_values[4, i, k+1] <- as.numeric(NC_balance_sheet_20_iterations[[k+1]][23, i+16])
    
  }
}

# I have all the data for Table 7.2 from each of the 20 iterations,
# so can now do some summary statistics on these to evaluate them

NC_asset_baseline # this just gives the biomass of sandeels, fish that feed on sandeels,...
# first column is year 0, second column is year 59

NC_asset_baseline_mean <- apply(NC_asset_baseline, c(1, 2), mean)
NC_asset_baseline_sd <- apply(NC_asset_baseline, c(1, 2), sd)
NC_asset_baseline_2.5_97.5_quantiles <- apply(NC_asset_baseline, c(1, 2), quantile, prob = c(0.025, 0.975))

NC_ecosystem_service_mean <- apply(NC_ecosystem_service, c(1, 2), mean)
NC_ecosystem_service_sd <- apply(NC_ecosystem_service, c(1, 2), sd)
NC_ecosystem_service_2.5_97.5_quantiles <- apply(NC_ecosystem_service, c(1, 2), quantile, prob = c(0.025, 0.975))

NC_benefits_and_values_mean <- apply(NC_benefits_and_values, c(1, 2), mean)
NC_benefits_and_values_sd <- apply(NC_benefits_and_values, c(1, 2), sd)
NC_benefits_and_values_2.5_97.5_quantiles <- apply(NC_benefits_and_values, c(1, 2), quantile, prob = c(0.025, 0.975))

rownames(NC_asset_baseline_mean) <- paste0("mean ", pull(NC_balance_sheet_1[8:15, 4]))
colnames(NC_asset_baseline_mean) <- NC_balance_sheet_1[1, c(5, 6)]

rownames(NC_ecosystem_service_mean) <- paste0("mean ", pull(NC_balance_sheet_1[c(2:4, 20), 9]))
colnames(NC_ecosystem_service_mean) <- NC_balance_sheet_1[1, c(5, 6)]

rownames(NC_benefits_and_values_mean) <- paste0("mean ", c(pull(NC_balance_sheet_1[2:4, 16]),
                                                           pull(NC_balance_sheet_1[23, 14])))
colnames(NC_benefits_and_values_mean) <- NC_balance_sheet_1[1, 17:19]

rownames(NC_asset_baseline_sd) <- paste0("s.d. ", pull(NC_balance_sheet_1[8:15, 4]))
colnames(NC_asset_baseline_sd) <- NC_balance_sheet_1[1, c(5, 6)]

rownames(NC_ecosystem_service_sd) <- paste0("s.d. ", pull(NC_balance_sheet_1[c(2:4, 20), 9]))
colnames(NC_ecosystem_service_sd) <- NC_balance_sheet_1[1, c(5, 6)]

rownames(NC_benefits_and_values_sd) <- paste0("s.d. ", c(pull(NC_balance_sheet_1[2:4, 16]),
                                                           pull(NC_balance_sheet_1[23, 14])))
colnames(NC_benefits_and_values_sd) <- NC_balance_sheet_1[1, 17:19]

round(NC_asset_baseline_mean - 1.96 * NC_asset_baseline_sd)
round(NC_asset_baseline_mean + 1.96 * NC_asset_baseline_sd)

round(NC_ecosystem_service_mean - 1.96 * NC_ecosystem_service_sd)
round(NC_ecosystem_service_mean + 1.96 * NC_ecosystem_service_sd)

NC_benefits_and_values_mean - 1.96 * NC_benefits_and_values_sd
round(NC_benefits_and_values_mean + 1.96 * NC_benefits_and_values_sd)
