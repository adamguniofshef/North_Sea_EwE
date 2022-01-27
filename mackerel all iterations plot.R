# short script looking at mackerel over the 20 iterations, to see if each
# iteration shows increasing biomass for mackerel through years 0 to 59

plot(base_model_all_runs_biomass_annual_v1[[1]]$year[project_years],
     base_model_all_runs_biomass_annual_v1[[1]]$Mackerel[project_years],
     ylim = c(500000,1300000), type = "l", xlab = "Project Year",
     ylab = "Annual biomass of mackerel, base model")

for(i in 2:20){
  
  lines(base_model_all_runs_biomass_annual_v1[[i]]$year[project_years],
        base_model_all_runs_biomass_annual_v1[[i]]$Mackerel[project_years])
  
}

# delete once finished with: tracking the order of the functional groups
# in the BAU-ecopath_catch_tonnes worksheet

# 1, 2, 3, 4, 65, 6, 7, 8, 10, 11, 12, 13+14, 15+16, 17+18, 19+20,
# 22, 23, 26, 27, 28+29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41,
# 42, 43, 44, 49, 51, 52, 53, 54, 55, 58