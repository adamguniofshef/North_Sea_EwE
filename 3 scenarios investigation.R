# working now with the data from the 3 scenarios

# important to mention that each iteration for the 3 scenarios are connected
# in that the same parameterisation was used for each of those iterations

# so sample_1 for MSY, half_MSY and no_F come from using the same parameters,
# and so these samples should be compared (leading to a distribution over
# the differences)

biomass_annual_3_scenarios <- North_Sea_EwE_data(output = "biomass_annual", n_iter = 40, total_years = 110,
                                                      project_years = 28:87)
catch_annual_3_scenarios <- North_Sea_EwE_data(output = "catch_annual", n_iter = 40, total_years = 110,
                                               project_years = 28:87)

# general aims are:

# produce plots corresponding to Figure 7 in the report, taking into account
# the uncertainty over the 40 iterations

North_Sea_EwE_plot(data_list_MSY = biomass_annual_3_scenarios$data_list_MSY,
                   data_list_half_MSY = biomass_annual_3_scenarios$data_list_half_MSY,
                   data_list_no_F = biomass_annual_3_scenarios$data_list_no_F, n_iter = 40,
                   output = "biomass_annual", groups = c("fish_groups", "sandeel_diet",
                                                         "seabirds", "whales", "seals"))

# to produce the NCBS sheet for each scenario, taking into account the uncertainty
# over the 40 iterations within a scenario

# to produce 3 NCBS sheet comparisons, comparing each scenario in turn
# this is analogous to Table 7.5 in the report

MSY_NCBS <- NCBS(data_list_biomass = biomass_annual_3_scenarios$data_list_MSY,
                 data_list_catch = catch_annual_3_scenarios$data_list_MSY, n_iter = 40)


half_MSY_NCBS <- NCBS(data_list_biomass = biomass_annual_3_scenarios$data_list_half_MSY,
                 data_list_catch = catch_annual_3_scenarios$data_list_half_MSY, n_iter = 40)


no_F_NCBS <- NCBS(data_list_biomass = biomass_annual_3_scenarios$data_list_no_F,
                 data_list_catch = catch_annual_3_scenarios$data_list_no_F, n_iter = 40)

MSY_vs_half_MSY_NCBS <- NCBS_compare(NCBS_1 = MSY_NCBS, NCBS_2 = half_MSY_NCBS)

round(MSY_NCBS$prpss_seals_sbrds_biomass_year59_quantiles)
round(half_MSY_NCBS$prpss_seals_sbrds_biomass_year59_quantiles)
round(MSY_vs_half_MSY_NCBS$prpss_seals_sbrds_biomass_year59_percent_change_quantiles)

MSY_vs_no_F_NCBS <- NCBS_compare(NCBS_1 = MSY_NCBS, NCBS_2 = no_F_NCBS)

round(no_F_NCBS$prpss_seals_sbrds_biomass_year59_quantiles)
round(MSY_vs_no_F_NCBS$prpss_seals_sbrds_biomass_year59_percent_change_quantiles)
