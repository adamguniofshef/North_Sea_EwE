# having a little play with mizer

# example from sizespectrum.org/mizer/

library(mizer)

# first line is for setting the model parameters, done by creating an object
# of class MizerParams. Model parameters include life history parameters of 
# each species, and the range of the size spectrum
# newMultispeciesParams sets up a multi-species size spectrum model
# by filling all slots in the MizerParams object based on user-provided
# or default parameters.
params <- newMultispeciesParams(NS_species_params, NS_interaction)

# second line runs the size spectrum model simulation
# returns an object of type MizerSim, which can be further 
# explored with summary, indicator and plotting functions,
# as in line 3
sim <- project(params, t_max = 10, effort = 0)
plot(sim)

# using different function for setting model parameters

params_community <- newCommunityParams()
sim_community <- project(params_community, t_max = 10, effort = 0)
plot(sim_community)
