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

sim <- project(params, t_max = 10, effort = 0)
plot(sim)