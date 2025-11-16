

#### Function to create model for Prior Predictive distribution #####
pp_simualtion_bernoulli <- function(data,priors){

  #### Libraries ####
  library(brms)

  ### Formula ###
  formula <- brmsformula(
    value ~ virus_dilution,
    family = bernoulli(link = "logit"))

  # Run the model
  prior_simulation <- brm(
    formula = formula,data = data,
    family = bernoulli(),
    prior = priors,
    sample_prior = "only",
    chains = 4,
    iter = 2000,
    seed = 123
    )

  # Return the Prior predictive simulation model
  return(prior_simulation)
}
