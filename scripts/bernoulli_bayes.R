
#### Function to run a Bayesian Bernoulli Model ####
bernoulli_bayes <- function(data,priors){

  #### Libraries ####
  library(brms)

  ### Formula ###
  formula <- brmsformula(
    value ~ virus_c,
    family = bernoulli(link = "logit"))

  # Run the model
  bernoulli_model <- brm(
    formula = formula,data = data,
    family = bernoulli(),
    prior = priors,
    chains = 4,
    iter = 2000,
    seed = 123
  )

  # Return the Prior predictive simulation model
  return(bernoulli_model)
}
