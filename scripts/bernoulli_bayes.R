
#### Function to run a Bayesian Bernoulli Model ####
bernoulli_bayes <- function(data,priors){

  #### Libraries ####
  library(brms)

  ### Formula ###
  formula <- brmsformula(
    value ~ virus_dilution,
    family = bernoulli(link = "logit"))

  # Run the model
  bernoulli_model <- brm(
    formula = formula,data = data,
    family = bernoulli(),
    prior = priors,
    chains = 4,
    iter = 4000,
    seed = 123
  )
  # Save the model and delete the previous one if exists
  if (file.exists("bernoulli_model.rds")) {
    file.remove("bernoulli_model.rds")
  }
  saveRDS(bernoulli_model, file = "bernoulli_model.rds")

  # Return the Prior predictive simulation model
  return(bernoulli_model)
}
