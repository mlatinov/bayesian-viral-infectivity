
#### Function to make a Bernoulli Hierarchical model for the combined data ####
bernoulli_hierarchical <- function(data,priors){

  #### Libraries ####
  library(brms)

  ### Formula ###
  formula <- brmsformula(
    outcome ~ virus_dilution + (virus_dilution | experiment ),
    family = brms::bernoulli()
    )

  #### Hierarchical model  ####
  hierarchical_model <- brm(
    formula = formula,
    data = data,
    family = bernoulli,
    prior = priors,
    chains = 4,
    iter = 5000,
    control = list(adapt_delta = 0.99),
    seed = 123
  )

  # Return the model
  return(hierarchical_model)
}
