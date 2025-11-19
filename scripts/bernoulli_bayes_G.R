
#### Function to Create Bayesian Model for Simulated Virus G ####
bernoulli_bayes_G <- function(data,priors) {

  #### Libraries ####
  library(brms)

  #### Formula ####
  formula <- brmsformula(
    outcome ~ virus_dilution + imaginary_var,
    family = bernoulli(link = "logit")
    )

  #### Model ####
  bayes_g <-
    brm(
      formula = formula,
      data = data,
      family = bernoulli,
      prior = priors,
      chains = 4,
      iter = 4000,
      seed = 123
      )

  # Return the model
  return(bayes_g)

}
