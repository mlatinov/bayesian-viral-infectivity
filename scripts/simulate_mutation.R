
#### Function to Simulation Random Walk Effect in Beta  ####
simulate_mutation <- function(
    baseline_infectivity_mean,
    baseline_infectivity_sd,
    beta_coef_mean,
    beta_coef_sd,
    mutation_effect_mean, # Mean effect of the mutation
    mutation_effect_sd,   # Var of the effect of mutation
    n_draws = 2,
    n = 58
){

  ## Set Seed
  set.seed(123)

  # X_i is the Virus dilution  Which is in the log scale with range from -1 to -7
  x_i <- round(seq(from = -1 , to = -7,length.out = n),digits = 0)

  ## The Parameters for the P_i equation comes from normal distribution ##

  # Intercept The probability of infection when x_i = 0
  a <- rnorm(n = n_draws, mean = baseline_infectivity_mean,baseline_infectivity_sd)

  # Coef for the impact of the dilution on the probability of infection

  # Each replicate evolves independently
  b <- numeric(n_draws)
  b[1] <- rnorm(1, mean = beta_coef_mean, sd = beta_coef_sd)

  # Loop over n_draws
  for(i in 2:n_draws){
    b[i] <- b[i-1] + rnorm(1, mutation_effect_mean, mutation_effect_sd)  # cumulative effect across replicates
  }
  # Fixed beta per replicate
  p_matrix <- sapply(1:n_draws, function(i){
    1 / (1 + exp(-(a[i] + b[i] * x_i)))
  })

  ## The prob of virus infection comes from Bernoulli Distribution ##
  outcome_matrix <- apply(p_matrix, 2, function(p) rbinom(length(p), 1, p))

  ## Combine in dataframe with the Outcome and the Virus Dilution ##
  virus_data <- data.frame(
    virus_dilution = rep(x_i, times = n_draws),
    repetition = rep(1:n_draws, each = length(x_i)),
    outcome = as.vector(outcome_matrix)
  )

  ## Return the dataframe
  return(virus_data)
}
