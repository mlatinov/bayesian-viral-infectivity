
#### Function to Simulate Seasonal Virus ####
seasonal_virus <- function(
    baseline_infectivity_mean,
    baseline_infectivity_sd,
    beta_coef_mean,
    beta_coef_sd,
    seasonal_variation = 1, # amplitude of seasonal variation
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
  b <- rnorm(n = n_draws, mean = beta_coef_mean,beta_coef_sd)

  ## The Parameter P_i can be expressed as logistic equation with oscillating alpha
  p_matrix <- sapply(1:n_draws, function(i) {
    alpha_t <- a[i] + seasonal_variation * sin(2 * pi * seq_along(x_i) / length(x_i))  # sine variation across replicates
    1 / (1 + exp(-(alpha_t + b[i] * x_i)))
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
