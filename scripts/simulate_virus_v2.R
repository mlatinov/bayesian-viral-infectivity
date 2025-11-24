#### Function to Simulate Viruses Infections Probability as a function of dilution + another random variable gamma ####
simulate_virus_v2 <- function(
    baseline_infectivity_mean,
    baseline_infectivity_sd,
    beta_coef_mean,
    beta_coef_sd,
    gamma_coef_mean,
    gamma_coef_sd,
    n_draws = 2
){

  # X_i is the Virus dilution  Which is in the log scale with range from -1 to -7
  x_i <- round(seq(from = -1 , to = -7,length.out = 1000),digits = 0)

  ## The Parameters for the P_i equation comes from normal distribution ##

  # Intercept The probability of infection when x_i = 0
  a <- rnorm(n = n_draws, mean = baseline_infectivity_mean,baseline_infectivity_sd)

  # Coef for the impact of the dilution on the probability of infection
  b <- rnorm(n = n_draws, mean = beta_coef_mean,beta_coef_sd)

  # Random Imaginary variable
  c <- rnorm(n = n_draws, mean = gamma_coef_mean, sd = gamma_coef_sd)

  # Coef for the impact of third imaginary variable Gamma on the probability of infection
  g_values <- rnorm(n = length(x_i) * n_draws, mean = 0, sd = 1)

  ## The Parameter P_i can be expressed as logistic linear equation
  p_matrix <- sapply(1:n_draws, function(i) {
    idx <- ((i - 1) * length(x_i) + 1):(i * length(x_i))
    1 / (1 + exp(-(a[i] + b[i] * x_i + c[i] * g_values[idx])))
  })

  ## The prob of virus infection comes from Bernoulli Distribution ##
  outcome_matrix <- apply(p_matrix, 2, function(p) rbinom(length(p), 1, p))

  ## Combine in dataframe with the Outcome and the Virus Dilution ##
  virus_data <- data.frame(
    virus_dilution = rep(x_i, n_draws),
    imaginary_var = g_values,
    repetition     = rep(1:n_draws, each = length(x_i)),
    outcome        = as.vector(outcome_matrix)
  )

  ## Return the dataframe
  return(virus_data)
}
