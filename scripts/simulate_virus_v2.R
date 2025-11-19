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
  a <- rnorm(n = 10, mean = baseline_infectivity_mean,baseline_infectivity_sd)

  # Coef for the impact of the dilution on the probability of infection
  b <- rnorm(n = 10, mean = beta_coef_mean,beta_coef_sd)

  # Coef for the impact of third imaginary variable Gamma on the probability of infection
  g <- rnorm(n = 10, mean = gamma_coef_mean,gamma_coef_sd)

  ## The Parameter P_i can be expressed as logistic linear equation
  p_matrix <- sapply(1:n_draws, function(i) 1 / (1 + exp(-(a[i] + b[i] * x_i  + g[i]))))

  ## The prob of virus infection comes from Bernoulli Distribution ##
  outcome_matrix <- apply(p_matrix, 2, function(p) rbinom(length(p), 1, p))

  ## Combine in dataframe with the Outcome and the Virus Dilution ##
  virus_data <- data.frame(
    virus_dilution = rep(x_i, times = n_draws),
    imaginary_var = rep(g, each = length(x_i)),
    repetition = rep(1:n_draws, each = length(x_i)),
    outcome = as.vector(outcome_matrix)
  )

  ## Return the dataframe
  return(virus_data)
}
