
#### Function to Simulate Virus in Multiple Experiments Settings for Hierarchical Bayes Model ####
simulate_virus_hierarchical <- function(
    baseline_infectivity_mean,
    baseline_infectivity_sd,
    beta_coef_mean,
    beta_coef_sd,
    rho = 0,                     # correlation between intercepts and slopes
    n_experiments = 5,           # number of experiments
    n_per_experiment = 1000      # number of dilution points per experiment
){

  #### Libraries ####
  library(MASS)

  # Virus dilution values
  x_i <- round(seq(from = -1, to = -7, length.out = n_per_experiment),digits = 0)

  # Covariance matrix for MVN
  Sigma <- matrix(
    c(baseline_infectivity_sd^2,
      rho * baseline_infectivity_sd * beta_coef_sd,
      rho * baseline_infectivity_sd * beta_coef_sd,
      beta_coef_sd^2),
    nrow = 2, byrow = TRUE
  )

  # Generate experiment specific effects
  effects <- mvrnorm(
    n = n_experiments,
    mu = c(baseline_infectivity_mean, beta_coef_mean),
    Sigma = Sigma
  )

  alpha_j <- effects[,1]
  beta_j  <- effects[,2]

  # Generate data for each experiment
  virus_data <- do.call(rbind,
                        lapply(1:n_experiments, function(j){

                          # logistic probability
                          p_ij <- plogis(alpha_j[j] + beta_j[j] * x_i)

                          # Bernoulli outcomes
                          outcome_ij <- rbinom(n_per_experiment, size = 1, prob = p_ij)

                          data.frame(
                            experiment = j,
                            virus_dilution = x_i,
                            outcome = outcome_ij
                          )
                        })
  )

  # Return the generated data
  return(virus_data)
}
