
#### Function to Collect insights from Bayesian model ####
bayes_insights <- function(model,data){

  #### Libraries ####
  library(bayesplot)
  library(posterior)
  library(brms)
  library(pracma)
  library(tidybayes)
  library(plotly)

  #### Posterior Summaries for Virus infections ####
  posterior_draws <- as_draws(model)

  ## Credible Intervals for Virus Infections ##
  posterior_credible_intervals <- mcmc_intervals(
    posterior_draws,
    pars = "b_virus_dilution",
    prob = 0.95,
    outer_size = 0.99,
    point_est = "median"
  )+
    labs(
      title = "Posterior Distribution of Virus Credible Intervals of Infection",
    )+
    theme_minimal()

## Posterior Histogram for Virus Infection ##
  posterior_histogram <- mcmc_hist(
    posterior_draws,
    pars = "b_virus_dilution",
    prob = 0.95,
    outer_size = 0.99,
    point_est = "median"
  )+
    labs(
      title = "Posterior Distribution Histogram of Virus Dilution Effect of Infection"
    )+
    theme_minimal()

  ## Posterior Histogram for Virus Baseline ##
  posterior_histogram_int <- mcmc_hist(
    posterior_draws,
    pars = "b_Intercept",
    prob = 0.95,
    outer_size = 0.99,
    point_est = "median"
  )+
    labs(
      title = "Posterior Distribution Histogram of Intercept"
    )+
    theme_minimal()


### Conditional Effect of Virus Infections and Virus Dilution ####
  contional_effect <- conditional_effects(
    model,
    prob = 0.88
    )

  # Create realistic dilution values from your data
  dilution_values <- seq(
    min(data$virus_dilution),
    max(data$virus_dilution),
    length.out = 100
  )

  # Calculate probabilities for each outcome
  probability_shifts <- posterior_draws %>%
    spread_draws(b_Intercept, b_virus_dilution) %>%
    # Create a row for each dilution
    expand_grid(dilution = dilution_values) %>%
    # Calculate probability for each combination
    mutate(
      probability = plogis(b_Intercept + b_virus_dilution * dilution)
    )

  # Plot the probability shifts
  condititional_prob_shits <-
    ggplot(probability_shifts, aes(x = dilution, y = probability)) +
    stat_lineribbon(aes(fill = after_stat(level)), .width = c(0.89, 0.95)) +
    scale_fill_brewer() +
    labs(
      title = "Infection Probability Across Dilution Levels",
      x = "Virus Dilution",
      y = "Probability of Infection",
      fill = "Credible Interval"
    ) +
    theme_minimal()


  #### EC50 ####

  ## Compute EC50 as -Intercept / b_virus_dilution
  ec_50 <- probability_shifts %>%
    mutate(
      ec_50 = -b_Intercept / b_virus_dilution
    )

  ## EC50 Posterior Distribution
  ec_50_posterior <-
    ggplot(data = ec_50,aes(x = ec_50))+
    geom_histogram(color = "black",fill = "#FF9999")+
    theme_minimal()+
    labs(
      title = "Posterior EC50 Distribution",
      x = "EC50",
      y = "Count"
    )

  #### Experimental ####

  ## Compute the Posterior Distribution of the Area Under Infection Probability Curve

  # Use Trapezoid Integration
  auipc_per_draw <- probability_shifts %>%
    group_by(.draw) %>%
    arrange(dilution, .by_group = TRUE) %>%
    summarise(
      auipc_raw = trapz(x = dilution, y = probability),
      x_min = min(dilution),
      x_max = max(dilution),
      .groups = "drop"
    ) %>%
    mutate(
      auipc_norm = auipc_raw / (x_max - x_min)
    )

  # Plot the Posterior AUIPC
  auipc_distribution <-
    ggplot(data = auipc_per_draw,aes(x = auipc_norm))+
    geom_histogram(fill = "lightblue",colour = "black")+
    theme_minimal()+
    labs(
      title = "Posterior Distribution of the Area Under Infection Probability Curve",
      x  = "AUIPC",
      y  = "Count"
    )

  # Return plots and tables
  return(list(
    data = list(
      post_draws = posterior_draws,
      prob_data = probability_shifts,
      pauipc = auipc_per_draw,
      ec50_raw = ec_50
    ),
    plots = list(
      condititional_prob_shits = condititional_prob_shits,
      contional_effect = contional_effect,
      posterior_histogram = posterior_histogram,
      auipc_distribution = auipc_distribution,
      ec_50_posterior = ec_50_posterior
      )
    ))
}
