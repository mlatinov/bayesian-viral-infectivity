
#### Function to Collect insights from Bayesian model ####
bayes_insights <- function(model,data){

  #### Libraries ####
  library(bayesplot)
  library(posterior)
  library(brms)
  library(pracma)
  library(tidybayes)

  #### Posterior Summaries for Virus infections ####
  posterior_draws <- as_draws(model)

  ## Posterior Distributions on log odds scale ##
  posterior_log_odds <- mcmc_areas(
    posterior_draws,
    pars = "b_virus_dilution",
    prob = 0.95,
    prob_outer = 0.99,
    point_est = "median"
    )+
    labs(
      title = "Posterior Distribution of Virus Dilution Effect of Infection",
      subtitle = paste0("Median : " , 3.24," ")
    )+
    theme_minimal()

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

### Conditional Effect of Virus Infections and Virus Dilution ####
  contional_effect <- conditional_effects(
    model,
    prob = 0.88,
    spaghetti = TRUE
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

  # Summary table
  table_virus_effect <-
    probability_shifts %>%
    mutate(
      dilution_t = round(dilution,digits = 1)
    ) %>%
    group_by(dilution_t) %>%
    summarise(
      mean_probability = mean(probability),
      ci_95 = quantile(probability, 0.975),
      ci_05 = quantile(probability, 0.025),
    )

  ## Error Bar plot ##
  error_bar <-
    table_virus_effect %>%
    ggplot(aes(x = dilution_t,y = mean_probability))+
    geom_point()+
    geom_errorbar(
      data= table_virus_effect,aes(ymin = ci_05,ymax = ci_95,colour = "red"),width = 0.2)+
    labs(
      title = "Infection Probability Across Dilution Levels",
      x = "Virus Dilution",
      y = "Probability of Infection",
      color = "Credible Interval"
    ) +
    theme_minimal()


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

  # Return plots
  return(list(
    condititional_prob_shits = condititional_prob_shits,
    error_bar = error_bar,
    table_virus_effect = table_virus_effect,
    contional_effect = contional_effect,
    posterior_histogram = posterior_histogram,
    posterior_log_odds = posterior_log_odds
    ))
}
