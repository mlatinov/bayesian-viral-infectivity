#### Function to Collect insights from Bayesian model ####
bayes_insights_3d <- function(model,data){

  #### Libraries ####
  library(bayesplot)
  library(posterior)
  library(brms)
  library(pracma)
  library(tidybayes)
  library(plotly)
  library(MASS)

  #### Posterior Summaries for Virus infections ####
  posterior_draws <- as_draws(bernoulli_bayes_model_sim_virus_G)

  ## Posterior Distributions on log odds scale for dilution ##
  posterior_log_odds_d <- mcmc_areas(
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

  ## Posterior Distribution on log odds scale for G ##
  posterior_log_odds_g <- mcmc_areas(
    posterior_draws,
    pars = "b_imaginary_var",
    prob = 0.95,
    prob_outer = 0.99,
    point_est = "median"
  )

  ## Credible Intervals for Virus Infections ##
  posterior_credible_intervals <- mcmc_intervals(
    posterior_draws,
    pars = c("b_virus_dilution","b_imaginary_var","b_Intercept"),
    prob = 0.95,
    outer_size = 0.99,
    point_est = "median"
  )+
    labs(
      title = "Posterior Distribution of Virus Credible Intervals of Infection and Imaginary G",
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
      title = "Posterior Distribution Histogram of Virus Dilution Effect of Infection",
      x = "Virus Dilution"
    )+
    theme_minimal()

  ### Conditional Effect of Virus Infections and Virus Dilution ####
  contional_effect <- conditional_effects(
    bernoulli_bayes_model_sim_virus_G,
    prob = 0.88
  )

  # Create realistic dilution values from the data
  dilution_values <- seq(
    min(virus_G$virus_dilution),
    max(virus_G$virus_dilution),
    length.out = 10
  )

  # Create realistic imaginary values from the data
  imaginary <- seq(
    min(virus_G$imaginary_var),
    max(virus_G$imaginary_var),
    length.out = 10
  )

  # Expand bolt variables to create prediction grid
  pred_grid <- expand_grid(
    dilution = dilution_values,
    imaginary = imaginary
  )

  # Calculate probabilities for each outcome
  probability_shifts <- posterior_draws %>%
    spread_draws(b_Intercept, b_virus_dilution,b_imaginary_var) %>%
    # Add all predictor combinations
    crossing(pred_grid) %>%
    # Calculate probability for each combination
    mutate(
      probability = plogis(
        b_Intercept + b_virus_dilution * dilution + b_imaginary_var * imaginary
        )
    )

  # Summary table
  table_virus_effect <-
    probability_shifts %>%
    mutate(
      dilution_t = round(dilution,digits = 1),
      imaginary_g = round(imaginary,digits = 3)
    ) %>%
    group_by(dilution_t,imaginary_g) %>%
    summarise(
      mean_probability = mean(probability)
    ) %>%
    ungroup()

  #### Experimental ####

  ## Compute the Posterior Distribution of the Area Under Infection Probability Curve

  # Make a full probability grid
  dilution_values <- sort(unique(probability_shifts$dilution))
  imaginary_values <- sort(unique(probability_shifts$imaginary))

  full_grid <- expand_grid(
    .draw = unique(probability_shifts$.draw),
    dilution = dilution_values,
    imaginary = imaginary_values
  )

  # Join probabilities
  prob_grid <- full_grid %>%
    left_join(probability_shifts, by = c(".draw", "dilution" = "dilution", "imaginary" = "imaginary")) %>%
    group_by(.draw) %>%
    filter(!is.na(probability))

  # Use Monte Carlo approximation:
  auipc_per_draw_mc <- prob_grid %>%
    group_by(.draw) %>%
    summarise(
      auipc_raw = mean(probability) * diff(range(dilution)) * diff(range(imaginary)),
      .groups = "drop"
    ) %>%
    mutate(
      auipc_norm = auipc_raw / (diff(range(prob_grid$dilution)) * diff(range(prob_grid$imaginary)))
    )

  # Average probability over dilution and imaginary
  posterior_surface <- prob_grid %>%
    group_by(dilution, imaginary) %>%
    summarise(
      mean_prob = mean(probability),
      .groups = "drop"
    )

  # Compute Z matrix
  z_matrix_2 <- posterior_surface %>%
    pivot_wider(
      names_from = imaginary,
      values_from = mean_prob
      ) %>%
    select(-dilution) %>%
    as.matrix()

  # Axis
  x <- sort(unique(posterior_surface$dilution))
  y <- sort(unique(posterior_surface$imaginary))

  # 3D plot for the AUIPC
  auipc_3d <-
    plot_ly(
      x = x,
      y = y,
      z = z_matrix_2,
      type = "surface"
    ) %>%
    layout(
      scene = list(
        xaxis = list(title = "Dilution"),
        yaxis = list(title = "Imaginary Var (g)"),
        zaxis = list(title = "Infection Probability")
      ),
      title = "Posterior Mean Infection Probability Surface"
    )

  # Return plots
  return(list(
    data = list(
      post_draws = posterior_draws,
      prob_data = probability_shifts,
      pauipc = auipc_per_draw
    ),
    condititional_prob_shits = condititional_prob_shits,
    error_bar = error_bar,
    table_virus_effect = table_virus_effect,
    contional_effect = contional_effect,
    posterior_histogram = posterior_histogram,
    posterior_log_odds = posterior_log_odds
  ))
}
