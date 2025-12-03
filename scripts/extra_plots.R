
#### Function to make Extra Comparison plots between models ####
extra_model_plots <- function(model_list,model_insight_list){

  #### Libraries ####
  library(tidybayes)
  library(ggridges)

  #### Extract Intercept for all models and mutate a marker ####
  beta_intercept_all <-
    bind_rows(
      model_list$virus_low_infection %>% spread_draws(b_Intercept,b_virus_dilution) %>% mutate(model = "B"),
      model_list$virus_not_effected %>% spread_draws(b_Intercept,b_virus_dilution) %>% mutate(model = "C"),
      model_list$virus_resistant %>% spread_draws(b_Intercept,b_virus_dilution) %>% mutate(model = "D"),
      model_list$virus_mutant %>% spread_draws(b_Intercept,b_virus_dilution) %>% mutate(model = "E")
    )

  #### Plot posterior density in ridget plot for b_Intercept ####
  beta_intercept_all$model <- paste0("theta[", beta_intercept_all$model, "]")

  intercept_ridges <-
    ggplot(data = beta_intercept_all,aes(x = b_Intercept,y = model ,fill = stat(x)))+
    geom_density_ridges_gradient() +
    scale_fill_viridis_c(name = "Depth", option = "C") +
    scale_y_discrete(labels = function(x) parse(text = x)) +
    coord_cartesian(clip = "off") +
    theme_minimal() +
    labs(
      title = expression("Posterior " ~ theta ~ " distributions for " ~ beta[0]),
      x = expression(beta[0]),
      y = "Simulation"
    )

  #### Plot posterior density in ridget plot for b_virus_dilution ####
  beta_ridges <-
    ggplot(data = beta_intercept_all,aes(x = b_virus_dilution ,y = model ,fill = stat(x)))+
    geom_density_ridges_gradient() +
    scale_fill_viridis_c(name = "Depth", option = "C") +
    scale_y_discrete(labels = function(x) parse(text = x)) +
    coord_cartesian(clip = "off") +
    theme_minimal() +
    labs(
      title = expression("Posterior " ~ theta ~ " distributions for " ~ beta[1]),
      x = expression(beta[1]),
      y = "Simulation"
    )

  #### EC50 ####
  ec_50 <- bind_rows(
    model_insight_list$virus_low_infection$data$ec50_raw %>% mutate(model = "B"),
    model_insight_list$virus_not_effected$data$ec50_raw %>% mutate(model = "C"),
    model_insight_list$virus_mutant$data$ec50_raw %>% mutate(model = "E")
  )

  ec_50$model <- paste0("theta[", ec_50$model, "]")

  # Plot Posterior EC50
  ec_50_ridges <-
    ggplot(data = ec_50,aes(x = ec_50  ,y = model ,fill = stat(x)))+
    geom_density_ridges_gradient() +
    scale_fill_viridis_c(name = "Depth", option = "D") +
    scale_y_discrete(labels = function(x) parse(text = x)) +
    theme_minimal() +
    labs(
      title = "Posterior distributions EC50",
      x = "EC50",
      y = "Simulation"
    )

  #### AUIPC ####
  auipc <- bind_rows(
    model_insight_list$virus_low_infection$data$pauipc %>% mutate(model = "B"),
    model_insight_list$virus_not_effected$data$pauipc %>% mutate(model = "C"),
    model_insight_list$virus_resistant$data$pauipc %>% mutate(model = "D"),
    model_insight_list$virus_mutant$data$pauipc %>% mutate(model = "E")
  )

  auipc$model <- paste0("theta[", auipc$model, "]")

  # Plot AUIPC
  auipc_ridges <-
    ggplot(data = auipc,aes(x = auipc_norm,y = model ,fill = stat(x)))+
    geom_density_ridges_gradient() +
    scale_fill_viridis_c(name = "Depth", option = "C") +
    scale_y_discrete(labels = function(x) parse(text = x)) +
    theme_minimal() +
    labs(
      title = "Posterior distributions AUIPC",
      x = "AUIPC",
      y = "Simulation"
    )

  # Return
  return(list(
    auipc_ridges = auipc_ridges,
    ec_50_ridges = ec_50_ridges,
    beta_ridges = beta_ridges,
    intercept_ridges = intercept_ridges
    )
  )
}
