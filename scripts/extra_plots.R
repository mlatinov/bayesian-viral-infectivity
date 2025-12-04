
#### Function to make Extra Comparison plots between models ####
extra_model_plots <- function(model_list,model_insight_list){

  #### Libraries ####
  library(tidybayes)
  library(ggridges)

  #### Extract Intercept for all models and mutate a marker ####
  beta_intercept_all <-
    bind_rows(
      model_list$b %>% spread_draws(b_Intercept,b_virus_dilution) %>% mutate(model = "B"),
      model_list$c %>% spread_draws(b_Intercept,b_virus_dilution) %>% mutate(model = "C"),
      model_list$d %>% spread_draws(b_Intercept,b_virus_dilution) %>% mutate(model = "D"),
      model_list$e %>% spread_draws(b_Intercept,b_virus_dilution) %>% mutate(model = "E"),
      model_list$f %>% spread_draws(b_Intercept,b_virus_dilution) %>% mutate(model = "F"),
      model_list$g %>% spread_draws(b_Intercept,b_virus_dilution) %>% mutate(model = "G"),
      model_list$h %>% spread_draws(b_Intercept,b_virus_dilution) %>% mutate(model = "H"),
      model_list$i %>% spread_draws(b_Intercept,b_virus_dilution) %>% mutate(model = "I"),
      model_list$j %>% spread_draws(b_Intercept,b_virus_dilution) %>% mutate(model = "J"),
      model_list$k %>% spread_draws(b_Intercept,b_virus_dilution) %>% mutate(model = "K")
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
    model_insight_list$b$data$ec50_raw %>% mutate(model = "B"),
    model_insight_list$c$data$ec50_raw %>% mutate(model = "C"),
    model_insight_list$d$data$ec50_raw %>% mutate(model = "D"),
    model_insight_list$e$data$ec50_raw %>% mutate(model = "E"),
    model_insight_list$f$data$ec50_raw %>% mutate(model = "F"),
    model_insight_list$g$data$ec50_raw %>% mutate(model = "G"),
    model_insight_list$h$data$ec50_raw %>% mutate(model = "H"),
    model_insight_list$i$data$ec50_raw %>% mutate(model = "I"),
    model_insight_list$j$data$ec50_raw %>% mutate(model = "J"),
    model_insight_list$k$data$ec50_raw %>% mutate(model = "K")

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
    model_insight_list$b$data$pauipc %>% mutate(model = "B"),
    model_insight_list$c$data$pauipc %>% mutate(model = "C"),
    model_insight_list$d$data$pauipc %>% mutate(model = "D"),
    model_insight_list$e$data$pauipc %>% mutate(model = "E"),
    model_insight_list$f$data$pauipc %>% mutate(model = "F"),
    model_insight_list$g$data$pauipc %>% mutate(model = "G"),
    model_insight_list$h$data$pauipc %>% mutate(model = "H"),
    model_insight_list$i$data$pauipc %>% mutate(model = "I"),
    model_insight_list$j$data$pauipc %>% mutate(model = "J"),
    model_insight_list$k$data$pauipc %>% mutate(model = "K")
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
