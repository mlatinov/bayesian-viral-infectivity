#### Function to Check if the Generated data produces Expected Results ####
check_generated_virus <- function(data){

  #### Libraries ####
  library(patchwork)

  ## Table
  table <-
    data %>%
    group_by(virus_dilution) %>%
    summarise(
      count = sum(outcome),
      infection_rate = (sum(outcome) / n()) * 100
    )

  ## Infection Rates  as a function of dilution
  p_1 <-
    ggplot(data = table,aes(x = virus_dilution,y = infection_rate))+
    geom_col(fill = "lightblue")+
    theme_minimal()+
    labs(
      title = "Virus Outcome over the Virus Dilution",
      x = "Virus Dilution",
      y = "Infection Rate"
    )

  ## Line plot of mean infection rates
  p_2 <-
    data %>%
    group_by(virus_dilution) %>%
    summarise(infection_rate = mean(outcome)) %>%
    ggplot(aes(x = virus_dilution, y = infection_rate)) +
    geom_point(size = 3) +
    geom_line() +
    theme_minimal() +
    labs(
      title = "Mean Infection Rate by Dilution",
      y = "Infection Rate",
      x = "Virus Dilution"
    )

  ## Log Curve
  p_3 <-
    ggplot(data, aes(x = virus_dilution, y = outcome)) +
    geom_jitter(width = 0.1, height = 0.05, alpha = 0.2) +
    stat_smooth(
      method = "glm",
      method.args = list(family = "binomial"),
      se = FALSE,
      color = "blue"
    ) +
    theme_minimal() +
    labs(
      title = "Observed Data With Fitted Logistic Curve",
      x = "Virus Dilution",
      y = "Outcome"
    )

  ## Combine the plots
  p_final <- (p_1 + p_2) / p_3

  # Return
  return(list(
    plots = list(
      log_curve = p_3,
      line_plot = p_2,
      col_plot = p_1,
      combined_plot = p_final
      ),
    summary_table = table
    ))
}
