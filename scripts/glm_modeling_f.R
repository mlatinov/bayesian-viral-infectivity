
#### Function for Classic GLM Model for reference ####
glm_modeling_f <- function(data){

  #### Libraries ####
  library(logistf)

  # Fit the model
  glm_model <- logistf(value ~ virus_c, data = data)

  # Create a fine sequence of virus_c for smooth curve
  virus_seq <- seq(min(data$virus_c),
                   max(data$virus_c),
                   length.out = 200)

  # Predicted probabilities
  pred_probs <- predict(glm_model, newdata = data.frame(virus_c = virus_seq), type = "response")

  # Combine into a data frame
  curve_df <- data.frame(virus_c = virus_seq, pred = pred_probs)

  # Plot raw data + logistic curve
  p1 <-
    ggplot(data, aes(x = virus_c, y = value)) +
    geom_line(data = curve_df, aes(y = pred), color = "blue", size = 1.2) +
    labs(
      title = "Logistics model predictions",
      x = "log(Virus concentration)",
      y = "Predicted Probability of infection",
    ) +
    theme_minimal()

  # Return
  return(list(
    model_summary = summary(glm_model),
    model_pred_plot = p1
    )
  )
}
