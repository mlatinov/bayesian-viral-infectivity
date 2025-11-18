
#### Function to Compare Bayesian Models ####
compare_bayes_models <- function(model) {

  #### Libraries ####
  library(brms)

  #### Model Comparison ####

  ## Measure of Explained Variance R2

  # Collect the R2 Data
  r2_distribution_data <- as.data.frame(
    bayes_R2(model,summary = FALSE))

  # Plot R2 Histogram
  r2_histogram <-
    ggplot(data = r2_distribution_data,aes(x = R2))+
    geom_histogram(fill = "lightblue",colour = "black")+
    theme_minimal()+
    labs(
      title = "Posterior Bayesian R2 Distribution",
      x = "R2",
      y = "Count"
    )

  # LOO R2
  loo_r2_data <- data.frame(loo_R2(model,summary = FALSE))

  # Plot the LOO R2 Distribution
  loo_r2_distirubtion <-
    ggplot(data = loo_r2_data,aes(x = R2))+
    geom_histogram(fill = "lightblue",colour = "black")+
    theme_minimal()+
    labs(
      title = "Leave One Out R2 Distribution",
      x = "LOO R2",
      y = "Count"
    )

  # List with R2 plots and data
  r2 <- list(
    data = list(
      in_sample_r2_data = r2_distribution_data,
      loo_r2_data = loo_r2_data
      ),
    plots = list(
      in_sample_r2_distribution = r2_histogram,
      loo_r2_distirubtion = loo_r2_distirubtion
    )
  )

  ## Measure of Errors

  # Collect the Posterior Expected  Errors
  error <- predictive_error(model,method = "posterior_epred")

  # Compute Bayesian RMSE
  bayes_rmse_data <- data.frame(sqrt(rowMeans(error^2))) %>%
    rename(
      RMSE = "sqrt.rowMeans.error.2.."
    )

  # Histogram of Bayesian RMSE
  rmse_plot <-
    ggplot(data = bayes_rmse_data,aes(x = RMSE))+
    geom_histogram(fill = "lightblue")+
    labs(
      x = "RMSE",
      y = "Count",
      title = "Posterior RMSE Distribution"
    )+
    theme_minimal()


  # List with error measurements
  error_measurements <- list(
    error_data = error,
    rmse_data = bayes_rmse_data,
    rmse_distribution = rmse_plot
  )

  ## Measures of LPD

  ## Compute Loo
  loo_general <- loo(model,save_psis = TRUE)

  # Return
  return(list(
    explained_variance = r2,
    error_measurements = error_measurements,
    general_loo = loo_general
  ))
}
