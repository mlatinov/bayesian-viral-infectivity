
#### Function to Check Bayes Models ####
bayes_diagnostics <- function(model){

  #### Libraries ####
  library(brms)
  library(bayesplot)
  library(priorsense)
  library(loo)

  #### Convergent Diagnostics ####

  ## Trace Plots ##
  trace_plot <- mcmc_plot(model,type = "trace")+
    ggtitle("MCMC Trace Chains plot")+
    theme_minimal()

  ## Autocorrection plot ##
  auto_c <- mcmc_acf(model)+
    ggtitle("MCMC Chains Autocorrection plot")+
    theme_minimal()

  ## Effect Sample Size ##
  eff <- mcmc_neff(neff_ratio(model))+
    ggtitle("MCMC Chains Effective Sample Size Ratio")+
    theme_minimal()

  # Collect Convergent Diagnostics in a list
  convergence_diagnostics <- list(
    trace_plot = trace_plot,
    autocorrelation_chain  = auto_c,
    effective_sample_ratio = eff
  )

  #### Posterior Predictive Checks ####

  ## Dens overlay ##
  pp_dens <- pp_check(model,type = "dens_overlay")+
    ggtitle("Posterior Predictive Density Overlayed by the sample")+
    theme_minimal()

  ## Stat ##
  pp_stat <- pp_check(model,type = "stat")+
    ggtitle("Posterior Predictive Density Histogram")+
    theme_minimal()

  # Collect Predictive Checks in a list
  posterior_predictive_checks <- list(
    posterior_density = pp_dens,
    posteror_stat = pp_stat
  )

  #### PRIOR Sensitivity Analysis ####

  ## Prior Sensitivity table ##
  sens_table <- powerscale_sensitivity(model)

  ## Sensitivity under prior perturbations ##
  sens_density <- powerscale_plot_dens(model)+
    theme_minimal()

  ## Probability density shifts under prior perturbations ##
  sens_shifts <- powerscale_plot_ecdf(model)+
    theme_minimal()

  # Collect sensitivity in a list
  prior_sensitivity <- list(
    summary_table = sens_table,
    posterior_density = sens_density,
    posterior_shifts = sens_shifts
  )

  #### Model Comparison ####

  ## Measure of Explained Variance R2

  # Collect the R2 Data
  r2_distribution_data <- as.data.frame(
    bayes_R2(bernoulli_bayes_model,summary = FALSE))

  # Plot R2 Histogram
  r2_histogram <-
    ggplot(aes(x = R2))+
    geom_histogram(fill = "lightblue")+
    theme_minimal()+
    labs(
      title = "Posterior Bayesian R2 Distribution",
      x = "R2",
      y = "Count"
    )

  ## Measure of Errors

  # Collect the Posterior Expected  Errors
  error <- predictive_error(bernoulli_bayes_model,method = "posterior_epred")

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


  ## Measures of LPD

  ## Compute Loo
  loo_model <- loo(
    model,
    save_psis = TRUE
    )

  y <- as.numeric(model$data$outcome)
  yrep <- posterior_predict(model, draws = 2000)

  ## Loo QQ Plot
  loo_pit_qq <- ppc_loo_pit_qq(
    y = y,
    yrep = yrep,
    psis_object = loo_model$psis_object
  ) +
    theme_minimal() +
    labs(
      title = "LOO-PIT Q–Q Plot",
      subtitle = "Checks calibration for discrete outcomes",
      x = "Theoretical PIT quantiles",
      y = "Empirical PIT quantiles"
    )

  # Return
  return(list(
    convergence_diagnostics = convergence_diagnostics,
    posterior_predictive_checks = posterior_predictive_checks,
    prior_sensitivity = prior_sensitivity,
    loo_pit_qq = loo_pit_qq,
    psis_plot = plot(loo_model),
    bayes_r2 = list(r2_data = r2_distribution_data,r2_plot = r2_histogram)
  ))
}
