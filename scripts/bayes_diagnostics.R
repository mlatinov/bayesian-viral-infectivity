
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

  # Return
  return(list(
    convergence_diagnostics = convergence_diagnostics,
    posterior_predictive_checks = posterior_predictive_checks,
    prior_sensitivity = prior_sensitivity
    ))
}
