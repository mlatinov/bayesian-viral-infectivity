
#### Function for SBC on the Combined Model ####
sbc_combined <- function(brms_generator,n_sim = 100){

  #### Libraries ####
  library(SBC)

  # Generate Datasets
  simulated_datasets <- generate_datasets(generator = brms_generator, n_sims = n_sim)

  # SBC backend
  backend <- SBC_backend_brms_from_generator(
    generator = brms_generator,
    chains = 4,
    iter = 4000,
    warmup = 2000
    )

  #### Compute SBC ####
  sbc <- compute_SBC(datasets = simulated_datasets, backend = backend)

  #### Discovering bad parametrization ####

  ## ECDF
  ecdf <- plot_ecdf(sbc)+
    theme_minimal()+
    labs(
      title = "Sampled ECDF Vs Theoretical CDF "
    )

  ## Coverage
  coverage <- plot_coverage(sbc)+
    theme_minimal()+
    labs(
      title = "Empirical coverage"
    )

  # Return
  return(list(
    ecdf = ecdf,
    empirical_coverage = coverage
  ))

}
