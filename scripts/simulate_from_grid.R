
### Function to simulate all viruses from Simulation Grid ####
simulate_from_grid <- function(simulation_grid,n,n_draws){

  # Loop over the parameters of the simulation and simulation data for every row
  simulation_list <- pmap(
    simulation_grid,
    function(baseline_infectivity_mean, beta_coef_mean, baseline_infectivity_sd, beta_coef_sd, ...) {
      simulate_virus(
        baseline_infectivity_mean = baseline_infectivity_mean,
        baseline_infectivity_sd   = baseline_infectivity_sd,
        beta_coef_mean            = beta_coef_mean,
        beta_coef_sd              = beta_coef_sd,
        n = n,
        n_draws = n_draws
      )
    }
  )

  # Name the list elements
  names(simulation_list) <- letters[2:11]

  # Return data
  return(simulation_list)

}
