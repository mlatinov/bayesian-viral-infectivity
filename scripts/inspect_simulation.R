
#### Function to Check the Prior Predictive Simulation ####
inspect_simulation <- function(simulation) {

  #### Libraries  ####
  library(brms)

  #### Prior Predictive Checks ####
  error_binned <-
  pp_check(simulation, type = "error_binned")+
    theme_minimal()+
    ggtitle("Prior Predictive Simulation : Error Binded")

  dens_overlay <-
  pp_check(simulation,type = "dens_overlay")+
  theme_minimal()+
    ggtitle("Prior Predictive Simulation :Density plot")

  # Return
  return(list(
    error_binned_plot = error_binned,
    dens_overlay_plot = dens_overlay
  ))
}
