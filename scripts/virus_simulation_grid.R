
#### Function to make virus simulation grid and produce the results from it ####
optimal_simulation <- function(n){

  #### Libraries ####
  library(DiceDesign)
  library(ggvoronoi)
  library(pcaPP)

  ## Create a simulation desing ####

  # Create a LHC Design
  lhs <- lhsDesign(
    n = n,
    dimension = 4
  )

  # Optimize the grid Min Max Optimization
  optim_grid <- maximinSA_LHS(
    design = lhs$design,
    T0 = 10, # The Initial Temperature of the SA
    c = 0.99, # Cool down
    it = 2000
    )

  # Map the Design to the parameters
  lhs_scaled <- data.frame(
    baseline_infectivity_mean = optim_grid$design[,1] * 16,
    beta_coef_mean = optim_grid$design[,2] * 5,
    baseline_infectivity_sd = optim_grid$design[,3] * 1,
    beta_coef_sd = optim_grid$design[,4] * 1
  )

  # PCA to reduce the dim
  pca_lhc <- prcomp(lhs_scaled,scale. = TRUE)

  # Extract PC scores
  pca_df <- as.data.frame(pca_lhc$x) %>%
    select(PC1,PC2)

  # Calculate the distance between each point
  dist <- pca_df %>% as.matrix() %>% dist() %>% as.matrix()

  # Compute min distance
  min_dist <- apply(dist, 1, function(x) min(x[x > 0]))

  # Add to the data
  pca_df$dist <- min_dist


  # Plot a Voronoi Diagram
  voronoi_diagram <-
    ggplot(pca_df, aes(x = PC1, y = PC2, fill = dist)) +
    geom_voronoi() +
    stat_voronoi(geom = "path")+
    geom_point()+
    scale_fill_gradient(
      low = "#F9F9F9",
      high = "#312271"
    )+
    theme_minimal()+
    labs(
      title = "Virus Simulation Grid Voronoi Diagram",
      fill = "Distance"
    )

  # Return
  return(list(
    simulation_grid = lhs_scaled,
    voronoi_diagram = voronoi_diagram
  ))
}
