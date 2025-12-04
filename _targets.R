
#### Global Libraries ####
library(targets)
library(tidyverse)

#### Source Function ####
tar_source("scripts/")


#### Targets Pipeline ####
list(

  #### Simulate Viruses ####

  # Scenario 1 Simulate High Infection Rate Virus
  tar_target(
    name = virus_high_infection,
    command = simulate_virus(
      baseline_infectivity_mean = 5, # baseline probability
      baseline_infectivity_sd = 0.5,
      beta_coef_mean = 1.2,
      beta_coef_sd = 0.15,
      n = 200,
      n_draws = 10
      )
    ),

  # Create a Simulation Grid #
  tar_target(
    name = virus_simulation_grid,
    command = optimal_simulation(n = 10) # Number of simulation
  ),

  # Simulate 10 Viruses from a Simulation Grid #
  tar_target(
    name = simulation_viruses,
    command = simulate_from_grid(
      virus_simulation_grid$simulation_grid,
      n = 100, # Number of simulations per draw
      n_draws = 10 # Number of draws
      )
  ),

  # Simulate data for Hierarchical Virus Model with Multiple Repeated Experiments
  tar_target(
    name = sim_data_hierarchical,
    command = simulate_virus_hierarchical(
      baseline_infectivity_mean = 5,
      baseline_infectivity_sd = 0.5,
      beta_coef_mean = 1.2 ,
      beta_coef_sd = 0.15,
      rho = 0.8,                   # correlation between intercepts and slopes
      n_experiments = 5,           # number of experiments
      n_per_experiment = 500       # number of dilution points per experiment
    )
  ),

  #### Check Generated data ####

  # Check the Simulation of  Viruses
  tar_target(
    name = simulation_data_check_viruses,
    command = lapply(simulation_viruses,check_generated_virus)
  ),

  # Check Hierarchical Virus Data
  tar_target(
    name = simulation_data_check_hierarchical,
    command = check_generated_virus(sim_data_hierarchical)
  ),

  #### Load the Experimental Datasets ####
  tar_target(
    name = experimental_data,
    command = load_experiment()
  ),

  #### Clean the Raw Data ####
  tar_target(
    name = clean_experimental_data,
    command = lapply(experimental_data, clean_data_f)
  ),

  ## Combine the clean datasets
  tar_target(
    name = data_combined,
    command = bind_rows(
      as.data.frame(clean_experimental_data[[1]]),
      as.data.frame(clean_experimental_data[[1]]),
      .id = "experiment"
      ) %>%
      mutate(
        experiment = as.factor(experiment)
      )
  ),

  #### Bayes Models Parameter Recovery #####

  # Bernoulli Bayes Model Simulation Virus #
  tar_target(
    name = bernoulli_bayes_model_sim_viruses,
    command = lapply(
      simulation_viruses,
      function(df) {
        bernoulli_bayes(
          data   = df,
          priors = c(
            prior(normal(2,1), class = "b", coef = "virus_dilution"),
            prior(normal(4,2), class = "Intercept")
          )
        )
      }
    )
  ),

  # Bernoulli Bayes Model for Hierarchical Model
  tar_target(
    name = bernoulli_sim_hierarchical,
    command = bernoulli_hierarchical(
      data = sim_data_hierarchical,
      priors = c(
        prior(normal(0,3), class = "b"),                   # slope coefficients
        prior(normal(2,2), class = "Intercept"),           # population intercept
        prior(exponential(2), class = "sd"),               # group-level SD
        prior(lkj(0.5), class = "cor", group = "experiment")  # correlation
      )
    )
  ),

  #### Prior Predictive Simulations for the Real Models ####

  # Prior Predictive Simulation for the Experimental Data  #
  tar_target(
    name = pp_sim_bernoulli,
    command = pp_simualtion_bernoulli(
      data = as.data.frame(clean_experimental_data[[1]]),
      priors = c(
        # For each increase of log(virus_dilution) the chance of virus infection drops
        prior(normal(2,1), class = "b", coef = "virus_dilution"),
        # When virus_dilution = 0 The log odds of virus infection
        prior(normal(4,2),class = "Intercept")
      ))
  ),

  # Prior Predictive Simulation on the Combined Data for Hierarchical Model
  tar_target(
    name = pp_sim_bernoulli_hierarchical,
    command = pp_simulation_bernoulli_hierarchical(
      data = sim_data_hierarchical,
      priors = c(
        prior(normal(0,3), class = "b"),                   # slope coefficients
        prior(normal(2,2), class = "Intercept"),           # population intercept
        prior(exponential(2), class = "sd"),               # group-level SD
        prior(lkj(0.5), class = "cor", group = "experiment")  # correlation
        )
      )
    ),

  # Collect all PP Simulations in a list
  tar_target(
    name = pp_sim_list,
    command = list(
      pp_simulation_data_1 = pp_sim_bernoulli,
      pp_simulation_hierarchical = pp_sim_bernoulli_hierarchical
    )
  ),

  ## Inspect Prior Predictive Simulation models ##
  tar_target(
    name = pp_simualtion_inspections,
    command = lapply(pp_sim_list,inspect_simulation)
  ),

  #### Bayes Models Simulation Based Calibration #####

  # SBC For Combined Data Bayes Model
  tar_target(
    name = sbc_bernoulli_combined,
    command = sbc_combined(
      # BRMS Generator
      brms_generator =
        SBC_generator_brms(
          formula = outcome ~ virus_dilution,
          family = brms::bernoulli(link = "logit"),
          data = data_combined,
          prior = c(
            prior(normal(2,1), class = "b", coef = "virus_dilution"),
            prior(normal(4,2), class = "Intercept")
          )
        ),
      # Number of Simulations
      n_sim = 100
    ),
    packages = "brms"
  ),

  # SBC For Hierarchical Bayes Model
  tar_target(
    name = sbc_bernoulli_hierarchical,
    command = sbc_combined(
      # BRMS Generator
      brms_generator =
        SBC_generator_brms(
          formula =  outcome ~ virus_dilution + (virus_dilution | experiment ),
          family = brms::bernoulli(link = "logit"),
          data = data_combined,
          priors = c(
            prior(normal(0,0.7), class = "b"),                   # slope coefficients
            prior(normal(0,1.5), class = "Intercept"),           # population intercept
            prior(exponential(2), class = "sd"),                 # group-level SD
            prior(lkj(0.5), class = "cor", group = "experiment") # correlation
          )
        ),
      # Number of Simulations
      n_sim = 100
    ),
    packages = "brms"
  ),

  #### Full Bayesian Models ####

  # Bernoulli Bayes Model with Experimental data 1 #
  tar_target(
    name = bernoulli_bayes_model,
    command = bernoulli_bayes(
      data = as.data.frame(clean_experimental_data[[1]]),
      priors = c(
        prior(normal(2,1), class = "b", coef = "virus_dilution"),
        prior(normal(4,2), class = "Intercept")
        )
      )
  ),

  # Bernoulli Bayes Model with Combined Experimental datasets #
  tar_target(
    name = bernoulli_bayes_model_v2,
    command = bernoulli_bayes(
      data = data_combined,
      priors = c(
        # For each increase of log(virus_dilution) the chance of virus infection drops by
        prior(normal(2,1), class = "b", coef = "virus_dilution"),
        # When virus_dilution = 0 The log odds of virus infection
        prior(normal(4,2),class = "Intercept")
        )
      )
  ),

  # Bernoulli Hierarchical Model for the Different Experiments #
  tar_target(
    name = bernoulli_hierarchical_model,
    command = bernoulli_hierarchical(
      data = data_combined,
      priors = c(
        prior(normal(0,3), class = "b"),                      # slope coefficients
        prior(normal(2,2), class = "Intercept"),              # population intercept
        prior(exponential(2), class = "sd"),                  # group-level SD
        prior(lkj(0.5), class = "cor", group = "experiment")  # correlation
      )
    )
  ),

  #### Model Diagnostics Bayes Models  ####
  tar_target(
    name = bernoulli_model_diagnostics,
    command = bayes_diagnostics(bernoulli_bayes_model_v2)
  ),

  #### Model Insights ####

  # Model Insights for Bayes Model trained on Simulation Viruses ##
  tar_target(
    name = bernoulli_model_insights_sim_viruses,
    mapply(
      function(model, data) {
        bayes_insights(
          model = model,
          data  = data
        )
      },
      model = bernoulli_bayes_model_sim_viruses,
      data  = simulation_viruses,
      SIMPLIFY = FALSE
    )
  ),

  # Model Insights for Bayes Model trained on Experimental data 1 ##
  tar_target(
    name = bernoulli_model_insights,
    command = bayes_insights(
      model = bernoulli_bayes_model,
      data = as.data.frame(clean_experimental_data[[1]])
      )
  ),

  # Model Insights for Bayes Model trained Combined data  ##
  tar_target(
    name = bernoulli_model_insights_2,
    command = bayes_insights(
      model = bernoulli_bayes_model_v2,
      data = data_combined
      )
  ),

  #### Model Compare ####
  tar_target(
    name = model_compare,
    command =
      lapply(
        list(bernoulli_bayes_model,bernoulli_bayes_model_v2),
        compare_bayes_models
        )
    ),

  ## Combine All model Insights in a list
  tar_target(
    name = model_insight_list,
    command = list(
      full_model = bernoulli_model_insights_2,
      half_model = bernoulli_model_insights,
      scenario_model = bernoulli_model_insights_sim_viruses
    )
  ),

  #### Additional Combined model plots ####
  tar_target(
    name = extra_plots,
    command = extra_model_plots(
      model_insight_list = model_insight_list$scenario_model,
      model_list = bernoulli_bayes_model_sim_viruses
      )
  ),

  #### Report Data ####
  ## Combine everything into one list
  tar_target(
    name = report_data,
    command = list(
      ## Data Simulation checks
      data_simulation_checks = list(
        simulation_data_check_viruses,
        simulation_data_check_hierarchical
      ),
      ## Prior Predictive Simulations for the Real Models
      prior_simulations = list(
        prior_simulations = pp_simualtion_inspections),
      ## Models Diagnostics
      diagnostics = list(
        # General Diagnostics
        bernoulli_model_diagnostics,
        # SBC Protocol
        sbc_bernoulli_combined,
        sbc_bernoulli_hierarchical
        ),
      ## Models insights
      insights = list(
        bernoulli_model_insights,
        bernoulli_model_insights_2,
        bernoulli_model_insights_sim_viruses
      ),
      ## Model Comparisons
      comparison = list(model_compare),

      ## Extra plots
      extra_plots = extra_plots
    ),
  )
)

