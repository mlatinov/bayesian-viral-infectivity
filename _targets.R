
#### Global Libraries ####
library(targets)
library(tarchetypes)
library(tidyverse)

#### Source Function ####
tar_source("scripts/")


#### Targets Pipeline ####
list(

  #### Simulate Viruses ####

  # Simulate High Infection Rate Virus even in high dilutions
  tar_target(
    name = virus_A,
    command = simulate_virus(
      baseline_infectivity_mean = 5, # baseline probability
      baseline_infectivity_sd = 0.5,
      beta_coef_mean = 1.2, # dilution does not reduce infection rates much
      beta_coef_sd = 0.15
        )
    ),

  # Simulate Low Infection Rate Virus even in low dilutions
  tar_target(
    name = virus_B,
    command = simulate_virus(
      baseline_infectivity_mean = 4.7, # baseline probability
      baseline_infectivity_sd = 0.5,
      beta_coef_mean = 1.8 , # dilution does reduce infection rates
      beta_coef_sd = 0.15
    )
  ),

  # Simulate High Infection Rate Virus effected by imaginary variable Gamma
  tar_target(
    name = virus_G,
    command = simulate_virus_v2(
      baseline_infectivity_mean = 5, # baseline probability
      baseline_infectivity_sd = 0.5,
      beta_coef_mean = 1.2,   # dilution does not reduce infection rates much
      beta_coef_sd = 0.15,
      gamma_coef_mean = -0.5, # Imaginary variable gamma reduces the infection rates
      gamma_coef_sd = 0.15
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

  ## Combine the datasets in a list ##
  tar_target(
    name = simulation_virus_AB,
    command = list(
      virus_A = virus_A,
      virus_B = virus_B
    )
  ),

  #### Check Generated data ####

  # Check the AB Viruses
  tar_target(
    name = simulation_data_check_AB,
    command = lapply(simulation_virus_AB,check_generated_virus)
  ),

  # Check the imaginary G Virus
  tar_target(
    name = simulation_data_check_G,
    command = check_generated_virus(virus_G)
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

  # Bernoulli Bayes Model Simulation Virus AB #
  tar_target(
    name = bernoulli_bayes_model_sim_virus_AB,
    command = lapply(
      simulation_virus_AB,
      function(df) {
        bernoulli_bayes(
          data   = df,
          priors = c(
            prior(normal(0,3), class = "b", coef = "virus_dilution"),
            prior(normal(2,2), class = "Intercept")
          )
        )
      }
    )
  ),

  # Bernoulli Bayes Model Simulation Virus G #
  tar_target(
    name = bernoulli_bayes_model_sim_virus_G,
    command = bernoulli_bayes_G(
      data = virus_G,
      priors = c(
        prior(normal(0,3), class = "b", coef = "virus_dilution"),
        prior(normal(0,3), class = "b", coef = "imaginary_var"),
        prior(normal(2,2), class = "Intercept")
      )
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
        prior(normal(0,3), class = "b", coef = "virus_dilution"),
        # When virus_dilution = 0 The log odds of virus infection
        prior(normal(2,2),class = "Intercept")
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
    command = sbc_combined()
  ),

  # SBC For Hierarchical Bayes Model
  tar_target(
    name = sbc_bernoulli_hierarchical,
    command = sbc_hierarchical()
  ),

  #### Full Bayesian Models ####

  # Bernoulli Bayes Model with Experimental data 1 #
  tar_target(
    name = bernoulli_bayes_model,
    command = bernoulli_bayes(
      data = as.data.frame(clean_experimental_data[[1]]),
      priors = c(
        # For each increase of log(virus_dilution) the chance of virus infection drops by
        prior(normal(0,3), class = "b", coef = "virus_dilution"),
        # When virus_dilution = 0 The log odds of virus infection
        prior(normal(2,2),class = "Intercept")
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
        prior(normal(0,3), class = "b", coef = "virus_dilution"),
        # When virus_dilution = 0 The log odds of virus infection
        prior(normal(2,2),class = "Intercept")
        )
      )
  ),

  # Bernoulli Hierarchical Model for the Different Experiments #
  tar_target(
    name = bernoulli_hierarchical_model,
    command = bernoulli_hierarchical(
      data = data_combined,
      priors = c(
        prior(normal(0,3), class = "b"),                   # slope coefficients
        prior(normal(2,2), class = "Intercept"),           # population intercept
        prior(exponential(2), class = "sd"),               # group-level SD
        prior(lkj(0.5), class = "cor", group = "experiment")  # correlation
      )
    )
  ),

  # Append All the models in one list #
  tar_target(
    name = all_models,
    command = append(
      bernoulli_bayes_model_sim_virus_AB,
      list(
        hierarchical_model = bernoulli_hierarchical_model,
        virus_G = bernoulli_bayes_model_sim_virus_G,
        experimental = bernoulli_bayes_model,
        experimental_full = bernoulli_bayes_model_v2
      ))
  ),

  #### Model Diagnostics for Real Bayes Models  ####
  tar_target(
    name = bernoulli_model_diagnostics,
    command = lapply(all_models,bayes_diagnostics)
  ),

  #### Model Insights ####

  # Model Insights for Bayes Model trained on Simulation Virus AB ##
  tar_target(
    name = bernoulli_model_insights_sim_virus_AB,
    mapply(
      function(model, data) {
        bayes_insights(
          model = model,
          data  = data
        )
      },
      model = bernoulli_bayes_model_sim_virus_AB,
      data  = simulation_virus_AB,
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

  #### Render Reports ####

  ## Combine everything into one list
  tar_target(
    name = report_data,
    command = list(
      # Data Simulation checks
      data_simulation_checks = list(
        virusAB = simulation_data_check_AB,
        virus_G = simulation_data_check_G,
        simulation_data_check_hierarchical
      ),
      # Prior Predictive Simulations for the Real Models
      prior_simulations = list(
        prior_simulations = pp_simualtion_inspections),
      # Models Diagnostics
      diagnostics = list(
        bernoulli_model_diagnostics
        ),
      # Models insights
      insights = list(
        bernoulli_model_insights,
        bernoulli_model_insights_2,
        bernoulli_model_insights_sim_virus_AB
      ),
      # Model Comparisons
      comparison = list(model_compare)
    )
  ),

  ## Report summarizing the results
  tar_render(
    end_report,
    path = "documents/end_report.Rmd",
    output_file = "end_report.html",
    params = list(data = report_data)
  )
)
