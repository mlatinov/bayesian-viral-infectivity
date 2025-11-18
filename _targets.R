
#### Global Libraries ####
library(targets)
library(tarchetypes)
library(tidyverse)

#### Source Function ####
tar_source("scripts/")


#### Targets Pipeline ####
list(

  #### Simulate Viruses ####

  # Simulate Highly Infection Rate Virus even in high dilutions
  tar_target(
    name = virus_A,
    command = simulate_virus(
      baseline_infectivity_mean = 3, # baseline probability ~ 0.95
      baseline_infectivity_sd = 3,
      beta_coef_mean = 0.5, # dilution does not reduce infection rates much
      beta_coef_sd = 3
        )
    ),

  # Simulate Low Infection Rate Virus even in low dilutions
  tar_target(
    name = virus_B,
    command = simulate_virus(
      baseline_infectivity_mean = 0,  # baseline probability ~ 0.50
      baseline_infectivity_sd = 3,
      beta_coef_mean = 1 , # dilution does reduce infection rates
      beta_coef_sd = 3
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

  ## Check Generated data
  tar_target(
    name = simulation_data_check,
    command = lappy(simulation_virus_AB,check_generated_virus)
  ),

  #### Load the Experimental Data ####
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
      as.data.frame(clean_experimental_data[[1]])
      )
  ),

  #### Classic GLM ####
  tar_target(
    name = glm_model,
    command = glm_modeling_f(
      data = as.data.frame(clean_experimental_data[[1]])
      )
  ),

  #### Bayes Models #####

  #### Bernoulli Bayes Model Simulation Virus A ####
  tar_target(
    name = bernoulli_bayes_model_sim_virus_A,
    command = bernoulli_bayes(
      data = virus_A,
      priors = c(
        # For each increase of log(virus_dilution) the chance of virus infection drops by
        prior(normal(0, 3), class = "b", coef = "virus_dilution"),
        # When virus_dilution = 0 The log odds of virus infection
        prior(normal(2,2),class = "Intercept")
      )
    )
  ),

  #### Bernoulli Bayes Model Simulation Virus A ####
  tar_target(
    name = bernoulli_bayes_model_sim_virus_B,
    command = bernoulli_bayes(
      data = virus_B,
      priors = c(
        # For each increase of log(virus_dilution) the chance of virus infection drops by
        prior(normal(0, 3), class = "b", coef = "virus_dilution"),
        # When virus_dilution = 0 The log odds of virus infection
        prior(normal(2,2),class = "Intercept")
      )
    )
  ),

  ### Prior Predictive Simulation ###
  tar_target(
    name = pp_sim_bernoulli,
    command = pp_simualtion_bernoulli(
      data = as.data.frame(clean_experimental_data[[1]]),
      priors = c(
        # For each increase of log(virus_dilution) the chance of virus infection drops
        prior(normal(0, 3), class = "b", coef = "virus_dilution"),
        # When virus_dilution = 0 The log odds of virus infection
        prior(normal(2,2),class = "Intercept")
      ))
  ),

  ## Inspect Prior Predictive Simulation model ##
  tar_target(
    name = pp_simualtion_inspections,
    command = inspect_simulation(pp_sim_bernoulli)
  ),

  #### Bernoulli Bayes Model with Experimental data 1 ####
  tar_target(
    name = bernoulli_bayes_model,
    command = bernoulli_bayes(
      data = as.data.frame(clean_experimental_data[[1]]),
      priors = c(
        # For each increase of log(virus_dilution) the chance of virus infection drops by
        prior(normal(0, 3), class = "b", coef = "virus_dilution"),
        # When virus_dilution = 0 The log odds of virus infection
        prior(normal(2,2),class = "Intercept")
        )
      )
  ),

  #### Bernoulli Bayes Model with Combined datasets ####
  tar_target(
    name = bernoulli_bayes_model_v2,
    command = bernoulli_bayes(
      data = data_combined,
      priors = c(
        # For each increase of log(virus_dilution) the chance of virus infection drops by
        prior(normal(0, 3), class = "b", coef = "virus_dilution"),
        # When virus_dilution = 0 The log odds of virus infection
        prior(normal(2,2),class = "Intercept")
        )
      )
  ),

  ## Model Diagnostics for Bayes Model trained on Simulation Virus A  ##
  tar_target(
    name = bernoulli_model_diagnostics_sim_virus_A,
    command = bayes_diagnostics(bernoulli_bayes_model_sim_virus_A)
  ),

  ## Model Diagnostics for Bayes Model trained on Simulation Virus B ##
  tar_target(
    name = bernoulli_model_diagnostics_sim_virus_B,
    command = bayes_diagnostics(bernoulli_bayes_model_sim_virus_B)
  ),

  ## Model Diagnostics for Bayes Model trained on Experimental data 1 ##
  tar_target(
    name = bernoulli_model_diagnostics,
    command = bayes_diagnostics(bernoulli_bayes_model)
  ),

  ## Model Diagnostics for Bayes Model trained Combined data ##
  tar_target(
    name = bernoulli_model_diagnostics_2,
    command = bayes_diagnostics(bernoulli_bayes_model_v2)
  ),

  ## Model Insights for Bayes Model trained on Simulation Virus A  ##
  tar_target(
    name = bernoulli_model_insights_sim_virus_A,
    command = bayes_insights(
      model = bernoulli_bayes_model_sim_virus_A,
      data = virus_A
    )
  ),

  ## Model Insights for Bayes Model trained on Simulation Virus B  ##
  tar_target(
    name = bernoulli_model_insights_sim_virus_B,
    command = bayes_insights(
      model = bernoulli_bayes_model_sim_virus_B,
      data = virus_B
    )
  ),

  ## Model Insights for Bayes Model trained on Experimental data 1 ##
  tar_target(
    name = bernoulli_model_insights,
    command = bayes_insights(
      model = bernoulli_bayes_model,
      data = as.data.frame(clean_experimental_data[[1]])
      )
  ),

  ## Model Insights for Bayes Model trained Combined data  ##
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
      lapply(list(bernoulli_bayes_model,bernoulli_bayes_model_v2),compare_bayes_models)
    ),

  #### Render Reports ####

  ## Combine everything into one tibble
  tar_target(
    name = report_data,
    command = tibble(
      name = c("Basic_model","Full_model","Simulation_A","Simulation_B"),
      diagnostics = list(
        bernoulli_model_diagnostics,
        bernoulli_model_diagnostics_2,
        bernoulli_model_diagnostics_sim_virus_A,
        bernoulli_model_diagnostics_sim_virus_B
        ),
      insights = list(
        bernoulli_model_insights,
        bernoulli_model_insights_2,
        bernoulli_model_insights_sim_virus_A,
        bernoulli_model_insights_sim_virus_B
      ),
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


