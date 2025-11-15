
#### Global Libraries ####
library(targets)
library(tidyverse)


#### Source Function ####
tar_source("scripts/")


#### Targets Pipeline ####
list(

  #### Load the Experimental Data ####
  tar_target(
    name  = data_raw,
    command =  read_csv("data/Virus_herpes_Exp1 - Virus_data_1.csv")
  ),

  #### Clean the Raw Data ####
  tar_target(
    name = data_clean,
    command = clean_data_f(data_raw)
  ),

  #### Classic GLM ####
  tar_target(
    name = glm_model,
    command = glm_modeling_f(data_clean)
  ),

  #### Bayes Models #####

  ### Prior Predictive Simulation ###
  tar_target(
    name = pp_sim_bernoulli,
    command = pp_simualtion_bernoulli(
      data = data_clean,
      priors = c(
        # For each increase of log(virus_c) the chance of virus infection drops by 15-20%
        prior(normal(-0.5, 5), class = "b", coef = "virus_c"),
        # When virus_c = 0 The log odds of virus infection is 4.5 or 90%
        prior(normal(2,2),class = "Intercept")
      ))
  ),

  ## Inspect Simulation model ##
  tar_target(
    name = pp_simualtion_inspections,
    command = inspect_simulation(pp_sim_bernoulli)
  ),

  #### Bernoulli Bayes Model ####
  tar_target(
    name = bernoulli_bayes_model,
    command = bernoulli_bayes(
      data = data_clean,
      priors = c(
        # For each increase of log(virus_c) the chance of virus infection drops by 15-20%
        prior(normal(-0.5, 3), class = "b", coef = "virus_c"),
        # When virus_c = 0 The log odds of virus infection is 2 or 90%
        prior(normal(2,2),class = "Intercept")
      ))
  ),

  ## Model Diagnostics ##
  tar_target(
    name = bernoulli_model_diagnostics,
    command = bayes_diagnostics(bernoulli_bayes_model)
  )

)
