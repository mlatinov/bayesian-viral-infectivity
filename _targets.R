
#### Global Libraries ####
library(targets)
library(tarchetypes)
library(tidyverse)

#### Source Function ####
tar_source("scripts/")


#### Targets Pipeline ####
list(

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

  ## Inspect Simulation model ##
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
      lapply(
        list(bernoulli_bayes_model,bernoulli_bayes_model_v2),
        compare_bayes_models
        )
    ),

  #### Render Reports ####

  ## Combine everything into one tibble
  tar_target(
    name = report_data,
    command = tibble(
      name = c("Basic_model","Full_model"),
      diagnostics = list(
        bernoulli_model_diagnostics,
        bernoulli_model_diagnostics_2
        ),
      insights = list(
        bernoulli_model_insights,
        bernoulli_model_insights_2
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


