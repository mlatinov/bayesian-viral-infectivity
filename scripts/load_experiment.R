
#### Function to load Experimental datasets ####
load_experiment <- function(){

  # Experimental data 1
  experiment_1 <- read_csv("data/Virus_herpes_Exp1 - Virus_data_1.csv")

  # Experimental data 2
  experiment_2 <- read.csv("data/Virus_herpes_Exp1 - Virus_data_2.csv")

  # Return list with all datasets
  return(list(
    experiment_1 = experiment_1,
    experiment_2 = experiment_2
    ))
}
