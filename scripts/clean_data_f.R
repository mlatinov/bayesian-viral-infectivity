
#### Function to clean the raw experimental data ####
clean_data_f <- function(data){

 # CLean and save the data
  data <- data %>%

    # Pivot the data creating a repetition and outcome columns
    pivot_longer(
      cols = -Concentration,
      names_to = "repetition",
      values_to = "outcome"
      ) %>%

    # Rename Concentration
    rename(
      virus_dilution = Concentration
    ) %>%
    # Change the data types and convert the concentration to log scale
    mutate(
      virus_dilution = log10(virus_dilution),
      repetition = as.factor(repetition),
      outcome = as.factor(outcome)
    )

  # Return the cleaned data
  return(data)
}
