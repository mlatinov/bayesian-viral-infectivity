
#### Function to clean the raw experimental data ####
clean_data_f <- function(data){

 # CLean and save the data
  data <- data %>%

    # Pivot the data creating a repetition and value columns
    pivot_longer(
      cols = -Concentration,
      names_to = "repetition",
      values_to = "value"
      ) %>%

    # Rename Concentration
    rename(
      virus_c = Concentration
    ) %>%
    # Change the data types and convert the concentration to log scale
    mutate(
      virus_c = log10(virus_c),
      repetition = as.factor(repetition),
      value = as.factor(value)
    )

  # Return the cleaned data
  return(data)
}
