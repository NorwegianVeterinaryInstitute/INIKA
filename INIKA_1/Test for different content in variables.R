

get_frequencies <- function(df) {
  freq_list <- lapply(df, function(col) {
    freq_table <- as.data.frame(table(col, useNA = "ifany"))
    colnames(freq_table) <- c("Value", "Frequency")
    return(freq_table)
  })
  names(freq_list) <- colnames(df)
  return(freq_list)
}

# Apply the function to the dataset
frequencies <- get_frequencies(INIKA_SURVEY_ID)

# Combine the frequencies into a single dataframe
combined_frequencies <- bind_rows(
  lapply(names(frequencies), function(colname) {
    freq_table <- frequencies[[colname]]
    freq_table <- mutate(freq_table, Column = colname)
  }),
  .id = "id"
)

frequencies <- get_frequencies(INIKA_SURVEY_ID)

# Combine the frequencies into a single dataframe
combined_frequencies <- bind_rows(
  lapply(names(frequencies), function(colname) {
    freq_table <- frequencies[[colname]]
    freq_table <- mutate(freq_table, Column = colname)
  }),
  .id = "id"
)




write.csv(combined_frequencies, file = paste(outputFilbane,"Resultat\\","COMBINED_2.csv", sep = ""))
