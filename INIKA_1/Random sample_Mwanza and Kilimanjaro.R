

inputFilbane <- "K:\\FAG\\Tverrfaglig\\AMR\\FoU-aktiviteter & prosjekter\\31218_INIKA_OH_TZ\\WP3\\inputDataset\\" 
outputFilbane <- "K:\\FAG\\Tverrfaglig\\AMR\\FoU-aktiviteter & prosjekter\\31218_INIKA_OH_TZ\\WP3\\outputDataset\\" 



DATA <-
  read.table(
    paste(inputFilbane, "MWANZA_BROILER FARMS.csv", sep = ""),
    sep = ";",
    stringsAsFactors = F,
    header = TRUE
  )
# Assuming your data frame is named 'DATA'

# Select IDs where To.BE.selected.random = 1
ids_to_select_from <- DATA$ID[DATA$To.BE.selected.random == 1]

# Exclude IDs already selected
ids_already_selected <- DATA$ID[DATA$TO.INCLUDE == 1]

# Exclude IDs already selected from the ones to select from
ids_to_select_from <- setdiff(ids_to_select_from, ids_already_selected)

# Randomly sample 24 additional IDs
additional_ids <- sample(ids_to_select_from, 61, replace = FALSE)

# Mark the additional IDs as included
DATA$TO.INCLUDE[DATA$ID %in% additional_ids] <- 1

# Create a new column FIRST.CHOICE and mark selected IDs
DATA$FIRST.CHOICE <- ifelse(DATA$TO.INCLUDE == 1 | DATA$ID %in% additional_ids, 1, NA)

# Randomly shuffle the IDs not selected in To.BE.selected.random
ids_not_selected <- DATA$ID[!(DATA$ID %in% c(ids_already_selected, additional_ids))]
ids_not_selected <- sample(ids_not_selected)

# Assign ranks to the randomly shuffled IDs
rank_values <- 1:length(ids_not_selected)
rank_column <- rep(NA, nrow(DATA))
rank_column[DATA$ID %in% ids_not_selected] <- rank_values

DATA <- cbind( DATA, Rank = rank_column)

write.csv2(DATA, paste(outputFilbane,"MWANZA_SELECTION.csv",sep=""))

# Next random selection is from Kilimanjaro


DATA <-
  read.table(
    paste(inputFilbane, "KILIMANJARO_FARMS.csv", sep = ""),
    sep = ";",
    stringsAsFactors = F,
    header = TRUE
  )

your_dataset <-KILIMANJARO_FARMS

library(dplyr)

# Assuming your dataset is named 'your_dataset' and the column names are 'SN' and 'Not.to.be.included'

# Selecting 120 random IDs that are not marked as 'X' in the 'Not.to.be.included' column
first_choice <- your_dataset %>%
  filter(Not.to.be.included != "X") %>%
  sample_n(120, replace = FALSE) %>%
  mutate(First.choice = 1)

# Remaining IDs after selecting the first choice
remaining_ids <- setdiff(your_dataset$ID, first_choice$ID)

# Selecting 30 random reserve IDs from the remaining IDs
reserve <- your_dataset %>%
  filter(Not.to.be.included != "X" & !ID %in% first_choice$ID) %>%
  sample_n(30, replace = FALSE)

# Adding columns 'Second' and 'Third' and marking them as reserves
reserve <- reserve %>%
  mutate(Second = 1)

# Combining the selected IDs and reserves
final_selection <- bind_rows(first_choice, reserve)

reserve_2 <- your_dataset %>%
  filter(Not.to.be.included != "X" & !ID %in% first_choice$ID & !ID %in% reserve$ID) %>%
  sample_n(30, replace = FALSE)

# Adding columns 'Second' and 'Third' and marking them as reserves
reserve_2 <- reserve_2 %>%
  mutate(Third = 1)

# Combining the selected IDs and reserves
final_selection <- bind_rows(final_selection, reserve_2)


write.csv2(final_selection, paste(outputFilbane,"Kilimanjaro_SELECTION.csv",sep=""))
