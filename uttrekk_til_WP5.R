K:\FAG\Tverrfaglig\AMR\FoU-aktiviteter & prosjekter\31218_INIKA_OH_TZ\WP3\outputDataset


inputFilbane <- "K:\\FAG\\Tverrfaglig\\AMR\\FoU-aktiviteter & prosjekter\\31218_INIKA_OH_TZ\\WP3\\inputDataset\\" 
outputFilbane <- "K:\\FAG\\Tverrfaglig\\AMR\\FoU-aktiviteter & prosjekter\\31218_INIKA_OH_TZ\\WP3\\outputDataset\\" 
outputFilbane <- "C:\\Users\\15NORSTR\\Documents\\"

C:\Users\15NORSTR\Documents

Mwanza<-QUEST%>%
  select(INIKA_OH_TZ_ID,Respondent,Region, Region_2, District, District_2, Ward, Ward_2 )%>%
  filter(Region=="Mwanza")

Ilemala<-QUEST%>%
  filter(District=="Ilemela")


Magu<-QUEST%>%
  filter(District=="Magu")


Nyamagana<-QUEST%>%
  filter(District=="Nyamagana")



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
#frequencies <- get_frequencies(INIKA_SURVEY_ID)
frequencies<-get_frequencies(Mwanza)

Ward<- table(Mwanza$"Ward")
  print(Ward_2)
  
  # Load the necessary library
  library(dplyr)
  
  # Sample data
  data <- data.frame(ID = c(1, 2, 2, 3, 3, 4, 5, 5),
                     n = c(10, 20, 20, 30, 30, 40, 50, 50))
  
  # Count the frequency of 'n' column and group by 'ID'
  data <- data %>%
    group_by(ID, n) %>%
    summarise(frequency = n())
  
  # Identify and mark doublets with a new variable 'X'
  data <- Mwanza%>%
    mutate(n=1)%>%
    group_by(Ward, District)%>%
    summarize(frequenzy= n())
 
 

write.csv(data,file = paste(outputFilbane,"Mwanza.csv", sep = ""))

