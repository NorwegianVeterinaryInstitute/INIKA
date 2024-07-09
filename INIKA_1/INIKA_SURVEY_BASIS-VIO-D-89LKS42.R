

#Install needed libraries

install.packages(dplyr, tidyverse, readxl)
library(readxl)


library(dbplyr)
library(tidyverse)

inputFilbane <- "K:\\FAG\\Tverrfaglig\\AMR\\FoU-aktiviteter & prosjekter\\31218_INIKA_OH_TZ\\WP2\\inputDataset\\"

outputFilbane <- "K:\\FAG\\Tverrfaglig\\AMR\\FoU-aktiviteter & prosjekter\\31218_INIKA_OH_TZ\\WP2\\outputDataset\\"
# Need to write the correct Patways above


Data<-

   read_excel(paste(inputFilbane,"INIKA_SURVEY_ORIGINAL.xlsx"))

library(readxl)
INIKA_SURVEY_ORIGINAL <- read_excel("//vetinst.no/dfs-felles/StasjonK/FAG/Tverrfaglig/AMR/FoU-aktiviteter & prosjekter/31218_INIKA_OH_TZ/WP2/inputDataset/INIKA_SURVEY_ORIGINAL.xlsx")
View(INIKA_SURVEY_ORIGINAL)

# The numbers below can not be used because they are not unique, already used in the dataset!
#INIKA_SURVEY_ORIGINAL_T<-INIKA_SURVEY_ORIGINAL%>%
 # mutate (INIKA_OH_TZ_ID=case_when(INIKA_OH_TZ_ID =="121" ~ "12501", 
                                   #INIKA_OH_TZ_ID =="123" ~ "13502", 
                                   #INIKA_OH_TZ_ID =="137" ~ "12507", 
                                   #INIKA_OH_TZ_ID =="" ~ "12502", 
                                   
                                  # TRUE~INIKA_OH_TZ_ID ))%>%

#mutate (INIKA_OH_TZ_ID=case_when ("Name of the respondent" == "Kitula Miraji" ~ "12502", 
                                
                                 
                                # TRUE~INIKA_OH_TZ_ID ))

# How to filter the dataset

Test<-INIKA_SURVEY_ORIGINAL%>%
  filter(Comment=="Test")

Quest_1<-INIKA_SURVEY_ORIGINAL%>%
  filter(Comment=="First Questionaire")

Quest_2<-INIKA_SURVEY_ORIGINAL%>%
  filter(Comment=="Second Questionaire")
IDTEST<-full_join(Quest_1, Quest_2)%>%
  filter(INIKA_OH_TZ_ID%in%("581", "13520", "13529", "50034", "50038","1310", "137","11508", "11512", "13522","13502", "13516", "11521"," her kommer flere!")%>%
# How to join the dataset
  QUEST<-full_join(Quest_1, Quest_2)%>%
  select(INIKA_OH_TZ_ID,"Breed of chicken", "Sex of respondent", "Marital status of respondent", "Education level of respondent","Ethinicity of respondent", "Occupation of respondent")



rename(QUEST, Breed ="Breed of chicken", Gender="Sex of respondent", Status="Marital status of respondent", Etnicity="Ethinicity of respondent", 
       Education ="Education level of respondent")

  QUESTNEW<-QUEST%>%
  select("Breed", "Gender", "Status", "Etnicity", "Education")
  
  
correct_status<- c("Married", "Single")
  
  corrected_data <- QUEST%>%
  mutate(corrected_status = case_when(
    Status %in% correct_status ~ Status, # If country name is already correct, keep it
    TRUE ~ find_closest_match(Status, correct_status) # Otherwise, find closest match
  ))




  mutate(case_when(Status="Maried" ~ "Married"))

unique(QUEST)
   
 
data<-INIKA_SURVEY_ORIGINAL

content_check <- sapply(data, function(x) {
  if(is.numeric(x) | is.character(x)) {
    any(!is.na(x) & x != "")
  } else {
    TRUE  # Treat non-numeric and non-character columns as containing content
  }
})

content_check <- sapply(data, function(x) any(!is.na(x) & x != ""))

# Print the result
print(content_check)

content_check <- sapply(data, function(x) any(!is.na(x) & x != ""))
