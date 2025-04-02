
library(readxl)
library (dplyr)
library (tidyverse)


INIKAKAT<- QUEST%>%
mutate(Gender_F = case_when(Gender_F == "female" ~ "Female",
  Gender_F == "male" ~ "Male",
  TRUE ~ Gender_F
))%>%
  
  mutate(Gender_R = case_when(
    Gender_R == "female" ~ "Female",
    Gender_R == "male" ~ "Male",
    TRUE ~ Gender_R
  ))%>%
  
  
  mutate(Status_R = case_when(
    Status_R %in% c("Maried", "Female", "Married") ~ "Married",  
    Status_R %in% c("Window", "widow", "4") ~ "Widow",            
    Status_R %in% c("3", "2", "7", "single", "Single") ~ "Single",         
    TRUE ~ Status_R
  ))%>%
  
  mutate(Status_F = case_when(
    Status_F %in% c("Maried", "Female", "Married") ~ "Married",  
    Status_F %in% c("Window", "widow", "4") ~ "Widow",            
    Status_F %in% c("3", "2", "7", "single", "Single") ~ "Single",         
    TRUE ~ Status_F
  ))%>%
  
  mutate(Status_F_2 = case_when(
    Status_R %in% c("Maried", "Female", "Married") ~ "Married",  
    Status_R %in% c("Window", "widow", "4") ~ "Widow",            
    Status_R %in% c("3", "2", "7", "single", "Single") ~ "Single",         
    TRUE ~ Status_F_2
    
  ))%>%
  mutate(REGIONNO = case_when(
    Region == "Mwanza" | Region == "Mwnza" ~ "2",
    Region == "Kilimanjaro" | Region == "Moshi" ~ "1",
    TRUE ~ Region
  ))%>%
  mutate(DISTRICTNO = case_when(
    District == "Ilemela" ~ "1",
    District == "Nyamagana" ~ "2",
    District == "Magu" ~ "3",
    District %in% c("Moshi Municipal","Morogoro Municipal","Moshi", "Municipal Mc", "Moshi DC", "Moshi urban", "Moshi municipal", "Moshi urban(Boma mbuzi)") ~ "1",
    District %in% c("Moshi/rural", "Moshi rural", "Moshi Village", "Moshi village") ~ "2",
    District %in% c("Hai", "Hai district", "Siha") ~ "3",
    TRUE ~ District
  ))%>%
  mutate(REGIONNO = case_when(
    Respondent %in% c("Emanuel swai", "Azaria mndeme", "Miliki Mbwambo", "Magret Roman kirio", "Flate Mariwa") ~ "1",
    TRUE ~ REGIONNO
  ))%>%
  mutate(DISTRICTNO = case_when(
    Respondent %in% c("Emanuel swai", "Azaria mndeme", "Miliki Mbwambo", "Magret Roman kirio", "Flate Mariwa") ~ "1",
    Respondent == "Elizabeth Mtui" ~ "3",
    TRUE ~ DISTRICTNO
  ))%>%
  mutate(Respondent = case_when(
    Respondent == "bilabaN" ~ "Heriet simonfundi",
    Respondent == "Norbert N" ~ "Rose lema",
    TRUE ~ Respondent
  ))%>%
  mutate(Interviewer = case_when(
    Respondent %in% c("Heriet simonfundi", "Rose lema") ~ "bilabaN",
    TRUE ~ Respondent
  ))%>%
  mutate(Region = case_when(
    REGIONNO == "1" ~ "Kilimanjaro",
    REGIONNO == "2" ~ "Mwanza",
    TRUE ~ Region
  ))%>%
  mutate(Education_R = case_when(
    Education_R %in% c("Standard Seven", "Standard seven", "Standard sevev", "STD SEVEN", 	
                       "Standard sevev.","Standard 7", "StandardVII", "Standard VII", "STANDARD VII") ~ "Primary",
    Education_R %in% c("Form IV", "STANDARD IV", "Standard IV", "Standard four", "From four", "Form VI", "Form two", "Four IV", "Form four", "Formfour", "Form4", "Form six", "Form IV", "Fotm IV", "Form II") ~ "Secondary",
    Education_R %in% c("Degree", "Certificate","Diploma", "Advanced diploma", "Bachelor", "Cerrificate", "Certificate - Animal Health", "Colledge", "A degree") ~ "College",
    TRUE ~ Education_R
  ))%>%
  mutate(Ethnic_R = case_when(
    Ethnic_R %in% c("MCHAGA","Mchanga","Mchaga"
    ) ~ "Chaga",
    Ethnic_R == "Mpare" ~ "Pare",
    Ethnic_R %in% c ("Msukuma", "0") ~ "Sukuma",
    TRUE ~ "Others"
  ))%>%
  mutate(Ethnic_F = case_when(
    Ethnic_F %in% c("MCHAGA", "Mchaga") ~ "Chaga",
    Ethnic_F == "Mpare" ~ "Pare",
    Ethnic_F  %in% c("Msukuma", "0") ~ "Sukuma",
    TRUE ~ "Others"
  ))%>%
  mutate(Education_F = case_when(
    Education_F %in% c("Standard Seven", "Standard seven", "STD SEVEN", "Standard 7", "StandardVII", "Standard VII", "STANDARD VII") ~ "Primary",
    Education_F %in% c("Form IV", "Four IV", "Form four", "Formfour", "Form4", "Form six", "Form IV", "Fotm IV", "Form II") ~ "Secondary",
    Education_F %in% c( "Cerrificate", "Certificate - Animal Health") ~ "College",
    Education_F %in% c("Bachelor degree","Degree","Diploma", "Advanced diploma", "Bachelor","PhD") ~ "University",
    TRUE ~ Education_F
  ))%>%
  mutate(Occupation_R = case_when(
    Occupation_R %in% c("farmer", "Farmer", "FARMER", "Famer", "Farm Manager", "Farming", "Livestock", "Livestock keeper", "Livestock Farmer", "Livestock farmer","Farmer.", "Farmer and business", "Farmer and Business", "Broiler Farmer, agricultural, and poultry business man", "Farmer and Intrepreneurer") ~ "Farmer",
    Occupation_R %in% c("Farmer and loan officer", "Farmer and Nutritionist","Farmer and electrical technician", 	
                        "Farmer and interprenewer","Famer and mechanical person", "Farmer/cooker","Farmer_and_Other", 	
                        "Interprenewer in livestock keeper","Farming and other business", "Farmer_and_Other", "Farmer and Other", "Manager & Farming", 	
                        "Pastor and farmer") ~ "Farmer_and_Other",
    
    TRUE ~ "Other"
  ))%>%
  
  mutate(Occupation_F2 = case_when(
    Occupation_F2 %in% c("farmer", "Farmer", "FARMER", "Famer", "Farm Manager", "Farming", "Livestock", "Livestock keeper", "Livestock Farmer", "Livestock farmer","Farmer.", "Farmer and business", "Farmer and Business", "Broiler Farmer, agricultural, and poultry business man", "Farmer and Intrepreneurer") ~ "Farmer",
    Occupation_F2 %in% c("Farmer and loan officer", "Farmer and Nutritionist","Farmer and electrical technician", 	
                         "Farmer and interprenewer","Famer and mechanical person", "Farmer/cooker","Farmer_and_Other", 	
                         "Interprenewer in livestock keeper","Farming and other business", "Farmer_and_Other", "Farmer and Other", "Manager & Farming", 	
                         "Pastor and farmer") ~ "Farmer_and_Other",
    
    TRUE ~ "Other"
  ))%>%
  mutate(Usage_OF_AB = case_when(
    Usage_OF_AB == "no" ~ "Male",
    TRUE ~ Usage_OF_AB
  ))%>%
  mutate(Knowlede_AB = case_when(
    Knowlede_AB == "no" ~ "No",
    Knowlede_AB == "yes" ~ "Yes",
    TRUE ~ Knowlede_AB
  ))%>%
  
  mutate(AB_Thoughts==case_when(
    AB_Thoughts=="no" ~ "No",
    AB_Thoughts== "yes" ~ "Yes",
    TRUE ~ AB_Thoughts
  ))%>%
  
  
mutate(Heard_AMR==case_when(
  Heard_AMR =="no" ~ "No",
  Heard_AMR== "yes" ~ "Yes",
  TRUE ~ Heard_AMR
))%>%
  mutate(AB_training = case_when(
    AB_training == "no" ~ "No",
    AB_training== "yes" ~ "Yes",
    TRUE ~ AB_training
  ))%>%
  mutate(Knowledge_AMR = case_when(
    Knowledge_AMR == "somwhat familar" ~ "Somewhat familiar",
    Knowledge_AMR == "somewhat_familiar" ~ "Somewhat familiar",
    Knowledge_AMR == "very_familiar" ~ "Very familiar",
    TRUE ~ Knowledge_AMR
  ))%>%
  mutate(AMR_Knowledge_Perception = case_when(
    AMR_Knowledge_Perception == "average_knowledge" ~ "Average knowledge",
    AMR_Knowledge_Perception == "high_knowledge" ~ "High knowledge",
    AMR_Knowledge_Perception == "low_knowledge" ~ "Low knowledge",
    AMR_Knowledge_Perception == "very_high_knowledge" ~ "Very high knowledge",
    TRUE ~ AMR_Knowledge_Perception
  ))%>%
  mutate(Knowledge_Collaborative_OH = case_when(
    Knowledge_Collaborative_OH == "no" ~ "No",
    Knowledge_Collaborative_OH == "yes" ~ "Yes",
    TRUE ~ Knowledge_Collaborative_OH
  ))%>%
  mutate(AB_UnderstandingKAT = case_when(
    AB_Understanding %in% c(
      "Are drugs to control bacterial diseases",
      "Drugs to prevent diseases",
      "Drugs to prevent from disease",
      "Drugs to control diseases",
      "Drugs to control diseases of chiken",
      "Drugs use to prevent disease",
      "Drugs used to control diseases",
      "Medicine to prevent disease",
      "Medicine use to prevent disease",
      "Medicine used to prevent disease",
      "Preventive drugs to the chickens",
      "These are drugs to control diseases",
      "They use to prevent disease to chickens",
      "We use to prevent chicken from sick",
      "Drugs used to prevent dealth of chickens"
    ) ~ "Prevent",
    AB_Understanding %in% c(
      "Drugs use to treat and prevent chickens from sick",
      "It prevent and treat diaseases",
      "Medicine for both preventing and treatment of diseases",
      "Medicine for chicken to heal and prevent them",
      "Medicine use to cure diseases and prevent them from sick",
      "Medicine use to prevent and cure diseases",
      "Medicine use to prevent and treat diaseases",
      "Medicine use to treat and cure diseases",
      "Medicines used to treatments and prevention of diseases",
      "Medicined for Treatment and prevention",
      "Medicines used for prevent and reatments of diseases",
      "Medicines used for prevention and treatment of diseases",
      "Prevent and treatment of diseases",
      "Medicine used to prevent and cure",
      "Medicine used to prevent and cure diseases",
      "Medicine used to prevent and treat chickens",
      "Thinks used to kill germs and control diseases",
      "To prevent disease and treat diseases",
      "Medicines for treatment and prevention of diseases",
      "This are medicine used to prevent chickens from sick",
      "Medicined for Treatment and prevention"
    ) ~ "Prevent and Treat",
    AB_Understanding %in% c(
      "Chemical to kill bacteria diseases",
      "Chemical to treat diseases",
      "Chemical used to manage diseases.",
      "Chemicals and drugs to treat diseases",
      "Chemicals and things used to treat diaseses",
      "Chemicals to cure chicken diseases",
      "Chemicals used to treat diseases",
      "Diseases to kill germ",
      "Drugs for treating chicken",
      "Drugs for treating chickens",
      "Drugs ides to treat diseases",
      "Drugs that We use for treatment",
      "Drugs to give chicken when they are sick",
      "Drugs to help chickens when they get sick",
      "Drugs to kill bacteria",
      "Drugs to kill bacteria chicken diseases",
      "Drugs to kill disease",
      "Drugs to kill diseases",
      "Drugs to kill drugs",
      "Drugs to kill germs",
      "Drugs to kill germs of chicken",
      "Drugs to that treat diaseases",
      "Drugs to treat chicken diseases",
      "Drugs to treat diseases",
      "Drugs use to cure diseases",
      "Drugs use to treat chickens",
      "Drugs use to treat different diseases",
      "Drugs used to cure diseases",
      "Drugs used to cure diseases to the chickens",
      "Drugs used to kill diseases",
      "Drugs used to kill druds",
      "Drugs used to treat chickens",
      "Drugs used to treat diseases",
      "Drugs used to treat diseases of chikens",
      "Drugs used to treat germs",
      "Drugs used to treat kill germs",
      "For treatments of diseases",
      "Germs to treat diseases",
      "Medicine help chickens",
      "Medicine that kills bacteria",
      "Medicine to treat chickens",
      "Medicine to treat diseases",
      "Medicine to treat diseases more than one",
      "Medicine use to cure diseases",
      "Medicine use to prevent diseases",
      "These are drugs that kill bacteria",
      "These are drugs used to treat specific diseases",
      "These are drugs which treat diseases",
      "Things that kill bacteria",
      "Things used to treat diseases",
      "Medicine use to treat chickens",
      "Medicine use to treat diseases",
      "Medicines used for treatment of diseases",
      "Medicines used to treat diseases",
      "Medicine used to treat chickens",
      "Medicine used to treat diseases",
      "Medicines for treating chickens",
      "Medicines for treatment of diseases",
      "Medicines for treatments of diseases",
      "Medicines used to treatments",
      "Medicines used yo treat diseases",
      "Treat for chickens diseases",
      "Use to treat chickens",
      "When the chicken is sick we use them"
    ) ~ "Treatment",
    AB_Understanding %in% c(
      "Medicines used to treat medicines",
      "Medicines used to treat Medicines",
      "Drugs we use to the chickens"
    ) ~ "Unsure",
    TRUE ~ "Missing"
  ))%>%
  
  mutate(Breed = case_when
         (Breed %in% c("Alvin", "Arvin broiler", "Arvin broilersBroiler", "Broiler - arvin", "Broiler -arvin", "Broiler and croiler", "Broiler and Saso", "Broiler arvin", "Broiler Ross 308", "Broiler silverland", "Broiler, Kuroirer and Saso", "Broiler, layers", "Broiler, saso", "Broiler,alvin", "Broiler,layers", "Broiler,Saso", "Broilers alvin", "Cob 500", "Cobb 500", "Conb500", "Cobb and Rose", "Cobb500", "cobb500 & sasso", "Conb500Roos 308", "Ros308", "Ross 305", "Ross 308", "Ross300", "Ross308", "Intercheeck broiler", "Interchick", "Interchicks", "Interchicks and centrachicks", "Interchicks and centrochicks","Saso and broiler",  "Silverland broiler",
                       "Saso and Broiler", "Saso Ross", "Saso Ross 308", "Silverland broiler", "Arvin broiler", "Arvin broilersBroiler", "Broiler - arvin", "Broiler -arvin", "Broiler and croiler", "Broiler and Saso", "Broiler arvin", "Broiler Ross 308", "Broiler silverland", "Broiler, Kuroirer and Saso", "Broiler, layers", "Broiler, saso", "Broiler,alvin", "Broiler,layers", "Broiler,Saso", "Broilers alvin", "Cob 500", "Cobb 500", "Cobb and Rose", "Cobb500", "cobb500 & sasso", "Conb500Roos 308", "Ros308", "Ross 305", "Ross 308", "Ross300", "Ross308", "Intercheeck broiler","Saso and broiler", "Silverland broiler", "Saso and Broiler", "Saso Ross", 
                       "Saso Ross 308","Saso, kroiler and Broiler", "Arvin broilers", "300", "Roos 308") ~ "Broiler", 
           Breed %in% c("Saso silverland", "Saso silverland 308", "Chroirer", "Croiler", "Croiler - Interchicks", "Croiler and Saso",
                        "Asho", "Ashok", "Ashork", "Bovans", "Chotara", "Erveness and sendlecheck", "Evines", "Falcon", "hashokh", "Intercheeck ,and kencheek",
                        "Joshi", "Kibo and silverland", "Kloiler", "Kroiler", "Kroiler/ layers", "Kuroiler", "Layers and saso", "Saso", "Saso- silver land",
                        "Saso- tanbro","Tanbro,saso", "Saso - silverland", "Saso & kloiler", "Saso 38", "Saso& kroiler", "Saso, croiler",
                        "Saso,chotara", "Saso,Chotara", "Silverland", "Silverland saso", "Tanbro", "Tanbro -intercheeck", "Tanbro and Saso", "Tanbro,sasoall") ~ "Crossbreds" ,
           TRUE ~ Breed
           ))%>%

mutate(Overusage_AB_Poultry = case_when(Overusage_AB_Poultry=="very_overused" ~ "Very Overused",
                                        Overusage_AB_Poultry =="somewhat_overused" ~ "Somewhat overused",
                                        Overusage_AB_Poultry =="little_overused"~ "Little overused",
                                        Overusage_AB_Poultry =="overused" ~ "Overused",
                                        TRUE ~ Overusage_AB_Poultry 
))%>%
  mutate(ADM_antibiotics_G = case_when(ADM_antibiotics_G==
                                                 "mostly_women" ~ "Mostly women",
                                                ADM_antibiotics_G == "mostly_men" ~ "Mostly men",
                                                ADM_antibiotics_G == "men_and_women_equally" ~ "Men and Women equally",
                                                ADM_antibiotics_G == "almost_exclusively_men" ~ "Almost exclusively men",
                                                ADM_antibiotics_G == "almost_exclusively_women" ~ "Almost exclusively women",
    TRUE ~  ADM_antibiotics_G
  )) %>%
  mutate( Care_Chicken_G = case_when(
     Care_Chicken_G == "mostly_women" ~ "Mostly women",
     Care_Chicken_G == "mostly_men" ~ "Mostly men",
     Care_Chicken_G == "men_and_women_equally" ~ "Men and Women equally",
     Care_Chicken_G == "almost_exclusively_men" ~ "Almost exclusively men",
     Care_Chicken_G == "almost_exclusively_women" ~ "Almost exclusively women",
    TRUE ~  Care_Chicken_G
  )) %>%
  
  mutate(Disposal_G = case_when(
    Disposal_G == "mostly_women" ~ "Mostly women",
    Disposal_G == "mostly_men" ~ "Mostly men",
    Disposal_G == "men_and_women_equally" ~ "Men and Women equally",
    Disposal_G == "almost_exclusively_men" ~ "Almost exclusively men",
    Disposal_G == "almost_exclusively_women" ~ "Almost exclusively women",
    TRUE ~ Disposal_G
  )) %>%
  
  mutate(Disposal_dead_broilers_G  = case_when(
    Disposal_dead_broilers_G  == "mostly_women" ~ "Mostly women",
    Disposal_dead_broilers_G  == "mostly_men" ~ "Mostly men",
    Disposal_dead_broilers_G  == "men_and_women_equally" ~ "Men and Women equally",
    Disposal_dead_broilers_G  == "almost_exclusively_men" ~ "Almost exclusively men",
    Disposal_dead_broilers_G  == "almost_exclusively_women" ~ "Almost exclusively women",
    TRUE ~ Disposal_dead_broilers_G 
  )) %>%
  
  mutate(Decision_Treatment_G = case_when(
    Decision_Treatment_G == "mostly_women" ~ "Mostly women",
    Decision_Treatment_G == "mostly_men" ~ "Mostly men",
    Decision_Treatment_G == "men_and_women_equally" ~ "Men and Women equally",
    Decision_Treatment_G == "almost_exclusively_men" ~ "Almost exclusively men",
    Decision_Treatment_G == "almost_exclusively_women" ~ "Almost exclusively women",
    TRUE ~ Decision_Treatment_G
  ))%>%
  
  mutate(AB_Disposal_G = case_when(
    AB_Disposal_G == "mostly_women" ~ "Mostly women",
    AB_Disposal_G == "mostly_men" ~ "Mostly men",
    AB_Disposal_G == "men_and_women_equally" ~ "Men and Women equally",
    AB_Disposal_G == "almost_exclusively_men" ~ "Almost exclusively men",
    AB_Disposal_G == "almost_exclusively_women" ~ "Almost exclusively women",
    TRUE ~ AB_Disposal_G
  ))%>%

  mutate(Income_Decision_G = case_when(
    Income_Decision_G == "mostly_women" ~ "Mostly women",
    Income_Decision_G == "mostly_men" ~ "Mostly men",
    Income_Decision_G == "men_and_women_equally" ~ "Men and Women equally",
    Income_Decision_G == "almost_exclusively_men" ~ "Almost exclusively men",
    Income_Decision_G == "almost_exclusively_women" ~ "Almost exclusively women",
    TRUE ~ Income_Decision_G
  ))%>%

mutate(Heard_OH==case_when(Heard_OH=="no" ~ "No",
                             Heard_OH=="yes" ~ "Yes",
                             TRUE ~ Heard_OH
))%>%
  mutate(Broiler_Disease_Outbreak_Human_Health==case_when(Broiler_Disease_Outbreak_Human_Health =="high" ~ "High",
                                                          Broiler_Disease_Outbreak_Human_Health =="low" ~ "Low",
                                                          Broiler_Disease_Outbreak_Human_Health =="moderate" ~ "Moderate",
                                                          TRUE ~ "Broiler_Disease_Outbreak_Human_Health"
))%>% 
mutate(Source_Training_AB = case_when(Source_Training_AB == "Friends and family" ~ "Friends and Family",
Source_Training_AB %in% c("church", 
"From Lutheran church they provided us with the knowledge and books as guied(mchomvu))",
"NCA-Erasto",
"NCA-ERASTO",
"NCA","Erasto",
"NCA-ERASTO", "NCA-ERASTO",
          "NRS","From the church."
 ) ~ 'Church',
                                                                  
Source_Training_AB %in% c("Mabuki farm", "farm.", "Agricultural association services",
"Agricultural association services Veterinary professionals","Localcommunitymeetings","local_community_meetings","local_community_meetings" ) ~ 'Local community',
                     
Source_Training_AB %in% c ("Sua","Training from SUA via ministry of livestock","College",
            "Livestock College Madaba","Shinyanga",
            "During the college as am animal health practitioner"
            ) ~ 'Government',
Source_Training_AB  %in%  c("I don't remember",
                                          "I don't remember due to variety of companies that come to train us on how to grow our chicken",
                                          "I don't remember they were from kenya","I dont remember" ) ~ 'Unknown',
                                        
 
Source_Training_AB %in% c("Alvin",
                          "Alvin Company",
                          "Arvin chicks companies",
  "Arvin chikens companies",
  "Arvin saso",
  "Arvin supplier of chicks",
  "Ashorkh", "KIBO","KUGIES", "Tao nutrition","Mabuki farm",
  "NGOKMG","NRS","Saso Silverland","Shinyanga",
  "Silverland Iringa","Silverland saso","Chicks companies", 
  "To agrovet shops","Harsho and silverland,","Harshoseminars",
  "Harshok","HAVICK")  ~ 'Companies',
TRUE ~ "Other" 
))%>%

mutate(Access_InformationAB= 
         case_when(Access_InformationAB =="no" ~ "No",
                  Access_InformationAB =="yes" ~ "Yes",
  TRUE ~ Access_InformationAB
  ))


DATA<- select


write.csv(INIKAKAT, file = paste(outputFilbane,"Resultat\\","INIKAKAT.csv", sep = ""))
