
library(readxl)
library (dplyr)
library (tidyverse)


# You need to import the INIKA_SURVEY_ORIGINAL ( which is the downloaded dataset from KObotolbox with your Comments: Test, First Questioanire, Second Questionaire on each row!)
#Use the import function on the right to help you locate and to write the right path.
# This is where I have mine in my computer 
INIKA_SURVEY_ORIGINAL <- read_excel("//vetinst.no/dfs-felles/StasjonK/FAG/Tverrfaglig/AMR/FoU-aktiviteter & prosjekter/31218_INIKA_OH_TZ/WP2/inputDataset/INIKA_SURVEY_ORIGINAL.xlsx")

# Note I already got rid of the Testfarms in the dataset I import!head(INIKA_SURVEY_ORIGINAL)

head(INIKA_SURVEY_ORIGINAL)


# How to filter the Dataset
Test<-INIKA_SURVEY_ORIGINAL%>%
  filter(Comment!="TEST")

INIKA_SURVEY_ID <- Test%>%
  
  rename(Breed="Breed of chicken", Respondent= "Name of the respondent",Herdsize ="Number of chickens", No_Sheds="Number of sheds",
         No_Prod="Production cycle per year",DateI="Date and time of Interview",FarmPurp="Primary role of the farm",Age="Age of respondent..................",
         Gender= "Sex of respondent",Status="Marital status of respondent", Ethnic="Ethinicity of respondent",
         Education="Education level of the farm owner",Occupation="Occupation of the farm owner",FamilySize="Family size", 
         FarmYears="Number of years farming",NoWorkers="Number of workers employed on the farm", 
         Flockage="What is the current stage of your flocks? (Check more than one option if have flocks of different stages)",
         FlockPrestart="What is the current stage of your flocks? (Check more than one option if have flocks of different stages)/Pre-starter phase (0-7/10 days of age)" ,
         FlockStart="What is the current stage of your flocks? (Check more than one option if have flocks of different stages)/Starter phase (7/10-21 days of age)" ,
         Finisher="What is the current stage of your flocks? (Check more than one option if have flocks of different stages)/Finisher phase (21-42 days of age)"
  )%>% 
  mutate(REGIONNO=case_when(Region== "Mwanza" ~ "2",
                            Region== "Mwnza" ~ "2",
                            Region=="Kilimanjaro"~ "1",
                            Region=="Moshi"~ "1",
  ))%>% 
  mutate(DISTRICTNO=case_when(District== "Ilemela" ~ "1",
                              District== "Nyamagana"~ "2",
                              District== "Magu"~ "3",
                              District== "Moshi/rural"~ "1",
                              District== "Moshi rural"~ "1",
                              District== "Moshi/urban"~ "2",
                              District== "Moshi urban" ~ "2",
                              District== "Moshi Village"~ "1",
                              District== "Moshi village"~ "1",
                              District== "M"~ "2",
                              District== "Morogoro Municipal"~ "2",
                              District== "Moshi Municipal" ~ "2",
                              District== "Moshi" ~ "2",
                              District== "Municipal Mc" ~ "2",
                              District== "Moshi DC" ~ "1",
                              
                              District== "Moshi municipal" ~ "2",
                              District== "Moshi urban(Boma mbuzi)" ~ "2",
                              District== "Hai"~ "3",
                              
                              District== "Hai district"~ "3",
                              District== "Siha"~ "3"))%>% 
  
  mutate(REGIONNO=case_when(Respondent== "Emanuel swai"~ "1",
                            Respondent=="Azaria mndeme"~ "1",
                            Respondent=="Miliki Mbwambo"~ "1",
                            Respondent=="Magret Roman kirio"~ "1",
                            Respondent=="Flate Mariwa"~ "1", 
                            TRUE~REGIONNO))%>% 
  
  mutate(DISTRICTNO=case_when(Respondent== "Emanuel swai"~ "1",
                              Respondent=="Azaria mndeme"~ "1",
                              Respondent=="Miliki Mbwambo"~ "1",
                              Respondent=="Magret Roman kirio"~ "1",
                              Respondent=="Flate Mariwa"~ "1",
                              Respondent=="Elizabeth Mtui"~ "3",
                              TRUE~DISTRICTNO
  ))%>%
  mutate(Respondent=case_when(Respondent == "bilabaN" ~ "Heriet  simonfundi",
                              Respondent == "Norbert N" ~ "Rose lema",
                              TRUE ~ Respondent
  ))%>%
  mutate(Interviewer=case_when(Respondent =="Heriet  simonfundi"  ~ "bilabaN",
                               Respondent == "Rose lema" ~ "bilabaN",
                               TRUE ~ Respondent
  )) %>%
  mutate(Region=case_when(REGIONNO=="1" ~ "Kilimanjaro",
                          "REGIONNO"=="2" ~ "Mwanza",
                          TRUE ~ Region
  ))%>%
  
  mutate(Education=case_when(Education%in% c("Standard Seven","Standard seven", "STD SEVEN","Standard 7","StandardVII", "Standard VII", "STANDARD VII") ~ "Primary",
                             Education%in% c("Form IV","Four IV", "Form four","Formfour","Form4", "Form six", "Form IV","Fotm IV", "Form II") ~ "Secondary",
                             Education%in% c("Degree", "Diploma", "Advanced diploma","Bachelor","Cerrificate","Certificate - Animal Health") ~ "College",
                             TRUE ~ Education
                             
                             
                             
  ))%>%
  
  
  
  mutate(Respondent=case_when(Respondent == "bilabaN" ~ "Heriet  simonfundi",
                              Respondent == "Norbert N" ~ "Rose lema",
                              TRUE ~ Respondent
  ))%>%
  mutate(Interviewer=case_when(Respondent =="Heriet  simonfundi"  ~ "bilabaN",
                               Respondent == "Rose lema" ~ "bilabaN",
                               TRUE ~ Respondent
  ))%>%

  
  rename(Respondent="Name of the respondent", Interviewer="Name of the Interviewer")%>% 
  mutate (INIKA_OH_TZ_ID=case_when(Respondent == "Joseph Werema"  ~ "138",
                                   Respondent == "Kitula Miraji"  ~ "1188",
                                   Respondent == "Azaria Mademe"  ~ "1320",
                                   Respondent == "Ashura ibrahim."  ~ "13540",
                                   Respondent == "Prisca  Mush" ~ "13539",
                                   Respondent == "Aurelia msele"~ "13538",
                                   Respondent == "Eunice gidion mhulo"~ "13537",
                                   Respondent == "Vera mushi"~ "13580",
                                   Respondent == "Sophia Nuru Ismail" ~  "11511",
                                   Respondent == "Karisa kunda kimaro" ~  "11506",
                                   Respondent == "Florence Emilinju" ~ "11522",
                                   Respondent == "Bright Bariki" ~ "11526",
                                   Respondent == "Teresia F. Siriwa"  ~ "11525", 
                                   Respondent == "Fidelis Ngowi"  ~ "12514",
                                   Respondent == "Fidel Fulgence Mmbwando" ~ "12521",
                                   Respondent == "Sia Hamphrey Massawe"  ~ "12585",
                                   Respondent == "Lyidia Eliatosha Mfinanga"  ~ "12528",
                                   Respondent == "Vivian Swai"  ~ "13503",
                                   Respondent == "Anande munisi"  ~ "13500",
                                   Respondent == "Matilda Urio" ~ "13511",
                                   Respondent == "Zeno tesha"  ~ "13513",
                                   Respondent == "Evalene paulina"  ~ "13533",
                                   Respondent == "Flate Mariwa" ~ "13601",
                                   Respondent == "Teresia uissio"  ~ "13581",
                                   Respondent == "Leah melau"  ~ "13590",
                                   Respondent == "FRENDRICK MARTIN LEMA" ~ "13600",
                                   Respondent == "Robert Sonda" ~ "520",
                                   Respondent == "Fortunata Martin" ~ "529",
                                   Respondent == "Paulo Joseph Mpanda" ~ "530",
                                   Respondent == "Winfrida Mhondoke" ~ "531",
                                   Respondent == "Agness Maltin Lujuo"~ "532",
                                   Respondent == "Christina Wabeya"~ "533",
                                   Respondent == "Phea Thadei - Mama Deo"~ "534",
                                   Respondent == "Jane Begasha" ~ "535",
                                   Respondent == "IDDA PIUS SHAYO"~ "536",
                                   Respondent == "ELMES CHELFALIS" ~ "537",
                                   Respondent == "Felister mwailondele" ~ "538",
                                   Respondent == "Oteresia loswai" ~ "539",
                                   Respondent == "Ajaba said ally" ~ "540",
                                   Respondent == "Marium masalu said" ~ "541",
                                   Respondent == "Theodora zimeiya" ~ "542",
                                   Respondent == "Selina F. Mashimi" ~ "543",
                                   Respondent == "Gane. E.Mapande" ~ "544",
                                   Respondent == "Eva Sebastian daniel" ~ "545",
                                   Respondent == "Elisabeth Nyanda" ~ "546",
                                   Respondent == "Cristina Mzena" ~ "547",
                                   Respondent == "Agnes japhet bwana" ~ "548",
                                   Respondent == "Upendo Minja" ~ "549",
                                   Respondent == "Rahel  machuma" ~ "550",
                                   Respondent == "Boniphase bagambe" ~ "551",
                                   Respondent == "KP FARM kepha Joseph mbogo" ~ "552",
                                   Respondent == "Jamira amiri." ~ "12188",
                                   
                                   TRUE ~  INIKA_OH_TZ_ID
  ))%>% 
  mutate(=case_when( ~ "2",
                             ~ "2",
                            ~ "1",
                            "~ "1",
  ))%>% 
   mutate(=case_when( ~ "2",
                             ~ "2",
                            ~ "1",
                            "~ "1",
  ))%>% 
  mutate(=case_when( ~ "2",
                     ~ "2",
                     ~ "1",
                     "~ "1",
  ))%>% 
   mutate(=case_when( ~ "2",
                             ~ "2",
                            ~ "1",
                            "~ "1",
  ))%>% 
  mutate(=case_when( ~ "2",
                     ~ "2",
                     ~ "1",
                     "~ "1",
  ))%>% 
   mutate(=case_when( ~ "2",
                             ~ "2",
                            ~ "1",
                            "~ "1",
  ))%>% 
  mutate(=case_when( ~ "2",
                     ~ "2",
                     ~ "1",
                     "~ "1",
  ))%>% 
   mutate(=case_when( ~ "2",
                             ~ "2",
                            ~ "1",
                            "~ "1",
  ))%>% 
  mutate(=case_when( ~ "2",
                     ~ "2",
                     ~ "1",
                     "~ "1",
  ))%>% 
   mutate(=case_when( ~ "2",
                             ~ "2",
                            ~ "1",
                            "~ "1",
  ))%>% 
  mutate(=case_when( ~ "2",
                     ~ "2",
                     ~ "1",
                     "~ "1",
  ))%>% 
   mutate(=case_when( ~ "2",
                             ~ "2",
                            ~ "1",
                            "~ "1",
  ))%>% 
  mutate(=case_when( ~ "2",
                     ~ "2",
                     ~ "1",
                     "~ "1",
  ))%>% 
   mutate(=case_when( ~ "2",
                             ~ "2",
                            ~ "1",
                            "~ "1",
  ))%>% 
  mutate(=case_when( ~ "2",
                     ~ "2",
                     ~ "1",
                     "~ "1",
  ))%>% 
   mutate(=case_when( ~ "2",
                             ~ "2",
                            ~ "1",
                            "~ "1",
  ))%>% 
  