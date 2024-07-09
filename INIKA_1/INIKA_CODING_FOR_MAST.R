

# For your study you will need to do the following:
#The packages have to be installed on your laptop in R studio first by using 
install.packages("readxl","dplyr","tidyverse")
#To get anything running you use CTRL/ ENTER!
#Try and error, nothing can happen with the original dataset which you have stored safely in your PC!
# Run each line to the %>% to make sure it works,
 when you see it works you can run all! Mark the whole script and just run it 
 (CTRL A) and then CTRL ENTER
 
library(readxl)
library (dplyr)
library (tidyverse)
 

 # You need to import the INIKA_SURVEY_ORIGINAL ( which is the downloaded dataset from KObotolbox with your Comments: Test, First Questioanire, Second Questionaire on each row!)
 #Use the import function on the right to help you locate and to write the right path.
# This is where I have mine in my computer 
 INIKA_SURVEY_ORIGINAL <- read_excel("//vetinst.no/dfs-felles/StasjonK/FAG/Tverrfaglig/AMR/FoU-aktiviteter & prosjekter/31218_INIKA_OH_TZ/WP2/inputDataset/INIKA_SURVEY_ORIGINAL.xlsx")

  # Note I already got rid of the Testfarms in the dataset I import!
  
# How to filter the Dataset
Test<-INIKA_SURVEY_ORIGINAL%>%
  filter(Comment!="TEST")

Quest_1<-INIKA_SURVEY_ORIGINAL%>%
  filter(Comment=="First Questionaire")

Quest_2<-INIKA_SURVEY_ORIGINAL%>%
  filter(Comment=="Second Questionaire")

# First you select only a few variables as defined below to see that it works!
#(Row 40) This row you need to use and expand with those variables you will need afterwards and which you want to recode to avoid to long names, in the select statement.like this:
#select("", "", "" "", ""), IF you don't select - you get all the columns!
# rename( Give a new name to the columns needed, so it will be easier to handle, example below. You better copy paste the columnnames from the dataset so they will be exactly correct written!)
# mutate (means you create a new variable content ofor a variable here it will mainly be used with the fuction case_when ( which means if) so it is conditional when the content in another variable
# in the example below it is rdependent on the content in the Respondent, the last part is saying that it should keep all the content in the INIKA_OH_TZ_ID if not it should be changed.
#TRUE ~  INIKA_OH_TZ_ID. after each finsihed part we use something called a pipe. This is to avoid makeing new datasets the whole time. You can run a part to the pipe and see that it is working.
#In the end you can run alltogether.

INIKA_SURVEY_ID <- Test%>%
  select("Comment","INIKA_OH_TZ_ID","Name of the respondent", "Region", "District",
         "Ward","Name of the Interviewer" )%>% 
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
  mutate(NEWID = paste(REGIONNO,DISTRICTNO, sep=""))%>%
  
 

  mutate(INIKA_OH_TZ_ID=case_when(substr(INIKA_OH_TZ_ID, 1, 1) =="5"
                                  ~ paste(NEWID,INIKA_OH_TZ_ID, sep=""),
                                  TRUE~INIKA_OH_TZ_ID
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
  ))

# Then I check if the INIKA_OH_TZ_IDS are correct according to the coding of Regionnumbers and Diatrictsnumber and if not those are named WRONG
  Sjekk<-INIKA_SURVEY_ID%>%
  mutate(TESTID=case_when(substr(INIKA_OH_TZ_ID, 1, 2) != substr(NEWID, 1, 2) ~ "WRONG",
                          TRUE ~ "OK"
  ))
  
  #I then use the name above again
  INIKA_SURVEY_ID<-INIKA_SURVEY_ID%>%
  mutate(INIKA_OH_TZ_ID=case_when(substr(INIKA_OH_TZ_ID, 1, 2) !=
                                      substr(NEWID, 1, 2) ~ paste(NEWID,INIKA_OH_TZ_ID, sep="")
                                    ,
                                  TRUE ~ INIKA_OH_TZ_ID
    ))
  
  
                       
  # Then I check if the INIKA_OH_TZ_IDS are correct according to the coding of Regionnumbers and Districtsnumber and if not those are named WRONG
  Chekk<-INIKA_SURVEY_ID%>%
    mutate(TESTID=case_when(substr(INIKA_OH_TZ_ID, 1, 2) != substr(NEWID, 1, 2) ~ "WRONG",
                            TRUE ~ "OK"
    ))%>%
    filter(Comment!="TEST")
  
View(Check)
  


  
INIKA_SURVEY_ID<-INIKA_SURVEY_ID%>%
  rename(Breed ="Breed of chicken", Gender="Sex of respondent", Status="Marital status of respondent", Etnicity="Ethinicity of respondent", 
         Education ="Education level of respondent")%>%
  
  mutate(Education=case_when(Education%in% c("Standard Seven","Standard seven", "STD SEVEN","Standard 7","StandardVII", "Standard VII", "STANDARD VII") ~ "Primary",
                             Education%in% c("Form IV","Four IV", "Form four","Formfour","Form4", "Form six", "Form IV","Fotm IV", "Form II") ~ "Secondary",
                              Education%in% c("Degree", "Diploma", "Advanced diploma","Bachelor","Cerrificate","Certificate - Animal Health") ~ "College",
                             TRUE ~ Education
                             
                             
                             
  ))


# BELOW HERE YOU can not run anything as this is not ready YET and yOU were supposed to WRITE these! All the different contents from the dataset are there ( Copy pasted: You just need to group them as I started to do for the Broilers)
  
 #### Here below you need to write this part ready!
  mutate(Breed = (case_when
                   (Breed %in% c("Alvin","Arvin broiler", 
                                 "Arvin broilersBroiler", 
                                 "Broiler - arvin",
                                 "Broiler -arvin",      
                                 "Broiler and croiler", 
                                 "Broiler and Saso",                
                                 "Broiler arvin",             
                                 "Broiler Ross 308", 
                                 "Broiler silverland",
                                 "Broiler, Kuroirer and Saso",
                                 "Broiler, layers", 
                                 "Broiler, saso",                
                                 "Broiler,alvin",               
                                 "Broiler,layers", 
                                 "Broiler,Saso",               
                                 "Broilers alvin",
                                 "Cob 500",
                                 "Cobb 500", 
                                 "Cobb and Rose",
                                 "Cobb500",              
                                 "cobb500 & sasso", 
                                 "Conb500Roos 308",
                                 "Ros308",
                                 "Ross 305", 
                                 "Ross 308",
                                 "Ross300", 
                                 "Ross308,
"Intercheeck broiler", 
"Interchick",                 
"Interchicks",
"Interchicks and centrachicks",
"Interchicks and centrochicks",
"Saso and broiler",
Silverland broiler",            
                                 "Saso and Broiler",
                                 "Saso Ross",
                                 "Saso Ross 308",
                                 "Saso silverland",
                                 "Saso silverland 308,
"Silverland broiler")  ~ "Broiler",

"Chroirer",
"Croiler ",       
"Croiler - Interchicks", 
"Croiler and Saso",
) 
))


Asho
Ashok 
Ashork
Bovans
                      
Chotara 
   
Erveness and sendlecheck                      
Evines 
Falcon                      
hashokh    
Intercheeck ,and kencheek 

Joshi 
Kibo and silverland                      
Kloiler                      
Kroiler 
Kroiler/ layers                     
Kuroiler
Layers and saso 



Saso            
Saso- silver land                 
Saso- tanbro 
Saso - silverland
Saso & kloiler                      
Saso 38 
Saso& kroiler
Saso, croiler    
Saso, kroiler and Broiler 
Saso,chotara
Saso,Chotara 
Silverland 
Silverland saso                      
Tanbro 
Tanbro -intercheeck 
Tanbro and Saso 
Tanbro,saso 
  
  
  
 
  
 
    
    
    
    
   
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                   
  
  
  
  

