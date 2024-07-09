


# Get the name of the columns to copy paste into select; those that you need!, example below)

head(INIKA_SURVEY_ORIGINAL)

#To get the frequencies of any variable with their different values or spellings use the following:
frequency<-table(INIKA_SURVEY_ORIGINAL$"Breed of chicken")
          print(frequency)
         
# then write me in a wordfile how you want the groupings to be
          # You can try yourself using mutate and case_when as in the example below. But this is by far not finished yet so needs to be corrected!!!!
        


QUEST<-Test%>%
  select("Comment","phonenumber" ,"INIKA_OH_TZ_ID","Name of the respondent","Ward",
       "District",  "Region","Breed of chicken", "Number of chickens","Number of sheds", 
       "Production cycle per year",  
       "Date and time of Interview","Name of the Interviewer",
       "Primary role of the farm" , "Age of respondent..................",
       "Sex of respondent" ,"Marital status of respondent","Ethinicity of respondent",
       "Education level of the farm owner", "Occupation of the farm owner","Family size", "Number of years farming","Number of workers employed on the farm" , 
       "What is the current stage of your flocks? (Check more than one option if have flocks of different stages)",
       
       , "What is the current stage of your flocks? (Check more than one option if have flocks of different stages)/Pre-starter phase (0-7/10 days of age)"                                              
       , "What is the current stage of your flocks? (Check more than one option if have flocks of different stages)/Starter phase (7/10-21 days of age)"                                                 
       ,"What is the current stage of your flocks? (Check more than one option if have flocks of different stages)/Finisher phase (21-42 days of age)" 
)%>%

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
                             
                             
                             
  ))



# BELOW HERE YOU can not run anything as this is not ready YET and yOU were supposed to WRITE these! All the different contents from the dataset are there ( Copy pasted: You just need to group them as I started to do for the Broilers)

#### Here below you need to write this part ready!

mutate( Breed = (case_when
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
###########################################
frequency<- table(Quest_A$Breed)
print(frequency)


frequency<-table(INIKA_SURVEY_ORIGINAL$"Breed of chicken")
          print(frequency)
         
         