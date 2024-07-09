library(readxl)
library (dplyr)
library (tidyverse)

ID_INIKA_REG_DIS <- read_excel("//vetinst.no/dfs-felles/StasjonK/FAG/Tverrfaglig/AMR/FoU-aktiviteter & prosjekter/31218_INIKA_OH_TZ/WP2/inputDataset/ID_INIKA_REG_DIS.xlsx")
View(ID_INIKA_REG_DIS)

ID_INIKA_REG_DIS<-ID_INIKA_REG_DIS%>%
  select("NAME", "CONTACT","REGION", "DISTRICT",  "LOCATION", "ID")
Unique_combinations_LIST <- ID_INIKA_REG_DIS %>% 
  distinct(REGION,DISTRICT,LOCATION, ID)%>% 
  mutate(REGIONNO=case_when(REGION== "MWANZA" ~ "2",
                            REGION=="KILIMANJARO"~ "1"))%>% 
         mutate(DISTRICTNO=case_when(DISTRICT== "ILEMELA" ~ "1",
                                     DISTRICT== "NYAMAGANA"~ "2",
                                    DISTRICT== "MAGU"~ "3",
                                    DISTRICT== "MOSHI/RURAL"~ "1",
                                    DISTRICT== "MOSHI/URBAN"~ "2",
                                    DISTRICT== "HAI"~ "3"
                                    ))%>%
  mutate(INIKA_OH_TZ_ID=case_when
         (substring(INIKA_OH_TZ_ID,1,1) =="5"  ~ "REGIONNO",)))


substr(analyttkode, 1, 8) %in% c("04060103",

TEST<-(full_join(ID_INIKA_REG_DIS,Unique_combinations_LIST,))%>%
  mutate(NEWID = paste(REGIONNO,DISTRICTNO, sep=""))%>%
  select(-ID)


  

INIKA_SURVEY_ORIGINAL <- read_excel("//vetinst.no/dfs-felles/StasjonK/FAG/Tverrfaglig/AMR/FoU-aktiviteter & prosjekter/31218_INIKA_OH_TZ/WP2/inputDataset/INIKA_SURVEY_ORIGINAL.xlsx")
INIKA_SURVEY_ID <- INIKA_SURVEY_ORIGINAL%>%
  select("INIKA_OH_TZ_ID","Name of the respondent", "Region", "District", "Ward", "GPS_Coordinates")%>% 
  rename(Respondent="Name of the respondent")%>% 
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
  mutate(INIKA_OH_TZ_ID=case_when(substr(INIKA_OH_TZ_ID, 1, 1)=="5"
                                  ~ paste(NEWID,INIKA_OH_TZ_ID, sep=""),
                                  TRUE~INIKA_OH_TZ_ID
  ))%>%
  select(-ID)
  
install.packages("fuzzyjoin")
library(fuzzyjoin)
RESULT<-stringdist_left_join(TEST,INIKA_SURVEY_ORIGINAL,  BY="Respondent", method="jw")
 








Unique_combinations <- INIKA_SURVEY_ORIGINAL %>% 
  distinct(Region, District, Ward)


head(Quest_A)


Quest_A<-as.data.frame(INIKA_SURVEY_ORIGINAL)%>%
  


mutate(Respondent=case_when(Respondent == "Heriet  simonfundi" ~ "bilabaN",
                            Respondent == "Rose lema" ~ "Norbert N",
                            TRUE ~ Respondent
))%>%



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

frequency<- table(Quest_A$Breed)
print(frequency)

unique<- table(Quest_A$Breed)
print(unique) 
  Quest_A<-Quest_A%>%
 
  mutate (
    INIKA_OH_TZ_ID=case_when(Respondent == "Joseph Werema"  ~ "138",
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


                           
                                                       
                        
                            TRUE ~  INIKA_OH_TZ_ID
                            ))
  
  Quest_T<-Quest_A%>%
    mutate(n=1)%>%
    group_by (INIKA_OH_TZ_ID, n)%>%
    summarise(frequency=n())
  
  Quest_T<-Quest_T%>% 
    group_by(INIKA_OH_TZ_ID)%>%
    mutate(X=ifelse(frequency>1, "Doublet", "Single"))
  
  Quest_X<- full_join(Quest_A, Quest_T)
  
  
# NB! Finner ikke?Respondent == "	
                             
                    
                            
                               
                               
                          
                                 
                                 
                              
head(Quest_T)
