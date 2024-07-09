#setwd("//vetinst.no/dfs-felles/Stasjon/FAG/Tverrfaglig/AMR/FoU-aktiviteter & prosjekter/31218_INIKA_OH_TZ/WP2")

INIKA_SURVEY_ORIGINAL <- read_excel("//vetinst.no/dfs-felles/StasjonK/FAG/Tverrfaglig/AMR/FoU-aktiviteter & prosjekter/31218_INIKA_OH_TZ/WP2/inputDataset/INIKA_SURVEY_ORIGINAL.xlsx")

INIKA<-INIKA_SURVEY_ORIGINAL%>%
  filter(Comment!="TEST")
colnames(INIKA)

# sette in kordinater her!

QUEST<-INIKA%>%
  dplyr::rename(Respondent= "Name of the respondent",
         Breed="Breed of chicken",
         Herdsize ="Number of chickens",
         No_Sheds="Number of sheds",
         No_Prod="Production cycle per year",
         DateI="Date and time of Interview",
         FarmPurp="Primary role of the farm",Age_R="Age of respondent..................",
         Gender_R= "Sex of respondent",
         Status_R="Marital status of respondent",
         Ethnic_R="Ethinicity of respondent", 
         Gender_F= "Sex of the farm owner",
         Status_F="Marital status of the farm owner",   
         Interviewer= "Name of interviewer",                                                                                                                                                                        
         Ethnic_F=  "Ethnicity of the farm owner" ,                                                                                                                                                    
         Occupation_R  = "Occupation of respondent" , 
         Education_R = "Education level of respondent",
         Education_F= "Education level of the farm owner"  ,                                                                                                                                                           
         Occupation_F= "Occupation of the farm owner" , 
         FamilySize="Family size", 
         FarmYears="Number of years farming",
         NoWorkers="Number of workers employed on the farm", 
         Flockage="What is the current stage of your flocks? (Check more than one option if have flocks of different stages)",
         FlockPrestart="What is the current stage of your flocks? (Check more than one option if have flocks of different stages)/Pre-starter phase (0-7/10 days of age)" ,
         FlockStart="What is the current stage of your flocks? (Check more than one option if have flocks of different stages)/Starter phase (7/10-21 days of age)" ,
         Finisher="What is the current stage of your flocks? (Check more than one option if have flocks of different stages)/Finisher phase (21-42 days of age)", 
         Knowlede_AB= "Are you familiar with the term antibiotics?"  ,                                                                                                                                                 
         AB_Understanding= "According to you, what is it?",                                                                                                                                                                 
         AB_training= "Have you ever received formal training on the appropriate use of antibiotics?" ,
         Source_Training_AB= "Who provided that training?...43",                                                                                                                                                              
         Source_info_AB_Use= "What sources of information do you rely on to learn about antibiotic use in farming? (you can select more than one option)" ,                                                                   
         Agriculture_Service_AB= "What sources of information do you rely on to learn about antibiotic use in farming? (you can select more than one option)/Agricultural association services",                                
         Veterinary_AB= "What sources of information do you rely on to learn about antibiotic use in farming? (you can select more than one option)/Veterinary professionals",                                           
         Local_community_AB= "What sources of information do you rely on to learn about antibiotic use in farming? (you can select more than one option)/Local community meetings" ,                                          
         Online_AB ="What sources of information do you rely on to learn about antibiotic use in farming? (you can select more than one option)/Online resources"  ,                                                 
         Friends_Family_AB= "What sources of information do you rely on to learn about antibiotic use in farming? (you can select more than one option)/Friends and family",                                                 
         Others_AB= "What sources of information do you rely on to learn about antibiotic use in farming? (you can select more than one option)/Others",                                                             
         Specify_OTHERS_SOURCE_AB= "If others please specify", 
         
         AB_Thoughts= "Do you think antibiotic usage can be reduced in your farm by increasing biosecurity/ improved feed/ or vaccination?",                                            
         Heard_AMR= "Have you ever heard of antibiotic resistance?" ,                                                                                                                                                
         Knowledge_AMR="If yes, How familiar are you with the concept of antibiotic resistance" ,                                                                                                                       
         Explanation_AMR= "If very familiar what is it?",                                                                                                                                                                  
         AMR_CONS= "What do you believe are the consequences of antibiotic resistance?"  ,                                                                                                                          
         AMR_CONS_Effectivenes_REDUCED_F_HUMANS= "What do you believe are the consequences of antibiotic resistance?/Reduced effectiveness of antibiotics for humans"  ,                                                                         
         AMR_CONS_Effectivenes_REDUCED_F_ANIMALS="What do you believe are the consequences of antibiotic resistance?/Reduced effectiveness of antibiotics for animals" ,                                                                          
         AMR_CONS_MED_COST=  "What do you believe are the consequences of antibiotic resistance?/Increased medical costs"  ,                                                                                                 
         AMR_CONS_Effectivenes_REDUCED_Farmproductivity= "What do you believe are the consequences of antibiotic resistance?/Reduced farm productivity" ,                                                                                                 
         AMR_CONS_OTHERS= "What do you believe are the consequences of antibiotic resistance?/Others (please specify)" ,                                                                                                   
         AMR_CONS_OTHERS_SPEC= "If others, please specify...62",

         
        Preventing_AMR_Guidelines= "Strictly follow the recommended antibiotic use guidelines or the advice and prescription guidance from veterinarians.",                                                                       
         Preventing_AMR_NO_USAGE ="Don't use antibiotics at all",                                                                                                                                                                                                                                                                                                                            
         Preventing_AMR_Biosecurity =  "Increase biosecurity measures"   ,                                                                                                                                                              
         Preventing_AMR_SwitchBreed =  "Switch to other breeds"  ,                                                                                                                                                                      
         Preventing_AMR_Genral_Husbandry =  "Following good chicken-keeping practices (General husbandry)"   ,                                                                                                                               
         Preventing_AMR_POLICIES=  "Advocate for policies to regulate antibiotic use"      ,                                                                                                                                        
         Peventing_AMR_Others= "If others, please specify and rank."  ,   
         
         
         # The questions in this part seems to be the same as those for administing ABS? - check for consistency
         Usage_OF_AB= "Do you use antimicrobials for your chickens?" ,                                                                                
         Usage_OF_AB_Frequency=  "If yes, how often do you use them?",                                                                               
         Never_usage_Know= "If never,how do you know to administer the antibiotics?"   ,                                                                              
         Never_usage_Know_Drugseller=  "If never,how do you know to administer the antibiotics?/Drug seller tells me",                                                                             
         Never_usage_Know_Label=  "If never,how do you know to administer the antibiotics?/I read the labels",                                                                            
         Never_usage_Know_Neighbour=  "If never,how do you know to administer the antibiotics?/I ask my neighbour",                                                                           
         Never_usage_Know_Myself=  "If never,how do you know to administer the antibiotics?/From experience/I know myself",                                                                         
         number_drugs_2 =  "Number of drugs that the respondent mentioned to use are  ${number_drugs}",                                                                        
         Other_Drugs_last_12Months= "Any other drugs used in the last 12 month",                                                                   
         Ways_of_administer_ABs= "How do you primarily administer antimicrobials to your chicken?",                                                                  
         Ways_of_administer_ABs_Water=  "How do you primarily administer antimicrobials to your chicken?/Water" ,                                                                 
         Ways_of_administer_ABs_Feed= "How do you primarily administer antimicrobials to your chicken?/Feed"   ,                                                                
         Ways_of_administer_ABs_Injection= "How do you primarily administer antimicrobials to your chicken?/Injection"   ,                                                               
         Ways_of_administer_ABs_Other="How do you primarily administer antimicrobials to your chicken?/Other (please specify)"   ,                                                              
         Ways_of_administer_ABs_Specification="If others, please specify...160" 
         ,  
         
         Heard_OH= "Have you ever heard about One Health?" ,                                                                                                                                                        
         Explanation_OH= "If yes, what does it mean?"   ,                                                                                                                                                                 
         Broiler_Disease_Outbreak_Human_Health= "How likely do you think that disease outbreak in broiler farm can affect human health?",                                                                                                        
         Knowledge_Collaborative_OH= "Are you aware of any collaborative efforts between animal health workers, public health and environment officers in your region?"   ,                                                           
         Explaination_Collaborative_OH= "If Yes, mention"  ,                                                                                                                                                                             
         Participation_Training_OH= "Have you ever participated in any training program or workshop related to the OHA?" ,                                                                                                           
         Explain_Training_content_OH= "What was the training about?" ,                                                                                                                                                                 
         Source_Training_OH= "Who provided that training?...77"  ,                                                                                                                                                            
         Concern_AMR_FarmProductivity= "How concerned are you about the potential impact of antibiotic resistance on your farms productivity and livelihood?"  ,                                                                        
         Correct_AB_Usage= "How important do you think  it is to use antibiotics correctly?" ,                                                                                                                              
         Overusage_AB_Poultry= "Do you think antibiotics are overused in the poultry sector?",                                                                                                                                
         AMR_Knowledge_Perception= "What do you think of your knowledge on antibiotics?"  ,                                                                                                                                         
         Know_IF_AB_Usage_AMR= "Do you think that the usage of antibiotics over a long period can create resistance?" ,                                                                                                         
         Know_IF_AB_GrowthP_AMR= "Do you think that the usage of antibiotics as a growth promoter or for preventive purpose can create resistance?",

        Practices_AB_Causes_AMR = "Which of the following antibiotic use practices do you think contribute to antibiotic resistance? (select more than one)" ,                                                                     
         Practices_AB_Causes_AMR_Feed= "Which of the following antibiotic use practices do you think contribute to antibiotic resistance? (select more than one)/option_1__overuse_of_antibiotics_in_feed"  ,                           
         Practices_AB_Causes_AMR_Routine_Profylax= "Which of the following antibiotic use practices do you think contribute to antibiotic resistance? (select more than one)/option_2__routine_prophylactic_use_of_an" ,                            
         Practices_AB_Causes_AMR_Poor_management_AB="Which of the following antibiotic use practices do you think contribute to antibiotic resistance? (select more than one)/option_3__poor_management_of_antibiotic_" ,                            
         Practices_AB_Causes_AMR_Inadeq_monitoring_AB=  "Which of the following antibiotic use practices do you think contribute to antibiotic resistance? (select more than one)/option_4__inadequate_monitoring_of_antib" ,                           
         Practices_AB_Causes_AMR_All= "Which of the following antibiotic use practices do you think contribute to antibiotic resistance? (select more than one)/option_5__all_of_the_above"  ,
         Practices_AB_Causes_AMR_Feed_2 ="Which of the following antibiotic use practices do you think contribute to antibiotic resistance? (select more than one)/Overuse of antibiotics in feed to promote growth" ,                    
         Practices_AB_Causes_AMR__Profylax_2 ="Which of the following antibiotic use practices do you think contribute to antibiotic resistance? (select more than one)/Routine prophylactic use of antibiotics regarding illness presence" ,  
         Practices_AB_Causes_AMR_Poor_management_AB_2 ="Which of the following antibiotic use practices do you think contribute to antibiotic resistance? (select more than one)/Poor management of antibiotic withdrawal period before selling"  ,     
         Practices_AB_Causes_AMR_Inadeq_monitoring_AB_2   ="Which of the following antibiotic use practices do you think contribute to antibiotic resistance? (select more than one)/Inadequate monitoring of antibiotic residues"  ,                      
         Practices_AB_Causes_AMR_All_2   =  "Which of the following antibiotic use practices do you think contribute to antibiotic resistance? (select more than one)/All of the above", 
         
         
         
         
         Administer_AB_Consult_Animal_HEALTH = "Do you consult an animal health practitioner before administering antibiotics?"   ,                                                                                                             
         Administer_AB_Knowhow = "If No, how do you know how to administer the antibiotics?"                                   ,                                                                                                  
         Administer_AB_Drugseller=  "If No, how do you know how to administer the antibiotics?/Drug seller tells me"       ,                                                                                                         
         Administer_AB_Label= "If No, how do you know how to administer the antibiotics?/I read the labels"       ,                                                                                                            
         Administer_AB_Neighbour= "If No, how do you know how to administer the antibiotics?/I ask my neighbor" ,                                                                                                                  
         Administer_AB_Myself = "If No, how do you know how to administer the antibiotics?/From experience/I know myself" ,                                                                                                      
         Administer_AB_OTHER_Farmers_Consult_Animal_HEALTH=  "In general, do you think broiler farmers consult animal health practitioners before administering antibiotics?",  
         
         Purpose_of_Broiler_F= "What is the main purpose of keeping broiler" ,                                                                                                                                                  
         Purpose_of_Broiler_F_Own_Consump= "What is the main purpose of keeping broiler/option_1__own_consumption"  ,                                                                                                                      
         Purpose_of_Broiler_F_Sale= "What is the main purpose of keeping broiler/option_2__mainly_for_sale"   ,                                                                                                                      
         Purpose_of_Broiler_F_Both = "What is the main purpose of keeping broiler/option_3__both"                    ,                                                                                                                
         Purpose_of_Broiler_F_Own_Consump_2 =  "What is the main purpose of keeping broiler/Own consumption"         ,                                                                                                                          
         Purpose_of_Broiler_F__Sale_2 =  "What is the main purpose of keeping broiler/Mainly for sale",                                                                                                                   
         Purpose_of_Broiler_F_Both_2 =  "What is the main purpose of keeping broiler/Both" ,
         
         Feed_use=  "What feed do you use for your broiler?"    ,                                                                                                                                                    
         Feed_use_Crops_Residues =  "What feed do you use for your broiler?/Drain or crop residues"  ,                                                                                                                               
         Feed_use_Houshold_waste = "What feed do you use for your broiler?/Household waste"     ,                                                                                                                                   
         Feed_use_Premix = "What feed do you use for your broiler?/Commercial pre-mixed feed",                                                                                                                              
         Feed_use_Others = "What feed do you use for your broiler?/Other specify"  ,                                                                                                                                       
         Feed_use_Others_Specify_2 = "If others, specify...114" ,
        
        

         
          Time_Since_Chichensick = "When was the last time a chicken/bird was sick?",
                Disease_Last_time= "What kind of disease was it?",                                                                                                                                                               
                Disease_Last_time_Respiratory =   "What kind of disease was it?/Respiratory"   ,                                                                                                                              
                Disease_Last_time_Intestinal= "What kind of disease was it?/Intestinal tract",                                                                                                                             
                Disease_Last_time_Reproductive = "What kind of disease was it?/Reproductive",                                                                                                                            
                Disease_Last_time_SuddenDeath=    "What kind of disease was it?/Sudden death",                                                                                                                           
                Disease_Last_time_Wounds =   "What kind of disease was it?/Wounds",                                                                                                                          
                Disease_Last_time_Parasites =   "What kind of disease was it?/Parasites",                                                                                                                         
                Disease_Last_time_Other=     "What kind of disease was it?/Other specify",                                                                                                                        
                Disease_Last_time_Specification =  "If others, specify...132",

                GPS="Tap to record GPS coordinate of this location"                                                                                                                                                 ,
                GPS_Latitude = "_Tap to record GPS coordinate of this location_latitude"                                                                                                                                       ,
                GPS_Longitude = "_Tap to record GPS coordinate of this location_longitude"                                                                                                                                      ,
                GPS_Altitude =  "_Tap to record GPS coordinate of this location_altitude"                                                                                                                                       ,
                GPS_Precision =  "_Tap to record GPS coordinate of this location_precision"                                                                                                                                      ,
                GPS_2 = "Location"                                                                                                                                                                                      ,
                GPS_Latitude_2 = "_Location_latitude"                                                                                                                                                                            ,
                GPS_Longitude_2 = "_Location_longitude"                                                                                                                                                                           ,
                GPS_Altitude_2 = "_Location_altitude"                                                                                                                                                                            ,
                GPS_Precision_2 =  "_Location_precision"                                                                                                                                                   ,
                GPS_3 = "GPS_Coordinates"                                                                                                                                                                               ,
                GPS_Latitude_3 ="_GPS_Coordinates_latitude"                                                                                                                                                                     ,
                GPS_Longitude_3 = "_GPS_Coordinates_longitude"                                                                                                                                                                    ,
                GPS_Altitude_3  ="_GPS_Coordinates_altitude" ,                                                                                                                                                                                   ,     
                GPS_Precision_3 = "_GPS_Coordinates_precision"  ,
                
                FarmPurp="Primary role of the farm",
                
                Occupation_F2= "Occupation of the farm owner" , 
                FamilySize_2="Family size", 
                FarmYears_2="Number of years farming",
                
                DateI_2="Date_of_Interview_Y_INIKA_OH_TZ_ID"  ,
                Status_R_2 ="Marital_status" ,
                Ethnic_F_2= "Ethinicity" ,
        Disposal_dead_broilers_G ="Disposal of dead broilers",
        Decision_Treatment_G="Decision for treatment",
        AB_Disposal_G="Disposal of antibiotics left over",
        Water_providing_G="Providing water",
        Income_Decision_G="Decisions to use income from broilers",
        Access_InformationAB="Do you feel that you have access to information about antibiotic resistance?",
                NOT_OWNER="If_you_are_not_the_f_on_on_the_farm_owner" ,
                Gender_F_2= "If_you_are_not_the_f_on_on_the_farm_owner/sex_of_the_farm_owner",
                Status_F_2 = "If_you_are_not_the_f_on_on_the_farm_owner/marital_status_of_the_farm_owner",
                Ethnic_F_3 = "If_you_are_not_the_f_on_on_the_farm_owner/ethnicity_of_the_farm_owner",
                Education_F_3 = "If_you_are_not_the_f_on_on_the_farm_owner/education_level_of_the_farm_owner",
                Occupation_F3 = "If_you_are_not_the_f_on_on_the_farm_owner/occupation_of_the_farm_owner",
        
        
        ####################################################################
       # All below has been removed from the dataset as there were no content, !!!!
          
                Location_2="Location_of_farmer"  ,
                Ward_2 = "Location_of_farmer/option_1__ward" ,
                District_2= "Location_of_farmer/option_2__district"  ,
                Region_2="Location_of_farmer/option_3__region" ,
                Breed_2="Type_of_breed_of_chicken" ,
                
                Familar_AMR= "How_familiar_are_you_ntibiotic_resistance"  ,
                
                Familar_AMR_VERY=  "How_familiar_are_you_ntibiotic_resistance/option_1__very_familiar"   ,
                Familar_AMR_SOMEWHAT = "How_familiar_are_you_ntibiotic_resistance/option_2__somewhat_familiar"    ,
                Familar_AMR_NOTATALL = "How_familiar_are_you_ntibiotic_resistance/option_3__not_familiar_at_all"  ,
                
                Familar_AB  = "Are_you_familiar_wit_the_term_antibiotics" ,
                Familar_AB_YES = "Are_you_familiar_wit_the_term_antibiotics/option_1__yes"  ,
                Familar_AB_NO  = "Are_you_familiar_wit_the_term_antibiotics/option_2__no" ,
                Familar_AB_UNSURE  = "Are_you_familiar_wit_the_term_antibiotics/option_3__unsure" ,
                
                TRAINING_ABUSE_NAME = "If_yes_Mention_the_name_of_the_training"  ,
                TRAINING_ABUSE = "Have_you_ever_receiv_e_use_of_antibiotics" ,
                TRAINING_ABUSE_NO ="Have_you_ever_receiv_e_use_of_antibiotics/option_2__no"  ,
                TRAINING_ABUSE_UNSURE = "Have_you_ever_receiv_e_use_of_antibiotics/option_3__unsure"  ,
                
                SOURCE_INFO_ABUSE = "What_sources_of_info_t_more_than_1_option"    ,
                SOURCE_INFO_ABUS_OPT1 = "What_sources_of_info_t_more_than_1_option/option_1"    ,
                SOURCE_INFO_ABUS_OPT2  = "What_sources_of_info_t_more_than_1_option/option_2"  ,
                SOURCE_INFO_ABUS_OPT3  = "What_sources_of_info_t_more_than_1_option/option_3" ,
                SOURCE_INFO_ABUSE_ONLINE = "What_sources_of_info_t_more_than_1_option/online_resources"  ,
                SOURCE_INFO_ABUSE_FriendsFamily = "What_sources_of_info_t_more_than_1_option/friendly_and_family"  ,
                SOURCE_INFO_ABUS_OTHER  = "What_sources_of_info_t_more_than_1_option/other_specify"   ,
                
                AMR_CONS_2 = "What_do_u_believe_ar_ntibiotic_resistance",
                AMR_CONS_Effectivenes_REDUCED_F_HUMANS_2=  "What_do_u_believe_ar_ntibiotic_resistance/reduced_effectives_of_antibiotics_for_hu"   ,
                AMR_CONS_MED_COST_2= "What_do_u_believe_ar_ntibiotic_resistance/increased_medical_costs",
                AMR_CONS_Effectivenes_REDUCED_Farmproductivity_2 = "What_do_u_believe_ar_ntibiotic_resistance/reduced_farm_productivity" ,
                AMR_CONS_OTHERS_2 = "What_do_u_believe_ar_ntibiotic_resistance/other_specify" ,
                
                View_Preventing_AMR = "In_your_view_on_a_sc_ntibiotic_resistance" ,
                View_Preventing_AMR_Guidelines = "In_your_view_on_a_sc_ntibiotic_resistance/strictly_follow_the_recommended_antibiot"  ,
                View_Preventing_AMR_NO_USAGE = "In_your_view_on_a_sc_ntibiotic_resistance/limit_antibiotic_use_all_together___"    ,
                View_Preventing_AMR_Biosecurity = "In_your_view_on_a_sc_ntibiotic_resistance/increase_health_security_measures___"  ,
                View_Preventing_AMR_SwitchBreed = "In_your_view_on_a_sc_ntibiotic_resistance/switch_to_other_breeds___"         ,
                View_Preventing_AMR_Vaccination ="In_your_view_on_a_sc_ntibiotic_resistance/following_vaccination_regime___"    ,
                View_Preventing_AMR_Genral_Husbandry =  "In_your_view_on_a_sc_ntibiotic_resistance/following_good_chicken_keeping_practices" ,
                View_Preventing_AMR_Policy =  "In_your_view_on_a_sc_ntibiotic_resistance/advocate_for_policy_to_regulate_antibiot"   ,
                View_Peventing_AMR_Others = "In_your_view_on_a_sc_ntibiotic_resistance/other_specify"  ,               
                
                
                Explanation_OH = "If yes, what does it mean?"   ,                                                                                                                                                                 
                Broiler_Disease_Outbreak_Human_Health = "How likely do you think that disease outbreak in broiler farm can affect human health?",                                                                                                        
                Knowledge_Collaborative_OH = "Are you aware of any collaborative efforts between animal health workers, public health and environment officers in your region?"   ,                                                           
                Explaination_Collaborative_OH = "If Yes, mention"  ,                                                                                                                                                                             
                Participation_Training_OH = "Have you ever participated in any training program or workshop related to the OHA?" ,                                                                                                           
                Explain_Training_content_OH = "What was the training about?" ,                                                                                                                                                                 
                Source_Training_OH = "Who provided that training?...77"  ,                                                                                                                                                            
                Concern_AMR_FarmProductivity = "How concerned are you about the potential impact of antibiotic resistance on your farms productivity and livelihood?"  ,                                                                        
                Correct_AB_Usage = "How important do you think  it is to use antibiotics correctly?" ,                                                                                                                              
                Overusage_AB_Poultry = "Do you think antibiotics are overused in the poultry sector?"  ,                                                                                                                                
                AMR_Knowledge_Perception = "What do you think of your knowledge on antibiotics?"  ,                                                                                                                                         
                Know_IF_AB_Usage_AMR =  "Do you think that the usage of antibiotics over a long period can create resistance?" ,                                                                                                         
                Know_IF_AB_GrowthP_AMR= "Do you think that the usage of antibiotics as a growth promoter or for preventive purpose can create resistance?" , 
                Heard_OH_2 = "Have_you_ever_heard_ntibiotic_resistance"   ,
                Heard_OH_YES_2 = "Have_you_ever_heard_ntibiotic_resistance/option_1__yes"  ,
                Heard_OH_NO_2 = "Have_you_ever_heard_ntibiotic_resistance/option_2__no" ,
                Heard_OH_UNSURE_2 =  "Have_you_ever_heard_ntibiotic_resistance/option_3__unsure",
                ADM_antibiotics_G="Administering antibiotics",
                Care_Chicken_G="Taking care of the chickens",
                Disposal_G="Disposal of manure/wastes"
                                                                                                                
  )%>%
  
  mutate(GPS = case_when(!is.na(GPS) ~ GPS,
                                     is.na(GPS) ~ GPS_2
  ))%>%
  mutate(GPS_Latitude = case_when(!is.na(GPS_Latitude) ~ GPS_Latitude,
                                               is.na(GPS_Latitude) ~ GPS_Latitude_2
  ))%>%
  
  mutate(GPS_Longitude = case_when(!is.na(GPS_Longitude) ~ GPS_Longitude,
                                                 is.na(GPS_Longitude) ~ GPS_Longitude_2
  ))%>%
  mutate(GPS_Altitude = case_when(!is.na(GPS_Altitude) ~ GPS_Altitude,
                                               is.na(GPS_Altitude) ~ GPS_Altitude_2
  ))%>%
  mutate(GPS_Precision = case_when(!is.na(GPS_Precision) ~ GPS_Precision,
                                                is.na(GPS_Precision) ~ GPS_Precision_2
  ))
  
         