
install.packages(writexl)
library(writexl)


library(readxl)
library (dplyr)
library (tidyverse)


# You need to import the INIKA_SURVEY_ORIGINAL ( which is the downloaded dataset from KObotolbox with your Comments: Test, First Questioanire, Second Questionaire on each row!)
#Use the import function on the right to help you locate and to write the right path.
# This is where I have mine in my computer 
INIKA_SURVEY_ORIGINAL <- read_excel("//vetinst.no/dfs-felles/StasjonK/FAG/Tverrfaglig/AMR/FoU-aktiviteter & prosjekter/31218_INIKA_OH_TZ/WP2/inputDataset/INIKA_SURVEY_ORIGINAL.xlsx")


# Get the name of the columns to copy paste into select; those that you need!, example below)

head(INIKA_SURVEY_ORIGINAL)
colnames(INIKA_SURVEY_ORIGINAL)

#To get the frequencies of any variable with their different values or spellings use the following:
frequency<-table(INIKA_SURVEY_ORIGINAL$"Ethnicity of respondent")
print(frequency)

# then write me in a wordfile how you want the groupings to be
# You can try yourself using mutate and case_when as in the example below. But this is by far not finished yet so needs to be corrected!!!!

# How to filter the Dataset
INIKA<-INIKA_SURVEY_ORIGINAL%>%
  filter(Comment!="TEST")

QUEST<-
 rename(INIKA,Respondent= "Name of the respondent",
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
         Interviewer= "Name of interviewer" ,                                                                                                                                                                        
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
        Specify_OTHERS_SOURCE_AB= "If others please specify"  , 
                                                                                                                                                                                
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
                                                                          
      Preventing_AMR_Guidelines= "Strictly follow the recommended antibiotic use guidelines or the advice and prescription guidance from veterinarians."  ,                                                                       
      Preventing_AMR_NO_USAGE =   "Don't use antibiotics at all",                                                                                                                                                             
      Preventing_AMR_Biosecurity =  "Increase biosecurity measures"   ,                                                                                                                                                              
      Preventing_AMR_SwitchBreed =  "Switch to other breeds"  ,                                                                                                                                                                      
      Preventing_AMR_Genral_Husbandry =  "Following good chicken-keeping practices (General husbandry)"   ,                                                                                                                               
      Preventing_AMR_POLICIES=  "Advocate for policies to regulate antibiotic use"      ,                                                                                                                                        
      Peventing_AMR_Others= "If others, please specify and rank."  ,   
      
      
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
      Overusage_AB_Poultry= "Do you think antibiotics are overused in the poultry sector?"  ,                                                                                                                                
      AMR_Knowledge_Perception= "What do you think of your knowledge on antibiotics?"  ,                                                                                                                                         
      Know_IF_AB_Usage_AMR= "Do you think that the usage of antibiotics over a long period can create resistance?" ,                                                                                                         
      Know_IF_AB_GrowthP_AMR= "Do you think that the usage of antibiotics as a growth promoter or for preventive purpose can create resistance?" , 
      
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
       
      Administer_AB_OTHER_Farmers_Consult_Animal_HEALTH=  "In general, do you think broiler farmers consult animal health practitioners before administering antibiotics?"  ,  
      
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
    Feed_use_Others_Specify_2 = "If others, specify...114"  ,

   rename(Time_Since_Chichensick = "When was the last time a chicken/bird was sick?",
          Disease_Last_time= "What kind of disease was it?"   ,                                                                                                                                                               
          Disease_Last_time_Respiratory =   "What kind of disease was it?/Respiratory"   ,                                                                                                                              
          Disease_Last_time_Intestinal= "What kind of disease was it?/Intestinal tract",                                                                                                                             
          Disease_Last_time_Reproductive = "What kind of disease was it?/Reproductive",                                                                                                                            
          Disease_Last_time_SuddenDeath=    "What kind of disease was it?/Sudden death"                          ,                                                                                                                           
          Disease_Last_time_Wounds =   "What kind of disease was it?/Wounds"                                 ,                                                                                                                          
          Disease_Last_time_Parasites =   "What kind of disease was it?/Parasites"                               ,                                                                                                                         
          Disease_Last_time_Other=     "What kind of disease was it?/Other specify"                            ,                                                                                                                        
          Disease_Last_time_Specification =  "If others, specify...132"  ,

Manage_Healthy_Chicken_Cleaning_OR_DIsinfect= "What do you do to keep your chicken healthy, so they don't get sick? (don't read options)/Clean or disinfect",                                                                            
Manage_Healthy_Chicken_Vaccine_Drugs= "What do you do to keep your chicken healthy, so they don't get sick? (don't read options)/Use vet drugs (eg.vaccine)",                                                                   
Manage_Healthy_Chicken_Well_Fed= "What do you do to keep your chicken healthy, so they don't get sick? (don't read options)/Keep well fed",                                                             
Manage_Healthy_Chicken_Spec_Fed="What do you do to keep your chicken healthy, so they don't get sick? (don't read options)/Giving special feed",                                                           
Manage_Healthy_Chicken_Fencing= "What do you do to keep your chicken healthy, so they don't get sick? (don't read options)/Fencing",                                                          
Manage_Healthy_Chicken_Others= "What do you do to keep your chicken healthy, so they don't get sick? (don't read options)/Other specify",    
    
    
Disease_management="What did you do when your chicken was sick? (several options)"           ,                                                                                                                      
Disease_management_Traditional= "What did you do when your chicken was sick? (several options)/Use traditional medicine"                  ,                                                                                      
 Disease_management_Medicine_Drigstore="What did you do when your chicken was sick? (several options)/Use medicine from veterinary drug store"    ,                                                                                     
 Disease_management_Consult_animal_health= "What did you do when your chicken was sick? (several options)/Consult Community animal health worker"      ,                                                                                    
Disease_management_Consult_private_Veterinarian= "What did you do when your chicken was sick? (several options)/Consult a private veterinarian"               ,                                                                                   
 Disease_management_Others= "What did you do when your chicken was sick? (several options)/Other specify.."                               ,                                                                                  
 Disease_management_Specification = "If others, specify...139"             ,
    
   # The questions in this part seems to be the same as those for administing ABS? - check for consistency
  Usage_OF_AB= "Do you use antimicrobials for your chickens?"                                                                  ,                                                                                
  Usage_OF_AB_Frequency=  "If yes, how often do you use them?"                                                                             ,                                                                               
  Never_usage_Know= "If never,how do you know to administer the antibiotics?"                                                         ,                                                                              
  Never_usage_Know_Drugseller=  "If never,how do you know to administer the antibiotics?/Drug seller tells me"                                     ,                                                                             
  Never_usage_Know_Label=  "If never,how do you know to administer the antibiotics?/I read the labels"                                         ,                                                                            
  Never_usage_Know_Neighbour=  "If never,how do you know to administer the antibiotics?/I ask my neighbour"                                         ,                                                                           
  Never_usage_Know_Myself=  "If never,how do you know to administer the antibiotics?/From experience/I know myself"                               , 
    
                                                                                                         ,                                                                         
    number_drugs_2 =  "Number of drugs that the respondent mentioned to use are  ${number_drugs}"                                             ,                                                                        
    Other_Drugs_last_12Months= "Any other drugs used in the last 12 month"                                                                                  ,                                                                   
        
    Ways_of_administer_ABs= "How do you primarily administer antimicrobials to your chicken?"                                                             ,                                                                  
    Ways_of_administer_ABs_Water=  "How do you primarily administer antimicrobials to your chicken?/Water"                                                        ,                                                                 
    Ways_of_administer_ABs_Feed= "How do you primarily administer antimicrobials to your chicken?/Feed"                                                          ,                                                                
    Ways_of_administer_ABs_Injection= "How do you primarily administer antimicrobials to your chicken?/Injection"                                                      ,                                                               
    Ways_of_administer_ABs_Other="How do you primarily administer antimicrobials to your chicken?/Other (please specify)"                                          ,                                                              
    Ways_of_administer_ABs_Specification="If others, please specify...160"         ,                                                             
        
   Source_OF_OBTAIN_ABs= "Where do you obtain antibiotics?"                       , 
  Source_OF_OBTAIN_ABs_Formal_Vet="Where do you obtain antibiotics?/option_1__formal_veterinary_channels__e_"                                                          ,                                                           
  Source_OF_OBTAIN_ABs_informal_local ="Where do you obtain antibiotics?/option_2__informal_sources__e_g__local_m"                                                           ,                                                          
 Source_OF_OBTAIN_ABs_Formal_Vet_2 = "Where do you obtain antibiotics?/Formal veterinary channels (e.g., veterinarians, pharmacies)" ,                                                         
Source_OF_OBTAIN_ABs_informal_local_2 = "Where do you obtain antibiotics?/Informal sources (e.g., local markets, unauthorized vendors)"                                         ,  
    
  Jugde_AB_Treatment="Who judges the necessity of an antibiotic treatment in your farm? (You may select more than one option)"                                ,                                                       
  Jugde_AB_Treatment_Vet= "Who judges the necessity of an antibiotic treatment in your farm? (You may select more than one option)/Veterinarian"                    ,                                                      
  Jugde_AB_Treatment_ParaVet ="Who judges the necessity of an antibiotic treatment in your farm? (You may select more than one option)/Para veterinarian"                ,                                                     
  Jugde_AB_Treatment_Drug_vendor=   "Who judges the necessity of an antibiotic treatment in your farm? (You may select more than one option)/Drug vendors"                      ,                                                    
  Jugde_AB_Treatment_Owner=  "Who judges the necessity of an antibiotic treatment in your farm? (You may select more than one option)/Owner"                              ,                                                   
  Jugde_AB_Treatment_Others=  "Who judges the necessity of an antibiotic treatment in your farm? (You may select more than one option)/Other"                               ,                                                  
  Jugde_AB_Treatment_Specification =  "if others, please specify" ,                                                                                                                                                                    
        
    Means_of_Decion_ABUSAGE= "How does he/she reach at the judgment of advising you to use antibiotics on your farm"                                                        ,                                                 
    Means_of_Decion_ABUSAGE_FARMVISIT ="How does he/she reach at the judgment of advising you to use antibiotics on your farm/option_1__visiting_the_farm"                             ,                                                
    Means_of_Decion_ABUSAGE_EXPLAIN_CLINICAL="How does he/she reach at the judgment of advising you to use antibiotics on your farm/option_2__explaining_the_clinical_signs_"                 ,                                               
    Means_of_Decion_ABUSAGE_PREVIOUS_EXP= "How does he/she reach at the judgment of advising you to use antibiotics on your farm/option_3__basing_on_previous_visit_exper"                  ,                                              
    Means_of_Decion_ABUSAGE_PHOTO ="How does he/she reach at the judgment of advising you to use antibiotics on your farm/option_4__sending_a_photo_of_dead_and_bi"                   ,                                             
    Means_of_Decion_ABUSAGE_FARMVISIT_2 ="How does he/she reach at the judgment of advising you to use antibiotics on your farm/Visiting the farm"                                           ,                                            
    Means_of_Decion_ABUSAGE_EXPLAIN_CLINICAL_2 ="How does he/she reach at the judgment of advising you to use antibiotics on your farm/Explaining the clinical signs to him/her"                     ,                                           
    Means_of_Decion_ABUSAGE_PREVIOUS_EXP_2= "How does he/she reach at the judgment of advising you to use antibiotics on your farm/Basing on previous visit experience"                           ,                                          
    Means_of_Decion_ABUSAGE_PHOTO_2 ="How does he/she reach at the judgment of advising you to use antibiotics on your farm/Sending a photo of dead and birds showing clinical signs"       ,                                         
        
    
    PLACE_OF_AB_PURHASE= "Where do you purchase your drugs?"                                                                                                                     ,                                        
    PLACE_OF_AB_PURHASE_VETPHARMACY= "Where do you purchase your drugs?/Veterinary pharmacy"                                                                                                  ,                                       
    PLACE_OF_AB_PURHASE_VILLAGE_PHARMACY= "Where do you purchase your drugs?/small village pharmacy"                                                                                                ,                                      
    PLACE_OF_AB_PURHASE_LOCAL_MARKET= "Where do you purchase your drugs?/Local market"                                                                                                           ,                                     
    PLACE_OF_AB_PURHASE_FARMER_COLLEGES= "Where do you purchase your drugs?/Farmer colleague"                                                                                                        ,                                    
    PLACE_OF_AB_PURHASE_OTHER= "Where do you purchase your drugs?/Other"                                                                                                                    ,                                   
    PLACE_OF_AB_PURHASE_SPECIFICATION = "If others, please specify...188"                                                                                                                             ,
    
    Follow_Dosage_AB= "Do you typically follow recommended dosage when using antibiotics?"                                                                                           ,
    Records_AB= "Do you keep records of antibiotics administered?"                                                                                                              ,                                
    Follow_Withdrawal_AB  =   "Do you typically follow recommended withdrawal periods when using antibiotics?"                                                                                 ,                               
    Follow_Withdrawal_AB_Other_Farmers  =  "Do you think broiler farmers in general follow recommended dosages and withdrawal periods when using antibiotics?"                                               ,                              
    Alternative_TO_ABUSAGE= "Are there any other alternatives apart from the use of antibiotics?"                                                                                              ,                             
    ALternatives_AB_Description="If yes what would they be?"                                                                                                                                        , 
    
    
    Barriers_To_Adopt_AB_Alternatives ="What barriers do you face in adopting alternative practices that reduce the need for antibiotic use? (Select all that apply)"                                       ,
    Barriers_To_Adopt_AB_Alternatives_Knowledge_Alternative = "What barriers do you face in adopting alternative practices that reduce the need for antibiotic use? (Select all that apply)/Lack of knowledge about alternative practices" ,                   
    Barriers_To_Adopt_AB_Alternatives_resources_Alternative ="What barriers do you face in adopting alternative practices that reduce the need for antibiotic use? (Select all that apply)/Limited access to resources for alternative practices"  ,          
    Barriers_To_Adopt_AB_Alternatives_traditionalpractices = "What barriers do you face in adopting alternative practices that reduce the need for antibiotic use? (Select all that apply)/Cultural or traditional practices"                       ,         
    Barriers_To_Adopt_AB_Alternatives_Economic="What barriers do you face in adopting alternative practices that reduce the need for antibiotic use? (Select all that apply)/Economic constraints"                                        ,     
    Barriers_To_Adopt_AB_Alternatives_Other="What barriers do you face in adopting alternative practices that reduce the need for antibiotic use? (Select all that apply)/Other (please specify)"                                       ,    
    Barriers_To_Adopt_AB_Alternatives_Specification= "If others, please specify...201"                                                                                                                                                            ,
    
    INFLUENCE_CHOICE_OF_AB="What influences your choice of antibiotics to use? more than one response is allowed"                                                                                                        , 
    INFLUENCE_CHOICE_OF_AB_PRICE= "What influences your choice of antibiotics to use? more than one response is allowed/option_1__price"                                                                                         , 
    INFLUENCE_CHOICE_OF_AB_AVALIABILITY="What influences your choice of antibiotics to use? more than one response is allowed/option_2__availability"                                                                                   ,
    INFLUENCE_CHOICE_OF_AB_EFFICIENCY= "What influences your choice of antibiotics to use? more than one response is allowed/option_3__efficiency"                                                                                     ,
    INFLUENCE_CHOICE_OF_AB_PRESCRIPTION= "What influences your choice of antibiotics to use? more than one response is allowed/option_4__prescription_by_the_veterinari"                                                                 ,
    INFLUENCE_CHOICE_OF_AB_PRESCR_LOCAL_MARKET_VENDOR ="What influences your choice of antibiotics to use? more than one response is allowed/option_5__prescription_by_the_local_mark"                                                                 ,
    INFLUENCE_CHOICE_OF_AB_2= "What influences your choice of antibiotics to use? more than one response is allowed/option_6__prescription_by_the_provender_", 
    INFLUENCE_CHOICE_OF_AB_PRICE_2= "What influences your choice of antibiotics to use? more than one response is allowed/Price"                                                                                                    ,
    INFLUENCE_CHOICE_OF_AB_AVALIABILITY_2= "What influences your choice of antibiotics to use? more than one response is allowed/Availability"                                                                                             ,
    INFLUENCE_CHOICE_OF_AB_EFFICIENCY_2= "What influences your choice of antibiotics to use? more than one response is allowed/Efficiency"                                                                                               ,
    INFLUENCE_CHOICE_OF_AB_PRESCRIPTION_2=  "What influences your choice of antibiotics to use? more than one response is allowed/Prescription by the veterinarian"                                                                         ,
    INFLUENCE_CHOICE_OF_AB_PRESCR_LOCAL_MARKET_VENDOR_2 ="What influences your choice of antibiotics to use? more than one response is allowed/Prescription by the local market vendor"                                                                  ,
    INFLUENCE_CHOICE_OF_AB_PRESCR_Provender_Seller =    "What influences your choice of antibiotics to use? more than one response is allowed/Prescription by the provender seller"                                                                     ,
        
   AB_Status_Now= "Are your chickens under antibiotic treatment now?"                                                                                                                                             ,
  Determine_Dose_AB= "How do you determine the dose/quantity of the antibiotic? (can provide more than one answer)"                                                                                                  ,
  Determine_Dose_AB_Animal_Health_P= "How do you determine the dose/quantity of the antibiotic? (can provide more than one answer)/option_1__according_to_animal_health_pra"                                                         ,
  Determine_Dose_AB_Manufacturer=     "How do you determine the dose/quantity of the antibiotic? (can provide more than one answer)/option_2__according_to_manufacturers__in"                                                         ,
    Determine_Dose_AB_Weighing=      "How do you determine the dose/quantity of the antibiotic? (can provide more than one answer)/option_3__by_weighing_the_birds"                                                                  ,
    Determine_Dose_AB_Estimate_Weight=  "How do you determine the dose/quantity of the antibiotic? (can provide more than one answer)/option_4__by_estimating_the_weight_of_th"                                                         ,
    Determine_Dose_AB_Age=   "How do you determine the dose/quantity of the antibiotic? (can provide more than one answer)/option_5__according_to_their_age"                                                                 ,
    Determine_Dose_AB_DONTKNOW= "How do you determine the dose/quantity of the antibiotic? (can provide more than one answer)/option_6__don_t_know"                                                                             ,
    Determine_Dose_AB_OTHERSSPEC=    "How do you determine the dose/quantity of the antibiotic? (can provide more than one answer)/option_7__others_specify"   
    ,
    Determine_Dose_AB_Animal_Health_P_2="How do you determine the dose/quantity of the antibiotic? (can provide more than one answer)/According to animal health practitioners instructions"                                            ,
    Determine_Dose_AB_Manufacturer_2=   "How do you determine the dose/quantity of the antibiotic? (can provide more than one answer)/According to manufacturers' instructions"                                                         ,
    Determine_Dose_AB_Weighing_2=  "How do you determine the dose/quantity of the antibiotic? (can provide more than one answer)/By weighing the birds"                                                                            ,
    Determine_Dose_AB_Estimate_Weight_2=  "How do you determine the dose/quantity of the antibiotic? (can provide more than one answer)/By estimating the weight of the birds"                                                            ,
    Determine_Dose_AB_Age_2=  "How do you determine the dose/quantity of the antibiotic? (can provide more than one answer)/According to their age"                                                                           ,
    Determine_Dose_AB_DONTKNOW_2=  "How do you determine the dose/quantity of the antibiotic? (can provide more than one answer)/Don't know"                                                                                       ,
    Determine_Dose_AB_OTHERSSPEC_2=    "How do you determine the dose/quantity of the antibiotic? (can provide more than one answer)/Others specify"                                                                                   ,
    
  Treatment_Failure_Repeat_Same= "In case of treatment failure, do you repeat the same treatment?"                                                                                                                               ,
  Treatment_Failure_Options= "In case of persistence of symptoms after first treatment, what do you do?( you select more than one option)"                                                                                   ,
  Treatment_Failure_Option_Increase_Dose= "In case of persistence of symptoms after first treatment, what do you do?( you select more than one option)/Increase the dosage of the same treatment"                                         ,
  Treatment_Failure_Options_Increase_Duration=  "In case of persistence of symptoms after first treatment, what do you do?( you select more than one option)/Increase the duration of the same treatment"                                       ,
  Treatment_Failure_Options_Change_Drug=   "In case of persistence of symptoms after first treatment, what do you do?( you select more than one option)/Change the drug"                                                                   ,
  Treatment_Failure_Options_Mixture_Drugs=  "In case of persistence of symptoms after first treatment, what do you do?( you select more than one option)/Prescribe a mixture of drugs"                                                      ,
  Treatment_Failure_Options_Sell_ASAP="In case of persistence of symptoms after first treatment, what do you do?( you select more than one option)/Commercialization ( Sell the chicken as quickly as possible before more are dying)",
  Treatment_Failure_Options_Inditgenous_Treat="In case of persistence of symptoms after first treatment, what do you do?( you select more than one option)/Indigenous treatment"                                                              ,
  Treatment_Failure_Options_Others= "In case of persistence of symptoms after first treatment, what do you do?( you select more than one option)/Others (specify)" ,
  Treatment_Failure_Options_Specification= "If others, please specify...240"                                                                                                          ,
       
  Frequenc_Same_Treatment_BeforeChange="How many times do you apply the same treatment before changing for a possible alternative?"                                                                                                    ,
  Respect_the_Treatment_Guidelines_by_AHP="Do you respect/align with the treatment guidelines prescribed by the Animal health practitioners?"                                                                                             ,
  Respect_Withdrawal= "Do you consider and adhere to the withdraw period?"                                                                                                                                            ,
  Barriers_Withdrawal= "If No, why is it difficult to consider/apply the withdraw period?"                                                                                                                             ,
  Mix_ABS_EFFECIVENESS= "Do you combine several antibiotics to increase efficiency?"                                                                                                                                    ,
        
    Management_DEADBIRDS="What do you do with the dead chickens ?"                                                                                                                                                       ,
    Management_DEADBIRDS_Incinerate= "What do you do with the dead chickens ?/Incinerate"                                                                                                                                            ,
    Management_DEADBIRDS_IN_PIT= "What do you do with the dead chickens ?/Throw in the pit"                                                                                                                                      ,
    Management_DEADBIRDS_Dustbin= "What do you do with the dead chickens ?/Throw in the dustbin"                                                                                                                                  ,
    Management_DEADBIRDS_Consume= "What do you do with the dead chickens ?/Consume"                                                                                                                                               ,
    Management_DEADBIRDS_Domesticated_ANIMALS= "What do you do with the dead chickens ?/Give to other domesticated animals"                                                                                                                    ,
    Management_DEADBIRDS_Other= "What do you do with the dead chickens ?/Other specify"                                                                                                                                         ,
    Management_DEADBIRDS_Specification= "If others, please specify...253"                                                                                                                                                               ,
        
 Selling_DURING_TREATEMENT="During the treatment period, do you continue selling chickens?"                                                                                                                                ,
Selling_DURING_TREATEMENT_OTHERFARMERS= "In general, do broiler farmers continue selling the chickens during the treatment period?"                                                                                                     ,
       
    Treatment_Failure_BIRDS= "When the treatment is not working, what do you do with the chickens?"                                                                                                                          ,
    Treatment_Failure_BIRD_SaleS= "When the treatment is not working, what do you do with the chickens?/Sale"                                                                                                                     ,
    Treatment_Failure_BIRDS_Slaughter= "When the treatment is not working, what do you do with the chickens?/Slaughter"                                                                                                                ,
    Treatment_Failure_BIRDS_Other= "When the treatment is not working, what do you do with the chickens?/Other (Specify)"                                                                                                          ,
    Treatment_Failure_BIRDS_Specification=   "If others, please specify...260"                                                                                                                                                               ,
    
    Expired_Drugs= "What do you do with expired drugs?"                                                                                                                                                            ,
    Expired_Drugs_Pharmacy=  "What do you do with expired drugs?/option_1__back_to_pharmacy"                                                                                                                                 ,
    Expired_Drugs_Nature=  "What do you do with expired drugs?/option_2__thrown_in_nature"                                                                                                                                 ,
    Expired_Drugs_Conserve=  "What do you do with expired drugs?/option_3__conserve"                                                                                                                                         ,
    Expired_Drugs_Animals=  "What do you do with expired drugs?/option_4__administer_to_anima"                                                                                                                              ,
    Expired_Drugs_Pharmacy_2= "What do you do with expired drugs?/Take back to pharmacy"                                                                                                                                      ,
    Expired_Drugs_Nature_2=  "What do you do with expired drugs?/Thrown in nature"                                                                                                                                           ,
    Expired_Drugs_Conserve_2=  "What do you do with expired drugs?/Conserve"                                                                                                                                                   ,
    Expired_Drugs_Animals_2=   "What do you do with expired drugs?/Administer to animal",
        
    AB_Information_access= "Do you feel that you have access to information about antibiotic resistance?"                                                                                                                  ,
    ABUsage_Learning="Do you think you are willing to invest time and resources in learning about responsible antibiotic use practices?"                                                                             ,
    ABUsage_Participation_AB=    "Would you be interested in participating in workshops or training sessions on sustainable anti practices, including antibiotic use?"                                                           ,
       
     Support= "Have you received any support from? (can select multiple)"                                                                                                                                     ,
    Support_Family="Have you received any support from? (can select multiple)/Close family"                                                                                                                        ,
    Support_Relatives=  "Have you received any support from? (can select multiple)/Relatives"                                                                                                                           ,
    Support_Friends=  "Have you received any support from? (can select multiple)/Friends"                                                                                                                             ,
    Support_Neighbors=  "Have you received any support from? (can select multiple)/Neighbors"                                                                                                                           ,
    Support_NCANGO =  "Have you received any support from? (can select multiple)/NCA/NGOs"                                                                                                                            ,
    Support_Local=  "Have you received any support from? (can select multiple)/Local social/agricultural unions"                                                                                                    ,
    Support_Government=  "Have you received any support from? (can select multiple)/Government"                                                                                                                          ,
    Support_None=  "Have you received any support from? (can select multiple)/No support"                                                                                                                          ,
   
    Rating_Support="How would you rate the importance of the support you received from each of the selected above?...282"                                                                                          ,
        
    Source_AMR= "What are the main sources of your information regarding AMR? (can select multiple)"                                                                                                            ,
    Source_AMR_Family=  "What are the main sources of your information regarding AMR? (can select multiple)/option_1__close_family"                                                                                     ,
    Source_AMR_Relatives=  "What are the main sources of your information regarding AMR? (can select multiple)/option_2__relatives"                                                                                        ,
    Source_AMR_Friends=  "What are the main sources of your information regarding AMR? (can select multiple)/option_3_friends"                                                                                           ,
    Source_AMR_Neighbors=  "What are the main sources of your information regarding AMR? (can select multiple)/option_4__neighbors"                                                                                        ,
    Source_AMR_Local_Agriculture=     "What are the main sources of your information regarding AMR? (can select multiple)/option_5__local_social_agricultural_asso"                                                                   ,
    Source_AMR_LocalGov=       "What are the main sources of your information regarding AMR? (can select multiple)/option_6__local_government"                                                                                 ,
    Source_AMR_NCA_NGOs=       "What are the main sources of your information regarding AMR? (can select multiple)/option_7__nca_ngos"                                                                                         ,
    Source_AMR_Nation_Gov=    "What are the main sources of your information regarding AMR? (can select multiple)/option_8__national_government"                                                                              ,
    Source_AMR_Media= "What are the main sources of your information regarding AMR? (can select multiple)/option_9__media"                                                                                            ,
    Source_AMR_None=   "What are the main sources of your information regarding AMR? (can select multiple)/option_10__no_information"   ,
    
    Source_AMR_Family_2=  "What are the main sources of your information regarding AMR? (can select multiple)/Close family"                                                                                               ,
    Source_AMR_Relatives_2= "What are the main sources of your information regarding AMR? (can select multiple)/Relatives"                                                                                                  ,
    Source_AMR_Friends_2= "What are the main sources of your information regarding AMR? (can select multiple)/Friends"                                                                                                    ,
    Source_AMR_Neighbors_2= "What are the main sources of your information regarding AMR? (can select multiple)/Neighbors"                                                                                                  ,
    Source_AMR_Local_Agriculture_2=  "What are the main sources of your information regarding AMR? (can select multiple)/Local social/agricultural associations"                                                                     ,
    Source_AMR_LocalGov_2= "What are the main sources of your information regarding AMR? (can select multiple)/Local government"                                                                                           ,
    Source_AMR_NCA_NGOs_2=  "What are the main sources of your information regarding AMR? (can select multiple)/NCA/NGOs"                                                                                                   ,
    Source_AMR_Nation_Gov_2= "What are the main sources of your information regarding AMR? (can select multiple)/National government"                                                                                        ,
    Source_AMR_Media_2=  "What are the main sources of your information regarding AMR? (can select multiple)/Media"                                                                                                      ,
    Source_AMR_None_2=  "What are the main sources of your information regarding AMR? (can select multiple)/No information",
    
    Rate_Support_2= "How would you rate the importance of the support you received from each of the selected above?...304"                                                                                          ,
    
    Local_engagement_Community="Are you currently involved in a local community/association?"                                                                                                                                  ,
    Freq_Participation= "How often do you participate?"                                                                                                                                                                 ,
    Participation_Usefulnes= "Is it helpful for you to participate in those organizations?"                                                                                                                                  ,
    Employment_Status= "What is your current employment status?"                                                                                                                                                       ,
    Income_Source="What is your primary source of income?"  ,
    Income_Source_other = "If others, please specify...310"                                                                                                                                                               ,
    Income_Broiler_main = "Is broiler farming your main income?"                                                                                                                                                          ,
    NOchicken_SoldPerMonth ="How many chickens do you approximately sell per month?"                                                                                                                                        ,
    Credit_LOans = "Do you have access to credit or loans for your farming activities?"                                                                                                                            ,
    DependFarmingINcome = "How many family members depend on your broiler farming income?"                                                                                                                                ,
    Gender_roles_BroilerFarming = "In your own words, how do you perceive the roles of men and women in broiler farming and within the broader framework of AMU practices? Who is involved more?"                                 ,
    Gender_roles_BroilerFarming_Own =   "Owning farms"                                                                                                                                                                                  ,
    Gender_roles_BroilerFarming_Clean =   "Cleaning farmhouse/sheds."                                                                                                                                                                     ,
    Gender_roles_BroilerFarming_Selling = "Selling the chickens"                                                                                                                                                                          ,
    Gender_roles_BroilerFarming_Adm_AB ="Administering antibiotics"                                                                                                                                                                     ,
    Gender_roles_BroilerFarming_Feeding = "Feeding chickens"                                                                                                                                                                              ,
    Gender_roles_BroilerFarming_CARE =  "Taking care of the chickens"                                                                                                                                                                   ,
    Gender_roles_BroilerFarming_Disp_Dead ="Disposal of dead broilers"                                                                                                                                                                     ,
    Gender_roles_BroilerFarming_Disp_Manure =  "Disposal of manure/wastes"                                                                                                                                                                     ,
    Gender_roles_BroilerFarming_TreatmentDecision ="Decision for treatment"                                                                                                                                                                        ,
    Gender_roles_BroilerFarming_Water ="Providing water"                                                                                                                                                                               ,
    Gender_roles_BroilerFarming_Disp_ABs = "Disposal of antibiotics left over"                                                                                                                                                             ,
    Gender_roles_BroilerFarming_IncomeDec ="Decisions to use income from broilers"                                                                                                                                                         ,
    Gender_Reasons = "In your opinion, what are the reasons for males/females being involved more in broiler farmers?"                                                                                               ,
       
      Goverment_FocusAreas = "Which of the following issues do you think the Tanzanian government should focus on? (select a maximum of three)"                                                                             , 
      Goverment_FocusAreas_ClimateChange = "Which of the following issues do you think the Tanzanian government should focus on? (select a maximum of three)/Tackling climate change"                                                      ,
      Goverment_FocusAreas_Taxing_Multinationalcompanies = "Which of the following issues do you think the Tanzanian government should focus on? (select a maximum of three)/Taxing large multinational companies"                                         ,
      Goverment_FocusAreas_Tackling_AMR = "Which of the following issues do you think the Tanzanian government should focus on? (select a maximum of three)/Tackling AMR"                                                                 ,
      Goverment_FocusAreas_Reduce_Unemployment = "Which of the following issues do you think the Tanzanian government should focus on? (select a maximum of three)/Reducing unemployment"                                                        ,
      Goverment_FocusArea_AgrigDevelopments = "Which of the following issues do you think the Tanzanian government should focus on? (select a maximum of three)/Investing in agricultural development"                                        ,
      Goverment_FocusAreas_NextPandemic = "Which of the following issues do you think the Tanzanian government should focus on? (select a maximum of three)/Preparing for the next pandemic"                                              ,
      Goverment_FocusAreas_Migration = "Which of the following issues do you think the Tanzanian government should focus on? (select a maximum of three)/Managing migration"                                                           ,
      Goverment_FocusAreas_Protect_Refugees = "Which of the following issues do you think the Tanzanian government should focus on? (select a maximum of three)/Protecting refugees"                                                          ,
      Goverment_FocusAreas_Reduce_Inequity = "Which of the following issues do you think the Tanzanian government should focus on? (select a maximum of three)/Reducing inequality and discrimination"                                       ,
      Goverment_FocusAreas_Misinformation = "Which of the following issues do you think the Tanzanian government should focus on? (select a maximum of three)/Tackling fake news and misinformation"                                        ,
      Goverment_FocusAreas_Crime =  "Which of the following issues do you think the Tanzanian government should focus on? (select a maximum of three)/Fighting crime and terrorism"                                                 ,
      Goverment_FocusAreas_Other =  "Which of the following issues do you think the Tanzanian government should focus on? (select a maximum of three)/Other"                                                                        ,
      Goverment_FocusAreas_Specification = "If others, please specify...358"                                                                                                                                                               ,
        
         
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
      Status_R_2 ="Marital_status"                                                                                                                                                                                ,
      Ethnic_F_2= "Ethinicity"                                                                                                                                                                                    ,
      NOT_OWNER="If_you_are_not_the_f_on_on_the_farm_owner"                                                                                                                                                     ,
      Gender_F_2= "If_you_are_not_the_f_on_on_the_farm_owner/sex_of_the_farm_owner"                                                                                                                               ,
       Status_F_2 = "If_you_are_not_the_f_on_on_the_farm_owner/marital_status_of_the_farm_owner"                                                                                                                    ,
        Ethnic_F_3 = "If_you_are_not_the_f_on_on_the_farm_owner/ethnicity_of_the_farm_owner"                                                                                                                         ,
      Education_F_3 = "If_you_are_not_the_f_on_on_the_farm_owner/education_level_of_the_farm_owner"                                                                                                                   ,
      Occupation_F3 = "If_you_are_not_the_f_on_on_the_farm_owner/occupation_of_the_farm_owner",
      
  Location_2="Location_of_farmer"                                                                                                                                                                            ,
   Ward_2 = "Location_of_farmer/option_1__ward"                                                                                                                                                             ,
    District_2= "Location_of_farmer/option_2__district"                                                                                                                                                         ,
    Region_2="Location_of_farmer/option_3__region" ,
     Breed_2="Type_of_breed_of_chicken"                                                                                                                                                                      ,
      
      Familar_AMR= "How_familiar_are_you_ntibiotic_resistance"  ,
      
      Familar_AMR_VERY=  "How_familiar_are_you_ntibiotic_resistance/option_1__very_familiar"                                                                                                                             ,
      Familar_AMR_SOMEWHAT = "How_familiar_are_you_ntibiotic_resistance/option_2__somewhat_familiar"                                                                                                                         ,
      Familar_AMR_NOTATALL = "How_familiar_are_you_ntibiotic_resistance/option_3__not_familiar_at_all"                                                                                                                       ,
      
      Familar_AB  = "Are_you_familiar_wit_the_term_antibiotics"                                                                                                                                                     ,
      Familar_AB_YES = "Are_you_familiar_wit_the_term_antibiotics/option_1__yes"                                                                                                                                       ,
      Familar_AB_NO  = "Are_you_familiar_wit_the_term_antibiotics/option_2__no"                                                                                                                                        ,
      Familar_AB_UNSURE  = "Are_you_familiar_wit_the_term_antibiotics/option_3__unsure"                                                                                                                                    ,
      
      TRAINING_ABUSE_NAME = "If_yes_Mention_the_name_of_the_training"                                                                                                                                                       ,
      TRAINING_ABUSE = "Have_you_ever_receiv_e_use_of_antibiotics"                                                                                                                                                     ,
      TRAINING_ABUSE_YES = "Have_you_ever_receiv_e_use_of_antibiotics/option_1__yes"                                                                                                                                       ,
      TRAINING_ABUSE_NO ="Have_you_ever_receiv_e_use_of_antibiotics/option_2__no"                                                                                                                                        ,
      TRAINING_ABUSE_UNSURE = "Have_you_ever_receiv_e_use_of_antibiotics/option_3__unsure"                                                                                                                                    ,
      
    SOURCE_INFO_ABUSE = "What_sources_of_info_t_more_than_1_option"                                                                                                                                                     ,
    SOURCE_INFO_ABUS_OPT1 = "What_sources_of_info_t_more_than_1_option/option_1"                                                                                                                                            ,
      SOURCE_INFO_ABUS_OPT2  = "What_sources_of_info_t_more_than_1_option/option_2"                                                                                                                                            ,
      SOURCE_INFO_ABUS_OPT3  = "What_sources_of_info_t_more_than_1_option/option_3"                                                                                                                                            ,
      SOURCE_INFO_ABUSE_ONLINE = "What_sources_of_info_t_more_than_1_option/online_resources"                                                                                                                                    ,
      SOURCE_INFO_ABUSE_FriendsFamily = "What_sources_of_info_t_more_than_1_option/friendly_and_family"                                                                                                                                 ,
      SOURCE_INFO_ABUS_OTHER  = "What_sources_of_info_t_more_than_1_option/other_specify"                                                                                                                                       ,
    
      AMR_CONS_2 = "What_do_u_believe_ar_ntibiotic_resistance"                                                                                                                                                     ,
      AMR_CONS_Effectivenes_REDUCED_F_HUMANS_2=  "What_do_u_believe_ar_ntibiotic_resistance/reduced_effectives_of_antibiotics_for_hu"                                                                                                            ,
      AMR_CONS_Effectivenes_REDUCED_F_ANIMALS_2= "What_do_u_believe_ar_ntibiotic_resistance/reduced_effectiveness_of_antibiotics_for"                                                                                                            ,
      AMR_CONS_MED_COST_2= "What_do_u_believe_ar_ntibiotic_resistance/increased_medical_costs"                                                                                                                             ,
      AMR_CONS_Effectivenes_REDUCED_Farmproductivity_2 = "What_do_u_believe_ar_ntibiotic_resistance/reduced_farm_productivity"                                                                                                                           ,
      AMR_CONS_OTHERS_2 = "What_do_u_believe_ar_ntibiotic_resistance/other_specify" ,
    
  View_Preventing_AMR = "In_your_view_on_a_sc_ntibiotic_resistance"                                                                                                                                                     ,
View_Preventing_AMR_Guidelines = "In_your_view_on_a_sc_ntibiotic_resistance/strictly_follow_the_recommended_antibiot"                                                                                                            ,
View_Preventing_AMR_NO_USAGE = "In_your_view_on_a_sc_ntibiotic_resistance/limit_antibiotic_use_all_together___"                                                                                                                ,
View_Preventing_AMR_Biosecurity = "In_your_view_on_a_sc_ntibiotic_resistance/increase_health_security_measures___"                                                                                                                ,
View_Preventing_AMR_SwitchBreed = "In_your_view_on_a_sc_ntibiotic_resistance/switch_to_other_breeds___"                                                                                                                           ,
View_Preventing_AMR_Vaccination ="In_your_view_on_a_sc_ntibiotic_resistance/following_vaccination_regime___"                                                                                                                     ,
View_Preventing_AMR_Genral_Husbandry =  "In_your_view_on_a_sc_ntibiotic_resistance/following_good_chicken_keeping_practices"                                                                                                            ,
View_Preventing_AMR_Policy =  "In_your_view_on_a_sc_ntibiotic_resistance/advocate_for_policy_to_regulate_antibiot"                                                                                                            ,
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
       Heard_OH_2 = "Have_you_ever_heard_ntibiotic_resistance"                                                                                                                                                      ,
      Heard_OH_YES_2 = "Have_you_ever_heard_ntibiotic_resistance/option_1__yes"                                                                                                                                        ,
      Heard_OH_NO_2 = "Have_you_ever_heard_ntibiotic_resistance/option_2__no"                                                                                                                                         ,
      Heard_OH_UNSURE_2 =  "Have_you_ever_heard_ntibiotic_resistance/option_3__unsure"
)
                                               
      
     




  
  QUEST<- INIKA

  colnames(QUEST)

  mutate(Gender_F = case_when(
    Gender_F == "female" ~ "Female",
    Gender_F == "male" ~ "Male",
    TRUE ~ Gender_F
  )) %>%
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
    District %in% c("Moshi/rural", "Moshi rural", "Moshi Village", "Moshi village", "Moshi", "Municipal Mc", "Moshi DC", "Moshi municipal", "Moshi urban(Boma mbuzi)") ~ "1",
    District %in% c("Hai", "Hai district", "Siha") ~ "3",
    TRUE ~ District
  ))%>%
  mutate(REGIONNO = case_when(
    Respondent %in% c("Emanuel swai", "Azaria mndeme", "Miliki Mbwambo", "Magret Roman kirio", "Flate Mariwa") ~ "1",
    TRUE ~ REGIONNO
  )) %>%
  mutate(DISTRICTNO = case_when(
    Respondent %in% c("Emanuel swai", "Azaria mndeme", "Miliki Mbwambo", "Magret Roman kirio", "Flate Mariwa") ~ "1",
    Respondent == "Elizabeth Mtui" ~ "3",
    TRUE ~ DISTRICTNO
  )) %>%
  mutate(Respondent = case_when(
    Respondent == "bilabaN" ~ "Heriet simonfundi",
    Respondent == "Norbert N" ~ "Rose lema",
    TRUE ~ Respondent
  )) %>%
  mutate(Interviewer = case_when(
    Respondent %in% c("Heriet simonfundi", "Rose lema") ~ "bilabaN",
    TRUE ~ Respondent
  )) %>%
  mutate(Region = case_when(
    REGIONNO == "1" ~ "Kilimanjaro",
    REGIONNO == "2" ~ "Mwanza",
    TRUE ~ Region
  )) %>%
  mutate(Education_R = case_when(
    Education_R %in% c("Standard Seven", "Standard seven", "Standard sevev", "STD SEVEN", 	
                       "Standard sevev.","Standard 7", "StandardVII", "Standard VII", "STANDARD VII") ~ "Primary",
    Education_R %in% c("Form IV", "STANDARD IV", "Standard IV", "Standard four", "From four", "Form VI", "Form two", "Four IV", "Form four", "Formfour", "Form4", "Form six", "Form IV", "Fotm IV", "Form II") ~ "Secondary",
    Education_R %in% c("Degree", "Certificate","Diploma", "Advanced diploma", "Bachelor", "Cerrificate", "Certificate - Animal Health", "Colledge", "A degree") ~ "College",
    TRUE ~ Education_R
  )) %>%
  mutate(Ethnic_R = case_when(
    Ethnic_R %in% c("MCHAGA","Mchanga","Mchaga"
    ) ~ "Chaga",
    Ethnic_R == "Mpare" ~ "Pare",
    Ethnic_R %in% c ("Msukuma", "0") ~ "Sukuma",
    TRUE ~ "Others"
  )) %>%
  mutate(Ethnic_F = case_when(
    Ethnic_F %in% c("MCHAGA", "Mchaga") ~ "Chaga",
    Ethnic_F == "Mpare" ~ "Pare",
    Ethnic_F  %in% c("Msukuma", "0") ~ "Sukuma",
    TRUE ~ "Others"
  )) %>%
  mutate(Education_F = case_when(
    Education_F %in% c("Standard Seven", "Standard seven", "STD SEVEN", "Standard 7", "StandardVII", "Standard VII", "STANDARD VII") ~ "Primary",
    Education_F %in% c("Form IV", "Four IV", "Form four", "Formfour", "Form4", "Form six", "Form IV", "Fotm IV", "Form II") ~ "Secondary",
    Education_F %in% c( "Cerrificate", "Certificate - Animal Health") ~ "College",
    Education_F %in% c("Bachelor degree","Degree","Diploma", "Advanced diploma", "Bachelor","PhD") ~ "University",
    TRUE ~ Education_F
  )) %>%
  mutate(Occupation_R = case_when(
    Occupation_R %in% c("farmer", "Farmer", "FARMER", "Famer", "Farm Manager", "Farming", "Livestock", "Livestock keeper", "Livestock Farmer", "Livestock farmer","Farmer.", "Farmer and business", "Farmer and Business", "Broiler Farmer, agricultural, and poultry business man", "Farmer and Intrepreneurer") ~ "Farmer",
    Occupation_R %in% c("Farmer and loan officer", "Farmer and Nutritionist","Farmer and electrical technician", 	
                        "Farmer and interprenewer","Famer and mechanical person", "Farmer/cooker","Farmer_and_Other", 	
                        "Interprenewer in livestock keeper","Farming and other business", "Farmer_and_Other", "Farmer and Other", "Manager & Farming", 	
                        "Pastor and farmer") ~ "Farmer_and_Other",

    TRUE ~ "Other"
  )) %>%
  
  mutate(Occupation_F2 = case_when(
    Occupation_F2 %in% c("farmer", "Farmer", "FARMER", "Famer", "Farm Manager", "Farming", "Livestock", "Livestock keeper", "Livestock Farmer", "Livestock farmer","Farmer.", "Farmer and business", "Farmer and Business", "Broiler Farmer, agricultural, and poultry business man", "Farmer and Intrepreneurer") ~ "Farmer",
    Occupation_F2 %in% c("Farmer and loan officer", "Farmer and Nutritionist","Farmer and electrical technician", 	
                        "Farmer and interprenewer","Famer and mechanical person", "Farmer/cooker","Farmer_and_Other", 	
                        "Interprenewer in livestock keeper","Farming and other business", "Farmer_and_Other", "Farmer and Other", "Manager & Farming", 	
                        "Pastor and farmer") ~ "Farmer_and_Other",
    
    TRUE ~ "Other"
  )) %>%
  mutate(Usage_OF_AB = case_when(
    Usage_OF_AB == "no" ~ "Male",
    TRUE ~ Usage_OF_AB
  )) %>%
  mutate(Knowlede_AB = case_when(
    Knowlede_AB == "no" ~ "No",
    Knowlede_AB == "yes" ~ "Yes",
    TRUE ~ Knowlede_AB
  )) %>%
  
  mutate(AB_training = case_when(
    AB_training == "no" ~ "No",
    AB_training== "yes" ~ "Yes",
    TRUE ~ AB_training
  )) %>%
  mutate(Knowledge_AMR = case_when(
    Knowledge_AMR == "somwhat familar" ~ "Somewhat familiar",
    Knowledge_AMR == "somewhat_familiar" ~ "Somewhat familiar",
    Knowledge_AMR == "very_familiar" ~ "Very familiar",
    TRUE ~ Knowledge_AMR
  )) %>%
  mutate(AMR_Knowledge_Perception = case_when(
    AMR_Knowledge_Perception == "average_knowledge" ~ "Average knowledge",
    AMR_Knowledge_Perception == "high_knowledge" ~ "High knowledge",
    AMR_Knowledge_Perception == "low_knowledge" ~ "Low knowledge",
    AMR_Knowledge_Perception == "very_high_knowledge" ~ "Very high knowledge",
    TRUE ~ AMR_Knowledge_Perception
  )) %>%
  mutate(Knowledge_Collaborative_OH = case_when(
    Knowledge_Collaborative_OH == "no" ~ "No",
    Knowledge_Collaborative_OH == "yes" ~ "Yes",
    TRUE ~ Knowledge_Collaborative_OH
  )) %>%
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
  )) %>%
  
   mutate(Breed = case_when
         (Breed %in% c("Alvin", "Arvin broiler", "Arvin broilersBroiler", "Broiler - arvin", "Broiler -arvin", "Broiler and croiler", "Broiler and Saso", "Broiler arvin", "Broiler Ross 308", "Broiler silverland", "Broiler, Kuroirer and Saso", "Broiler, layers", "Broiler, saso", "Broiler,alvin", "Broiler,layers", "Broiler,Saso", "Broilers alvin", "Cob 500", "Cobb 500", "Cobb and Rose", "Cobb500", "cobb500 & sasso", "Conb500Roos 308", "Ros308", "Ross 305", "Ross 308", "Ross300", "Ross308", "Intercheeck broiler", "Interchick", "Interchicks", "Interchicks and centrachicks", "Interchicks and centrochicks","Saso and broiler",  "Silverland broiler",
                       "Saso and Broiler", "Saso Ross", "Saso Ross 308", "Silverland broiler", "Arvin broiler", "Arvin broilersBroiler", "Broiler - arvin", "Broiler -arvin", "Broiler and croiler", "Broiler and Saso", "Broiler arvin", "Broiler Ross 308", "Broiler silverland", "Broiler, Kuroirer and Saso", "Broiler, layers", "Broiler, saso", "Broiler,alvin", "Broiler,layers", "Broiler,Saso", "Broilers alvin", "Cob 500", "Cobb 500", "Cobb and Rose", "Cobb500", "cobb500 & sasso", "Conb500Roos 308", "Ros308", "Ross 305", "Ross 308", "Ross300", "Ross308", "Intercheeck broiler","Saso and broiler", "Silverland broiler", "Saso and Broiler", "Saso Ross", 
                       "Saso Ross 308","Saso, kroiler and Broiler") ~ "Broiler", 
          Breed %in% c("Saso silverland", "Saso silverland 308", "Chroirer", "Croiler", "Croiler - Interchicks", "Croiler and Saso",
          "Asho", "Ashok", "Ashork", "Bovans", "Chotara", "Erveness and sendlecheck", "Evines", "Falcon", "hashokh", "Intercheeck ,and kencheek",
          "Joshi", "Kibo and silverland", "Kloiler", "Kroiler", "Kroiler/ layers", "Kuroiler", "Layers and saso", "Saso", "Saso- silver land",
          "Saso- tanbro", "Saso - silverland", "Saso & kloiler", "Saso 38", "Saso& kroiler", "Saso, croiler",
          "Saso,chotara", "Saso,Chotara", "Silverland", "Silverland saso", "Tanbro", "Tanbro -intercheeck", "Tanbro and Saso", "Tanbro,sasoall") ~ "Crossbreds" ))



mutate(Overusage_AB_Poultry = case_when(Overusage_AB_Poultry=="very_overused" ~ "Very Overused",
       Overusage_AB_Poultry =="somewhat_overused" ~ "Somewhat overused",
        Overusage_AB_Poultry =="little_overused"~ "Little overused",
        Overusage_AB_Poultry =="overused" ~ "Overused",
         TRUE ~ Overusage_AB_Poultry 
         ))%>%
  
  

       
	
         mutate(Heard_OH==case_when(Heard_OH=="no" ~ "No",
                                    Heard_OH=="yes" ~ "Yes",
                                    TRUE ~ Heard_OH
  )) %>%
    mutate(Broiler_Disease_Outbreak_Human_Health==case_when(Broiler_Disease_Outbreak_Human_Health =="high" ~ "High",
                                          Broiler_Disease_Outbreak_Human_Health =="low" ~ "Low",
                                          Broiler_Disease_Outbreak_Human_Health =="moderate" ~ "Moderate",
                            TRUE ~ "Broiler_Disease_Outbreak_Human_Health" ))%>%
  
  
  mutate(Source_Training_AB = case_when(Source_Training_AB
     == "Friends and family" ~ "Friends and Family",
     
     Source_Training_AB %in% c('church', "From Lutheran church they provided us with the knowledge and books as guied(mchomvu))",
          "NCA-Erasto","NCA-Erasto","NCA","Erasto", "NCA-ERASTO", "NCA-ERASTO","NRS","NCA-ERASTO",
          "NRS","From the church.) ~ 'Church',
          'farm',("Mabuki farm," Agricultural association services,"Agricultural association services Veterinary professionals",
                   "Localcommunitymeetings","local_community_meetings","local_community_meetings ) ~ 'Local community',
                     (Sua",
          "Training from SUA via ministry of livestock",
          "College",
          "Livestock College Madaba",
          "Shinyanga",
          "During the college as am animal health practitioner",
          "During the college as am animal health practitioner")
     ~ 'Government',
     c("I don't remember",
          "I don't remember due to variety of companies that come to train us on how to grow our chicken",
          "I don't remember they were from kenya","I dont remember" ) ~ 'Unknown',
  
    
    TRUE ~ 'Other' 
  ))
    

    
    
 



("Alvin","Alvin Company,"Arvin chicks companies,
"Arvin chikens companies","Arvin saso","Arvin supplier of chicks,
""Ashorkh", «KIBO","KUGIES", "Tao nutrition","Mabuki farm",
«NGOKMG","NRS,"Saso Silverland","Shinyanga",
"Silverland Iringa","Silverland saso","Chicks companies", 
"To agrovet shops","Harsho and silverland,","Harshoseminars",
"Harshok","HAVICK" )  ~'Companies'
  
    
    
                                  
                                 	
                                               
                               
                                  



# BELOW HERE YOU can not run anything as this is not ready YET and yOU were supposed to WRITE these! All the different contents from the dataset are there ( Copy pasted: You just need to group them as I started to do for the Broilers)

#### Here below you need to write this part ready!


###########################################
frequency<- table(QUEST$Breedkat)
print(frequency)


frequency<-table(INIKA_SURVEY_ORIGINAL$"Breed of chicken")
          print(frequency)
  
  
  
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
frequencies <- get_frequencies(QUEST)

# Combine the frequencies into a single dataframe
combined_frequencies <- bind_rows(
  lapply(names(frequencies), function(colname) {
    freq_table <- frequencies[[colname]]
    freq_table <- mutate(freq_table, Column = colname)
  }),
  .id = "id"
)

frequencies <- get_frequencies(INIKA_SURVEY_ORIGINAL)

# Combine the frequencies into a single dataframe
combined_frequencies <- bind_rows(
  lapply(names(frequencies), function(colname) {
    freq_table <- frequencies[[colname]]
    freq_table <- mutate(freq_table, Column = colname)
  }),
  .id = "id"
)




write.csv(combined_frequencies, file = paste(outputFilbane,"Resultat\\","COMBINED_2.csv", sep = ""))


# Reorder the columns
combined_frequencies <- combined_frequencies %>%
  select(Column, Value, Frequency)

# Write the combined frequencies to an Excel file
FREKVENS<_write_xlsx(combined_frequencies, "frequencies.xlsx")

# Print the combined frequencies
print(combined_frequencies)
       
         