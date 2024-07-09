

inputFilbane <- "K:\\FAG\\Tverrfaglig\\AMR\\FoU-aktiviteter & prosjekter\\31218_INIKA_OH_TZ\\WP3\\inputDataset\\" 
outputFilbane <- "K:\\FAG\\Tverrfaglig\\AMR\\FoU-aktiviteter & prosjekter\\31218_INIKA_OH_TZ\\WP3\\outputDataset\\" 

 library(readxl)
Kilimanjaro_SELECTION <- read_excel("//vetinst.no/dfs-felles/StasjonK/FAG/Tverrfaglig/AMR/FoU-aktiviteter & prosjekter/31218_INIKA_OH_TZ/WP2/Kilimanjaro_SELECTION.xlsx")
View(Kilimanjaro_SELECTION)

Kilimanjaro_SELECTION$INIKA_ID<- as.character(Kilimanjaro_SELECTION$INIKA_ID)


 
Kilimanjaro_TESTFARMS_KAP <- read_excel("//vetinst.no/dfs-felles/StasjonK/FAG/Tverrfaglig/AMR/FoU-aktiviteter & prosjekter/31218_INIKA_OH_TZ/WP2/Kilimanjaro_TESTFARMS_KAP.xlsx")
 View(Kilimanjaro_TESTFARMS_KAP)
library(dplyr)

 

PREV_IDS<- full_join(Kilimanjaro_TESTFARMS_KAP,Kilimanjaro_SELECTION)%>%
  mutate(ALREADY ="A")


PREV_UNSELECT<- full_join(PREV_IDS, KILIMANJARO_FARMS, by = "ID")



Testnew <- PREV_UNSELECT%>%
 mutate(REGIONNUMBER = 1) %>%
  mutate(DistrictNumber = case_when(
      Place.y %in% c("Moshi Urban", "Moshi/Urban", "Moshi Municipal") ~ 1,
      Place.y %in% c("Moshi Rural", "Moshi/Rural")  ~ 2,
      Place.y %in% c("Moshi Hai" , "Hai") ~ 3,
      TRUE ~ NA_integer_  
    )
  )%>%
  mutate(NEW_INIKA_ID=paste0(REGIONNUMBER,DistrictNumber, 181 + row_number ()))



write.csv2(Testnew, paste(outputFilbane,"Kilimanjaro_additionalIDS.csv",sep=""))



