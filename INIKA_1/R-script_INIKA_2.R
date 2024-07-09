outputFilbane <- "K:\\FAG\\Provedata\\Rapportering\\NormVetUtvikling\\NORMVETProsjekt\\NORMVET2022\\outputDataset\\"

install.packages("writexl")
library(writexl)

# Dataene
data <- data.frame(
  Jina = c("Ivan Machumu", "Felister Mebo", "Delish Food", "Apronia Lyimo", "Editha Matembe", "Richard Kaswalala", "Mussa Kiwanuka", "Agnes Ezekiel", "Machibya Lutema", "Philip Msangi", "Felister Mwailondele", "Mathias Malendeja", "Raphael Mabele", "Ivan Machumu", "Cleophace Nzumbi"),
  Idadi = c(400, 200, 8000, 50, 100, 100, 150, 50, 300, 150, 100, 50, 100, 200, 100),
  Kata = c("Kisesa", "Kisesa", "Kisesa", "Isandula", "Nyigogo", "Nyigogo", "Isandula", "Itumbili", "Kandawe", "Isandula", "Itumbili", "Kisesa", "Kisesa", "Kisesa", "Magu Mjini"),
  Simu = c("0768924661", "0713302132", "0769901465", "0784460230", "0656040294", "0744490500", "0788084714", "0784200808", "0688592510", "0759026549", "0784463646", "0765530741", "0788251514", "0768924661", "0786781076"),
  Aina_ya_kuku = c("Broiler", "Broiler", "Broiler", "Sasso", "Sasso", "Sasso", "Sasso", "Sasso", "Sasso", "Sasso", "Sasso", "Ssso", "Sasso", "Sasso", "Sasso"),
  stringsAsFactors = FALSE
)

# Eksporter til Excel
write_xlsx(data,"K:\\FAG\\Provedata\\Rapportering\\NormVetUtvikling\\NORMVETProsjekt\\NORMVET2022\\outputDataset\\output_MAGU.xlsx")