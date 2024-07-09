



install.packages("writexl")
library(writexl)

# Dataene
data <- data.frame(
  Jina_Idadi_Kata = c("Ivan Machumu", "Felister Mebo", "Delish Food", "Apronia Lyimo", "Editha Matembe", "Richard Kaswalala", "Mussa Kiwanuka", "Agnes Ezekiel", "Machibya Lutema", "Philip Msangi", "Felister Mwailondele", "Mathias Malendeja", "Raphael Mabele", "Ivan Machumu", "Cleophace Nzumbi"),
  Simu = c("400", "200", "8,000", "50", "100", "100", "150", "50", "300", "150", "100", "50", "100", "200", "100"),
  Aina_ya_kuku = c("Broiler", "Broiler", "Broiler", "Sasso", "Sasso", "Sasso", "Sasso", "Sasso", "Sasso", "Sasso", "Sasso", "Ssso", "Sasso", "Sasso", "Sasso"),
  stringsAsFactors = FALSE
)

# Eksporter til Excel
write_xlsx(data,paste(outputFilbane, "output.xlsx")

