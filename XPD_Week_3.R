#####
#1

table.birds <- read.table(file = "./Data/kiebitz.txt", sep = ",", header = TRUE)

str(table.birds)
head(table.birds)
#Datenpunkte: 9
#Merkmale: 3

filtered_birds = data.frame(table.birds[table.birds$Feld.Nr == 1411,])

sum(filtered_birds$Anzahl)
#61

#####
#2
#install.packages("readxl")
library(readxl)
table.survey <- read_excel("./Data/Fragebogen_ExpD_hs24.xlsx")

num_rows <- nrow(table.survey)
table.survey$Koerpergroesse[23]
table.survey$`Statistische Vorkenntnisse`[13]
table.survey$Ausbildung[15]
table.survey[5,]$Postleitzahl
table.survey[18,c("Augenfarbe","Ausbildung")]
table.survey[c(6,19),c("Sport","Anreisezeit an die ZHAW")]
table.survey[c(21:23),c("Haarfarbe","Augenfarbe")]
table.survey[c(1:10),"Geschlecht"]
table.survey[c((num_rows-10):num_rows), "Schlafzimmer"]
sum(as.integer(table.survey$Handspanne),na.rm = TRUE) / (num_rows -1)
sum(as.integer(table.survey$Koerpergroesse)) / num_rows



