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
table.survey$Schuhgrösse <- as.numeric(table.survey$Schuhgrösse)

table.survey$Koerpergroesse[23]
"178"
table.survey$`Statistische Vorkenntnisse`[13]
"gering"
table.survey$Ausbildung[15]
"Kaufmann"
table.survey[5,]$Postleitzahl
"8472"
table.survey[18,c("Augenfarbe","Ausbildung")]
"blau,Buchhändlerin EFZ"
table.survey[c(6,19),c("Sport","Anreisezeit an die ZHAW")]
"6     90"                      
"6     40"
table.survey[c(21:23),c("Haarfarbe","Augenfarbe")]
"braun     Braun"
"schwarz   Braun"     
"blond     blau"
table.survey[c(1:10),"Geschlecht"]
"9 x männlich, 1 x weiblich"
table.survey[c((num_rows-10):num_rows), "Schlafzimmer"]
"1 x Parterre"
sum(as.integer(table.survey$Handspanne),na.rm = TRUE) / (num_rows -1)
"19"
sum(as.integer(table.survey$Koerpergroesse)) / num_rows
"173.7"
median(table.survey[1:num_rows != 16, "Schuhgrösse"], na.rm = T)


