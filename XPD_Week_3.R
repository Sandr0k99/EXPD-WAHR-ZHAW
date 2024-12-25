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
table.survey$`Anreisezeit an die ZHAW` <- as.numeric(table.survey$`Anreisezeit an die ZHAW`)

table.survey$Koerpergroesse[23]
#178
table.survey$`Statistische Vorkenntnisse`[13]
#gering
table.survey$Ausbildung[15]
#Kaufmann
table.survey[5,]$Postleitzahl
#8472
table.survey[18,c("Augenfarbe","Ausbildung")]
#blau,Buchhändlerin EFZ
table.survey[c(6,19),c("Sport","Anreisezeit an die ZHAW")]
#6     90                      
#6     40
table.survey[c(21:23),c("Haarfarbe","Augenfarbe")]
#braun     Braun
#schwarz   Braun
#blond     blau
table.survey[c(1:10),"Geschlecht"]
#9 x männlich, 1 x weiblich
table.survey[c((num_rows-10):num_rows), "Schlafzimmer"]
#1 x Parterre
sum(as.integer(table.survey$Handspanne),na.rm = TRUE) / (num_rows -1)
#19
sum(as.integer(table.survey$Koerpergroesse)) / num_rows
#173.7
median(table.survey$Schuhgrösse[-16], na.rm = TRUE)
median(table.survey$Schuhgrösse, na.rm = TRUE)
#42,42
mean(table.survey$`Anreisezeit an die ZHAW`[c(7,18,27)])
round(mean(table.survey$`Anreisezeit an die ZHAW`))
#28,39
table.survey$Sport[c(7,18,27)]
#4,3,4
table.survey[c(13,15,17), c("Postleitzahl","Geschlecht","Koerpergroesse")]
#8050         weiblich   169           
#7270         männlich   179           
#8484         männlich   178 
table.survey$Ausbildung[c(3,21)]


#####
#3

# a)
sleep <- read.table('./Data/sleep.txt', sep = '\t', header =T )

# b)
nrow(sleep)
sleep$Differenz <- sleep$Schlafmittel - sleep$Placebo
mean(sleep$Differenz)
median(sleep$Differenz)

# c)
par(mfrow = c(1,2))
stripchart(sleep$Differenz, xlab='Stunden', method = "stack")
hist(sleep$Differenz, xlab='Stunden')

# d)
par(mfrow = c(1,1))
plot(ecdf(sleep$Differenz), verticals = T)
abline(v = 1, col = "red")
abline(v = 3, col = "red")
ecdf(sleep$Differenz)(1)
ecdf(sleep$Differenz)(3)
