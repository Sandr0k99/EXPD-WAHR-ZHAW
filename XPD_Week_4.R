#####
# 1

# a)
birds <- read.table(file = './Data/kiebitz.txt', sep = ',', header = T)

# b)
df_birds <- data.frame(birds[birds$Feld.Nr == 1411,])

# c)
df_birds[df_birds$Taetigkeit == 'fr', "Anzahl"]

# d)
sort(unique(birds$Feld.Nr[birds$Taetigkeit == 'ru']))

#####
# 2

fragen <- read.csv(file = "./Data/Fragebogen_ExpD_hs24.csv", header = T)

# a)
fragen <- fragen[,10:ncol(fragen)]

# b)
boxplot(fragen$Koerpergroesse)

# c)
abs(ecdf(fragen$Koerpergroesse)(180) - 1)

# d)
mad(fragen$Handspanne, na.rm = T)
sd(fragen$Handspanne, na.rm = T)

# e)
median(fragen$Sport[fragen$Haarfarbe %in% c("blond", "Blond")])
median(fragen$Sport[fragen$Haarfarbe %in% c("schwarz", "Schwarz")])
median(fragen$Sport[fragen$Haarfarbe %in% c("braun", "Braun")])

# f)
bedingung <- fragen$Alter < 24 & fragen$Schlafzimmer == "1. Stock" & +
  (fragen$Haarfarbe %in% c("braun", "Braun") | fragen$Anreisezeit.an.die.ZHAW >= 50 | fragen$Koerpergroesse > 180)
fragen[bedingung,]

# g)
max(fragen$Alter) - min(fragen$Alter)
IQR(fragen$Alter)
mad(fragen$Alter)
sd(fragen$Alter)

# h)
fragen$foot2height <- fragen$Schuhgrösse / fragen$Koerpergroesse
fragen$foot2height_cat[fragen$foot2height <= 0.236] <- 'klein'
fragen$foot2height_cat[fragen$foot2height > 0.236 & fragen$foot2height <= 0.243] <- 'mittel'
fragen$foot2height_cat[fragen$foot2height > 0.243] <- 'gross'
table(fragen$foot2height_cat)

# i)
fragen$ID <- 1:nrow(fragen)

# j)
install.packages("openxlsx", dependencies = T)
library(openxlsx)
write.xlsx(fragen, file = "./Data/Fragebogen_ExpD_hs24_processed.xlsx")





























