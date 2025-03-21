#####
# 3

#c)
farben <- c("Kreuz", "Herz", "Pik", "Karo")
nummern <- c(2:10, "Bube", " Dame", "Koenig", "Ass")
deck <- expand.grid(Farbe=farben, Nummer=nummern)
deck <- paste(deck$Farbe, deck$Nummer)
asse <- paste(farben, "Ass")

n <- 10000
haende <- data.frame(matrix(NA, ncol=2, nrow=n))
names(haende) <- c('Karte1', 'Karte2')
for (i in 1:n) {
  haende[i,] <- sample(deck, 2, replace = F)
}

# Wahrscheindlichkeit, dass nach einem Ass noch ein Ass kommt
sum(haende$Karte1 %in% asse & haende$Karte2 %in% asse) /
  sum(haende$Karte1 %in% asse)

