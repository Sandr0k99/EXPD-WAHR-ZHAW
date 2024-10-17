# Faktoren ----

# Erstellung einer Faktor-Variable in R
ProgKennt <- c("gross", "klein", "mittel", "nicht vorhanden", 
               "nicht vorhanden", "mittel", "mittel", "klein")
ProgKenntFac<- factor(ProgKennt)

ProgKenntFac # Alphabetische Anordnung der Level

# Wir analysieren die Struktur der Variable
str(ProgKenntFac)
mode(ProgKenntFac)
summary(ProgKenntFac)

# Welchen "Datentyp" hat die Faktor-Variable?
typeof(ProgKenntFac)  ## "integer" !!
ProgKenntInt <- as.integer(ProgKenntFac)



data.frame(
  Faktor = ProgKenntFac, 
  Integer = ProgKenntInt
)


# Diesmal geben wir eine eigene Reihenfolge der Faktor-Levels an
ProgKenntFac <- factor(
  ProgKennt,
  levels = c("nicht vorhanden", "klein", "mittel", "gross")
)

# Das hat einen Einfluss auf die interne Integer-Repräsentation der Levels
data.frame(
  Faktor = ProgKenntFac, 
  Integer = as.integer(ProgKenntFac)
  )



ProgKennt <- c("gross", "klein", "mittel", "nicht vorhanden", 
               "nicht vorhanden", "mittel", "mittel", "klein")
ProgKenntFac<- factor(ProgKennt)

barplot(table(ProgKennt),
        cex.names= 0.9, 
        cex.axis= 0.8)



nlevels(ProgKenntFac)

newFac <- ProgKenntFac[1:3]
newFac <- droplevels(newFac)
# oder
newFac <- ProgKenntFac[1:3, drop = T]
newFac


# Umbenennung einer Ausprägung: z.B. `klein` in `tief`:
  
# So funktioniert es nicht
ProgKenntFac[ProgKenntFac == "klein"] <- "tief"

# Ein Faktor-Variable kann nicht direkt auf eine unbekannte Level-Stufe umbenannt werden. 
# Korrekt muss die Level-Stufe geändert werden:
ProgKennt <- c("gross", "klein", "mittel", "nicht vorhanden", "nicht vorhanden", "mittel", "mittel", "klein")
ProgKenntFac <- factor(ProgKennt)

levels(ProgKenntFac)[levels(ProgKenntFac) == "klein"] <- "tief"

# Faktorlevel umbenennen/zusammenfassen Ausprägungen `klein` und `mittel` in
# gering` und gross` in `umfangreich` umbennen über die Funktion
# `as.character()`
  
ProgKenntFac <- factor(c("gross", "klein", "mittel", 
                         "nicht vorhanden", "nicht vorhanden", "mittel", "mittel", "klein"))

ProgKenntNew <- as.character(ProgKenntFac)
vSel <- ProgKenntNew == "klein" | ProgKenntNew == "mittel"
ProgKenntNew[vSel] <- "gering"
vSel <- ProgKenntNew == "gross"
ProgKenntNew[vSel] <- "umfangreich"
ProgKenntNewFac <- factor(ProgKenntNew)
ProgKenntNewFac
nlevels(ProgKenntNewFac)

ProgKenntFac
ProgKenntFac <- factor(x= ProgKenntFac,
                       levels= c("nicht vorhanden", "klein", "mittel", "gross"),
                       labels= c("nicht vorhanden","gering", "gering","umfangreich"))
ProgKenntFac

## Geordnete Faktoren

ProgKennt <- c("gross", "klein", "mittel", 
               "nicht vorhanden", "nicht vorhanden", "mittel", "mittel", "klein")
min(ProgKenntFac)
ProgKenntFac <- factor(
  ProgKennt,
  levels= c("nicht vorhanden","klein","mittel", "gross"),
  ordered= TRUE
)
min(ProgKenntFac) # oder max(), quantile(..., type = 1 | 3)

# Durch `ordered = TRUE` wird die Reihenfolge in statistischen Modellen berücksichtigt!  
## Faktoren aus Zahlen-Vektoren
  
alter <- c(22,26,22,20,23,26,24,24,23,20,24)
alterFac <- factor(alter)
alterFac
as.integer(alterFac)  # funktioniert nicht direkt

# funktioniert über den Umweg as.character()
as.integer(as.character(alterFac)) 

## Quantitative Variable in Faktor umwandeln

dat <- read.csv("daten/Fragebogen_ExpD_hs24.csv", sep=",")
az <- dat$Anreisezeit.an.die.ZHAW
summary(az)


azFac <- cut(x= az, breaks= seq(0, 120, by= 20 )) 
str(azFac)


# Pakete ----

search()

v <- c(1:10,1000)
huber(v)


library(MASS)
search()
