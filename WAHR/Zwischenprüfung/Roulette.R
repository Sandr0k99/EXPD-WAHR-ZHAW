# Du stehst im Casino und spielst Roulette (18 schwarze Felder, 18 rote Felder und 1 grünes Feld)
# mit der bekannten Strategie: 4.- auf Rot zu setzen. Du gewinnst,
# wenn die Kugel auf Rot landet und verlierst, wenn sie nicht auf Rot landet.
# Wenn du gewinnst, behältst du den Gewinn (das Doppelte des Einsatzes) und gehst
# nach Hause. Wenn du verlierst verdoppelst du den Einsatz und setzt wieder auf Rot.
# Dies machst du, bis du das erste Mal gewonnen oder du 5-mal gespielt hast (entspricht einem Budget von 124.-).

#Wie gross ist der mittlere Gewinn, welcher aus dieser Strategie resultiert?
#Hinweis: Lösen Sie die Aufgabe mit einer Simulation (n = 100000).

set.seed(42)        # Für Reproduzierbarkeit
n <- 100000         # Anzahl Simulationen
max_runden <- 5     # Max. 5 Runden erlaubt
einsatz_start <- 4  # Ersteinsatz

# Gewinnwahrscheinlichkeit für Rot (18 von 37 Feldern)
p_rot <- 18 / 37

# Funktion für eine einzelne Simulation der Strategie
spiele_roulette <- function() {
  einsatz <- einsatz_start
  gesamteinsatz <- 0
  
  for (runde in 1:max_runden) {
    gesamteinsatz <- gesamteinsatz + einsatz
    if (runif(1) < p_rot) {
      # Gewinnfall → Auszahlung ist 2 * aktueller Einsatz, minus gesamte Einsätze = Gewinn
      return((2 * einsatz) - gesamteinsatz)
    } else {
      einsatz <- einsatz * 2  # Verdoppeln bei Verlust
    }
  }
  return(-gesamteinsatz)  # Nach 5 Runden kein Gewinn → gesamter Verlust
}

# Simulation
gewinne <- replicate(n, spiele_roulette())

# Erwartungswert berechnen
mittelwert <- mean(gewinne)
cat("Mittlerer Gewinn aus dieser Strategie:", round(mittelwert, 2), "\n")




#NICHT KORREKT:
set.seed(42)  # für Reproduzierbarkeit
N<-100000 # Anzahl Simulationen
felder <-c(rep("Rot",18),rep("Schwarz",18),rep("Grün",1))
gewinn <- 0

for(i in 1:N){
  playing <- T
  round <- 1
  einsatz <- 4
  verlust <- 0
  while(playing){
    # Roulette wurf
    zug <- sample(felder, size=1, replace=T) # MIT ZURÜCKLEGEN
    is_red <- (zug[1] == "Rot")
    if(is_red){
      gewinn <- gewinn + ((einsatz*2) - verlust)
      playing <- F
    }else{
      verlust <- verlust + einsatz
    }
    if(round == 5){
      gewinn <- gewinn - verlust
      playing <- F
    }
    round <- round + 1
  }
}

round(gewinn / N, 2)