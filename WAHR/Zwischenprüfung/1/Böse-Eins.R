# “Die böse Eins” ist ein Würfelspiel. Gespielt wird es mit einem herkömmlichen, fairen, sechsseitigen Würfel.
# Das Ziel ist es, so viele Punkte als möglich zu erwürfeln. Jeder Teilnehmer darf dazu so oft er möchte würfeln.
# Die gewürfelten Augen werden zusammengerechnet.
# Würfelt man jedoch eine Eins, verfallen die Punkte der gesamten Runde (man bekommt 0 Punkte),
# und der nächste Spieler ist an der Reihe.

# Wie viele Punkte erzielt man im Mittel, wenn man weiterwürfelt, solange die aktuelle Summe <= 21 ist?
# Geben Sie das Resultat auf 2 Nachkommastellen an.

# Hinweis: Lösen Sie diese Aufgabe mit einer Simulation (n = 100000).
# Vergessen Sie nicht die Fälle zu berücksichtigen, bei denen Sie eine Eins würfeln und entsprechend 0 Punkte erzielen.

set.seed(42)      # Für Reproduzierbarkeit
n <- 100000       # Anzahl der Simulationen
grenze <- 21      # Stopp falls Grenze überschritten wird

# Funktion für eine einzelne Runde
boese_eins <- function() {
  punkte <- 0
  repeat {
    wurf <- sample(1:6, 1, replace = TRUE) #MIT ZURÜCKLEGEN
    if (wurf == 1) {
      return(0)  # Alle Punkte verfallen
    }
    punkte <- punkte + wurf
    if (punkte >= grenze) {
      return(punkte)  # Risiko zu hoch, deshalb Stopp
    }
  }
}

# Simulation
resultate <- replicate(n, boese_eins())

# Mittelwert berechnen
mittelwert <- mean(resultate)
cat("Durchschnittliche Punkte bei dieser Strategie:", round(mittelwert, 2), "\n")
