# Fuchs-Jagd ist ein Würfelspiel. Gespielt wird mit einem herkömmlichen fairen 6er Würfel.
# Ein Spieler ist der Fuchs, die anderen sind die Jäger.
# Der Fuchs legt zwei Würfe vor; 1. Jäger würfelt; Fuchs würfelt; 2. Jäger würfelt, dann wieder der Fuchs usw.
# Würfe von Fuchs bzw. Jägern werden zusammengezählt bis der Fuchs 30 Punkte erreicht hat (Fuchs gewinnt)
# oder die Jäger ihn einholen (Jäger gewinnen).

# Unabhängig von den Jägern, wie viele Würfel braucht der Fuchs im Mittel, um die 30 Punkte zu erreichen (oder überschreiten)?
# Geben Sie das Resultat auf 2 Nachkommastellen an.
# Hinweis: Lösen Sie die Aufgabe mit einer Simulation (n = 100000). 
# Sie können zuerst 30 Mal würfeln und dann schauen, bei welchem Wurf die 30er Marke erreicht wurde.

set.seed(42)  # für Reproduzierbarkeit
n <- 100000   # Anzahl Simulationen
ziel <- 30    # Zielpunkte

# Funktion für eine einzelne Simulation
fuchs_spiel <- function() {
  punkte <- 0
  würfe <- 0
  while (punkte < ziel) {
    wurf <- sample(1:6, 1, replace = TRUE)  # fairer 6er-Würfel
    punkte <- punkte + wurf
    würfe <- würfe + 1
  }
  return(würfe)
}

# Simulation durchführen
resultate <- replicate(n, fuchs_spiel())

# Mittelwert berechnen
mittelwert <- mean(resultate)
cat("Durchschnittliche Würfe bis 30 Punkte:", round(mittelwert, 2), "\n")
