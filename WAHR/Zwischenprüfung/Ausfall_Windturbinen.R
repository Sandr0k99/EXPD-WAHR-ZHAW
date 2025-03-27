# Es laufen 20 Windturbinen 26 Tage lang. 
# Jede Windturbine hat an jedem Tag eine Wahrscheinlichkeit von 7% auszufallen. 
# Die Tage sind unabhängig voneinander. Das heisst, wenn eine Turbine an einem Tag ausfällt, 
# kann sie am nächsten Tag wieder mit einer Wahrscheinlichkeit von 7% ausfallen.

# Wie oft fallen in den 26 Tagen 3 oder mehr Turbinen gleichzeitig aus?

# Hinweis: Lösen Sie die Aufgabe mit einer Simulation (n = 100000).
# Geben Sie das Resultat auf 2 Nachkommastellen an.

set.seed(42)       # Für Reproduzierbarkeit
n <- 100000        # Anzahl der Simulationen
tage <- 26
turbinen <- 20
p_ausfall <- 0.07

# Eine Simulation: Zählt, wie oft an einem Tag 3 oder mehr Turbinen ausfallen
eine_simulation <- function() {
  count <- 0
  for (i in 1:tage) {
    ausfälle <- rbinom(1, turbinen, p_ausfall)
    if (ausfälle >= 3) {
      count <- count + 1
    }
  }
  return(count)
}

# Alle Simulationen durchführen
resultate <- replicate(n, eine_simulation())

# Mittelwert berechnen
mittelwert <- mean(resultate)
cat("Durchschnittliche Anzahl Tage mit ≥3 Ausfällen:", round(mittelwert, 2), "\n")
