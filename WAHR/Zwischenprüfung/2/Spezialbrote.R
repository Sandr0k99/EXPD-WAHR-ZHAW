# -----------------------------------------------
# Aufgabe: Spezialbrote beim Beck
#
# Eine Bäckerei hat im Mittel 133 Kunden pro Tag.
# 10.4 % der Kunden sind ganz verrückt nach dem Spezialbrot und kaufen dieses.
# Pro Tag werden 12 Spezialbrote gebacken.
#
# Wie wahrscheinlich ist es, dass die tägliche Nachfrage nach Spezialbroten 
# vollständig befriedigt werden kann, d. h. alle Kunden, die ein Spezialbrot möchten,
# auch eines bekommen?
#
# Annahmen:
# - Die Anzahl Kunden pro Tag ist poissonverteilt.
# - Die Anzahl Kunden, die Spezialbrote wollen, ist binomialverteilt.
# - Simulation mit 100'000 Wiederholungen.
# - Gesucht: Wahrscheinlichkeit, dass ≤ 12 Spezialbrote verlangt werden.
# -----------------------------------------------

set.seed(42)  # für Reproduzierbarkeit

n <- 100000
erfolgreiche_tage <- 0

for (i in 1:n) {
  kunden <- rpois(1, 133) # Anzahl Kunden pro Tag ~ Poisson(133)
  spezialbrot_kunden <- rbinom(1, kunden, 0.104) # davon 10.4 % wollen Spezialbrot
  if (spezialbrot_kunden <= 12) {
    erfolgreiche_tage <- erfolgreiche_tage + 1
  }
}

wahrscheinlichkeit <- erfolgreiche_tage / n
cat("Wahrscheinlichkeit, dass die Nachfrage ≤ 12 Brote ist:", round(wahrscheinlichkeit, 2), "\n")
