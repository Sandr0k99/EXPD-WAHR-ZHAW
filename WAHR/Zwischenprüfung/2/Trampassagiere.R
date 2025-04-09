# ------------------------------------------------------------
# Aufgabe: Trampassagiere ohne gültigen Fahrausweis
#
# Im 7er Tram ab HB Zürich hat es an Werktagen um 7:41 Uhr im Mittel 46.8 Passagiere.
# Aus Erfahrung ist bekannt, dass 93 % der Passagiere einen gültigen Fahrausweis besitzen.
# 
# Annahmen:
# - Die Gesamtanzahl Passagiere ist poissonverteilt mit λ = 46.8.
# - Die Anzahl Schwarzfahrer ist binomialverteilt (mit p = 0.07).
#
# Gesucht:
# Berechnen Sie mittels Simulation die Wahrscheinlichkeit, dass sich 
# mehr als 3 Personen **ohne gültigen Fahrausweis** im Tram befinden.
#
# Hinweis: Verwenden Sie 100'000 Wiederholungen.
# ------------------------------------------------------------

set.seed(42)  # für Reproduzierbarkeit

n <- 100000
anzahl_mehr_als_3_schwarzfahrer <- 0

for (i in 1:n) {
  passagiere <- rpois(1, 46.8) # Anzahl Passagiere ~ Poisson(46.8)
  schwarzfahrer <- rbinom(1, passagiere, 0.07) # 7 % haben keinen gültigen Fahrausweis
  if (schwarzfahrer > 3) {
    anzahl_mehr_als_3_schwarzfahrer <- anzahl_mehr_als_3_schwarzfahrer + 1
  }
}

wahrscheinlichkeit <- anzahl_mehr_als_3_schwarzfahrer / n
cat("Wahrscheinlichkeit, dass >3 Personen keinen gültigen Fahrausweis haben:", round(wahrscheinlichkeit, 2), "\n")
