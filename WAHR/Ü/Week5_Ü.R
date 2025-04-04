#####
# A1
# e) Erwartungswert
prob.urs <- c(0.1, 0.1, 0.2, 0, 0.2, 0.4)
tassen.urs <- 1:6
ew <- sum(tassen.urs*prob.urs)
ew

# f)
(var <- sum((tassen.urs-ew)**2*prob.urs))


#####
# A4
# a)
(p <- 288/8306)

# b)
n <- 24
(1-p)**n

# c)
# Wir interessieren uns hier für die Zufallsvariable ="Anzahl Tote bei der Expeditionsgruppe".
# Wenn wir davon ausgehen, dass es sich um 18 unabhängige Bernoulli-Experimente mit konstanter
# Sterbewahrscheinlichkeit handelt, so gilt X ∼ Bin(n = 18,p = 0.03467). Die gefragten Wahrschein
# lichkeiten bestimmen wir also mit der Binomialverteilung, am bequemsten natürlich mit R. Die
# Verteilung sieht wie folgt aus:

k <- 0:18
plot(k, dbinom(k, 18, p), type="h", main="Wahrscheinlichkeit für k Tote")

#i)
dbinom(0,18, p)
#ii)
dbinom(5,18, p)
#iii)
sum(dbinom(3:18,18,p))
#vi)
 sum(dbinom(0:2,18,p))




