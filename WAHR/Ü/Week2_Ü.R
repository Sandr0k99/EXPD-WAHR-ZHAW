#####
# A1

set.seed(125)
N <- 100000 # Anzahl Zufallsexperimente
count <- 0 # Zählt die 5-stellige Zahlen, die kleiner als 41'721 sind
for( i in 1:N){
  # Ziehen von 5 zufällige Zahlen aus der Menge {1,2,4,7},
  # wobei p(1) = 1/8, p(2) = 1/4, p(4) = 1/8 und p(7) = 1/2
  sim <- sample(c(1,2,4,7), prob=c(1/8, 1/4, 1/8, 1/2),
                size=5, replace=TRUE)
  # Erezugen der fünf-stelligen Zahl
  # dafür multiplizieren wir sim mit dem
  # Vector c(10000, 1000, 100, 10, 1) und bilde die Summe
  zahl <- sum(sim * 10**(4:0))
  # Ist die zahl < 41721?
  s <- ifelse(zahl < 41721, 1, 0)
  # Falls s = TRUE, wird count um 1 erhöht, ansonsten um 0
  count <- count + s
}
count
#W'keit alsRelativeHäufigkeit
sim.res<-count/N
sim.res

######
# A2
set.seed(125)
N<-100000 # Anzahl Simulationen
count<-0 # Zählt, wie oft keine blaue Kugel gezogen wurde
kugeln <-c(rep("Rot",4),rep("Gelb",2),rep("Blau",3))

for(i in 1:N){
  # Ziehen von 2 Kugeln aus der Urne
  zug_i <- sample(kugeln, size=2, replace=FALSE)
  # Ist keine blaue Kugel dabei?
  s <- (zug_i[1] != "Blau" & zug_i[2] != "Blau")
  # Falls keine blaue Kugel, dann + 1
  count <- count + s
}
# W'keit als Relative Häufigkeit
count/N
