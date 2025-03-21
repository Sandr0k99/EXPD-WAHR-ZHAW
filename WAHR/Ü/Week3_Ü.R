#####
# Aufgabe 4
# c)

sim <- 10000
n <- 28
count <- 0
for (i in 1:sim){
  geburtstage <- sample(1:365, size=n, replace=TRUE)
  if (max(table(geburtstage))>1) count <- count + 1
}
count/sim

# d)
geburtstag_fun <- function(n){
  p <- 1
  for (i in 1:n){
    p <- p*(365-(i-1))/365
  }
  1-p
}
geburtstag_fun(23)
