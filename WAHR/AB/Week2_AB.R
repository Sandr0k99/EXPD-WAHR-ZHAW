#####
#Aufgabe 1

n_würfe <- 50000

#a
würfe <- sample(1:6,n_würfe, replace = T)
barplot(table(würfe))

#b
würfe_man <- sample(1:6, size = n_würfe, replace = T, prob = c(rep(0.2/5, 5), 0.8))

#c
sum(würfe == 6) / n_würfe
sum(würfe_man == 6) / n_würfe

#d
res <- sample(1:6, n_würfe* 10, replace = T) # 50000 Versuche * 10 Würfe
RES <- matrix(res, nrow = n_würfe)

n_sechs <- rowSums(RES == 6) # wie oft eine Sechs gewürfelt wurde in 50000 Versuchen
barplot(table(n_sechs))

        