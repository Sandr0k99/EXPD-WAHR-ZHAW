#####
# A1
# d)
P_1 <- choose(2,2)/choose(5,2)
P_2 <- (choose(2,1)*choose(3,1))/choose(5,2)
P_3 <- choose(3,2)/choose(5,2)
sample(c(-6,-1, 4), 8, prob=c(P_1, P_2, P_3), replace=TRUE)

#####
# A3
# a)
plot(x=c(-6,-1, 4), y=c(P_1, P_2, P_3),
     type ="h", xlab=expression(x[i]),# expression(x[i]) = "X tief i"
     ylab=expression(p(x[i])), ylim=c(0,0.7)) #expression(p(x[i])) = "funktion p von X tief i"

# b)
plot(stepfun(c(-6,-1,4), cumsum(c(0,P_1, P_2, P_3))),
     main = expression(F(x) == P(X<=x)), ylab = "F(x)")

# d)
N <- 10000
samp <- rep(NA, times=N)
for (i in 1:N){
  samp[i] <- sample(c(-6,-1, 4), 1, prob=c(P_1, P_2, P_3))
}
mean(samp)

#####
# A4
# b)
N <- 10000
samp <- rep(NA, times=N)
for (i in 1:N){
  samp[i] <- sample(c(-6,-1, 4), 1, prob=c(P_1, P_2, P_3))
}
var(samp)

#####
# A5
# a)
p <- 28/34

# b)
# X ~ Bin(n = 22, p = 28/34)

# c)
sum(dbinom(20:22, 22, 28/34))







