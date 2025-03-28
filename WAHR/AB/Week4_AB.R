#####
# A1
# a)
func.aids_test <- function(prae, sens, spez){
  res <- sens*prae / (sens*prae + (1-spez)*(1-prae))
  return (res)
}

#sens: elisa+|hiv+ = 99.7
#spez: elisa-|hiv- = 98.5
#prev: hiv+ = 0.5

# Wahrscheindlichkeit dass man HIV positiv ist wenn der Test positiv ist
func.aids_test(0.005, 0.997, 0.985)


# b)
res1 <- func.aids_test(0.005, 0.997, 0.985)
func.aids_test(res1, 0.997, 0.985)

# c)
prae <- seq(0.001, 0.3, by = 0.001)
res2 <- rep(NA, length(prae))
for (i in 1:length(prae)){
  res2[i] <- func.aids_test(prae = prae[i], sens = 0.997, spez = 0.985)
}

plot(prae, res2, type = 'l', ylim = c(0,1), xlab = "Prävalenz",
     ylab = 'P(HIV+| Test positiv)')



