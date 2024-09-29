#1
w <- c(1:5, 2) #numeric;double
x <- c(6:13)/3 #numeric;double
y <- c(TRUE, FALSE, TRUE, TRUE) #logical;logical
z <- c("Marie", "Betty", "Peter", "Peter") #charakter;#charakter
z_f <- as.factor(z) #numeric;integer

u <- (w <= 2)
u
mode(u)
as.numeric(u)
as.character(u)
as.character(z_f)
as.numeric(z_f)
as.numeric(z)
unique(z_f)
unique(w)
?unique

#2
seq(95,5,-5)
rep(letters, each=2)
rep(1:10, times=1:10)

#4
data <- read.csv('Data/Fragebogen_ExpD_hs24.csv')
str(data)
table(data$Haarfarbe)

rel_haeufigkeit_haarfarbe_prozent <- 100 * table(data$Haarfarbe) / nrow(data)
round(rel_haeufigkeit_haarfarbe_prozent, digits = 2)
col.haare <- c("yellow2","brown","black")
pie(table(data$Haarfarbe), col = col.haare)

tab.Schlafzimmer <- table(data$Schlafzimmer)
barplot(sort(tab.Schlafzimmer),
        main = "Absolute Häufigkeit der Stockwerke der Schlafzimmer",
        ylab = "Absolute Häufigkeit",
        col = rainbow(6), # Farbpalette
        las = 1)





