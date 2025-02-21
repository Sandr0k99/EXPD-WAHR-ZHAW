#####
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

#####
#2
seq(95,5,-5)
rep(letters, each=2)
rep(1:10, times=1:10)
matrix(seq(3,75,by=3), ncol = 5,byrow = TRUE)

#####
#3
dat = data.frame(Name = c("Ruedi","Hans","Peter","Stefan","Julia","Maria"),
           Alter = c(27,34,21,25,29,23),
           Note = c(5.0,6.0,5.5,4.0,3.5,5.0),
           Studium = c(FALSE,TRUE,TRUE,TRUE,FALSE,TRUE))

nrow(dat)
dim(dat)
str(dat) #chr;num;num;logi
pie(table(dat$Studium))


#####
#4
data <- read.csv('Data/Fragebogen_ExpD_hs24.csv')
str(data)

table(data$Haarfarbe) #absolute Häufigkeit
rel_haeufigkeit_haarfarbe_prozent <- 100 * table(data$Haarfarbe) / nrow(data) #rel. Häufigkeit
round(rel_haeufigkeit_haarfarbe_prozent, digits = 2)

col.haare <- c("yellow2","brown","black")
pie(table(data$Haarfarbe), col = col.haare)

tab.Schlafzimmer <- table(data$Schlafzimmer)
tab.Schlafzimmer
barplot(tab.Schlafzimmer,
        main = "Absolute Häufigkeit der Stockwerke der Schlafzimmer",
        ylab = "Absolute Häufigkeit",
        col = rainbow(6), # Farbpalette
        las = 1)

tab.Plz = table(data$Postleitzahl)
tab.Plz
barplot(sort(tab.Plz,decreasing = TRUE),
        main = "Postleitzahlen",
        las = 2,
        cex.names = 0.9)

