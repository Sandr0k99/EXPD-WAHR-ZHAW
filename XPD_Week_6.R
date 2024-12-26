#####
# 1
library(readxl)
dat <- readxl::read_xlsx("./Data/Fragebogen_ExpD_hs24.xlsx")
dat$Koerpergroesse <- as.numeric(dat$Koerpergroesse)

# a)
plot(Schuhgrösse ~ Koerpergroesse, data=dat)

# b)
dat <- dat[dat$Koerpergroesse < 190,]
plot(Schuhgrösse ~ Koerpergroesse, data=dat,
     main = "Streudiagramm Schugrösse und Körpergrösse",
     asp = 1)

#c)
dat$geschlechtCol <- ''
dat$geschlechtCol[dat$Geschlecht == 'männlich'] <- 'seagreen4'
dat$geschlechtCol[dat$Geschlecht == 'weiblich'] <- 'red4'
plot(Schuhgrösse ~ Koerpergroesse, data=dat,
     main = "Streudiagramm Schugrösse und Körpergrösse",
     asp = 1,
     col = dat$geschlechtCol)
legend("topleft", legend = c("weiblich", "männlich"), fill = c("red4", "seagreen4"), bty = "n")

#d)
dat$Haarfarbe <- factor(dat$Haarfarbe)
plot(Schuhgrösse ~ Koerpergroesse, data=dat,
     main = "Streudiagramm Schugrösse und Körpergrösse",
     asp = 1,
     col = dat$geschlechtCol,
     pch = as.numeric(dat$Haarfarbe))

#e)
legend("topleft", legend = c("weiblich", "männlich","blond","braun", "schwarz"),
       fill = c("red4", "seagreen4", rep(NA, 3)),
       border = c("red4", "seagreen4", rep(NA, 3)),
       pch = c(rep(NA,2),1:3),
       bty = "n")

#####
# 2

#a)
dias <- read.csv(file = './Data/diamanten.csv', sep=";")
hist(dias$price)

#b)
x <- quantile(dias$price, probs = c(0.25,0.5,0.75))
abline(v = x, lty = 2, col = "red")

#c)
barplot(sort(table(dias$cut), decreasing=T),
        xlab = "Qualität", ylab = "absolute Häufigkeit",
        main = "Balkendiagramm über Qualität")

#d)
pairs(dias[,c("price","carat", "depth")],col= rgb(0,0,0,alpha=0.2))

#f)
dias$clarity<-factor(dias$clarity,levels=c("I1","SI1","SI2",
                                         "VS1","VS2","VVS1","VVS2","IF"))
boxplot(price ~ clarity, data=dias, las=2)
