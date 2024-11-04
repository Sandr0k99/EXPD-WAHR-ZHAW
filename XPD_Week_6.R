#####
# 1
library(readxl)
dat <- readxl::read_xlsx("./Data/Fragebogen_ExpD_hs24.xlsx")
dat <- dat[dat$Augenfarbe!="",]
dat <- dat[!is.na(dat$Schlafzimmer),]
dat$Koerpergroesse <- as.numeric(dat$Koerpergroesse)

# a)
plot(Schuhgrösse ~ Koerpergroesse, data=dat)

# b)
dat <- dat[dat$Koerpergroesse < 190,]
plot(Schuhgrösse ~ Koerpergroesse, data=dat,
     main = "Streudiagramm Schugrösse und Körpergrösse",
     asp = 1, lpars = list(col = 'red'))
