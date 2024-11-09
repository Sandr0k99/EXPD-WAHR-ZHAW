#####
# 1

# a)
load('./Data/bestellungen.rda')

plz_data <-read.csv('./Data/PLZO_CSV_LV03.csv', header = T, sep = ';')

str(bestellungen)
bestellungen_plz = merge(x =bestellungen,y= plz_data, by=c('PLZ','Zusatzziffer'), all.x = T)

# b)
bestellungen_kanton <- aggregate(id ~ Kantonskuerzel,
                                 data=bestellungen_plz,
                                 FUN=length)
colnames(bestellungen_kanton)[2] <- "n"
bestellungen_kanton$meanP <- aggregate(preis ~ Kantonskuerzel,
                                            data=bestellungen_plz,
                                            FUN= mean,
                                            na.rm = T)[,'preis']
bestellungen_kanton$maxP <- aggregate(preis ~ Kantonskuerzel,
                                      data=bestellungen_plz,
                                      FUN= max,
                                      na.rm = T)[,'preis']

bestellungen_kanton$lastOrder <- aggregate(timestamp ~ Kantonskuerzel,
                                           data=bestellungen_plz,
                                           FUN= max)[,'timestamp']

# c)
bestellungen_plz <- merge(x = bestellungen_plz, y = bestellungen_kanton,
                          by=('Kantonskuerzel'), all.x = T)

#####
# 2

# a)
load('./Data/currencies.rda')
str(currencies)

plot(EUR.CHF ~ Time,data=currencies, type='l', ylim=c(0,2), las=1)
lines(USD.CHF ~ Time, data=currencies, col=2)
lines(GBP.CHF ~ Time, data=currencies, col=3)
lines(JPY.CHF ~ Time, data=currencies, col=4)
legend("topright",
       legend=c("EUR.CHF","USD.CHF","GBP.CHF","JPY.CHF"),
       lty=1,col=1:4,bty="n")

# b)
curriecies_names <- colnames(currencies)
currencies_long_format <- reshape(data=currencies,
                                  varying = curriecies_names[c(1:4)],
                                  v.names = 'Kurs',
                                  idvar = 'ID',
                                  timevar = 'Waehrung',
                                  times= curriecies_names[c(1:4)],
                                  direction = 'long')

#c
plot(Kurs ~ Time,
     data=currencies_long_format,
     col=factor(currencies_long_format$Waehrung),
     type='p', pch=16, main='Wechselkurse')
legend("bottomleft",
       legend = c("EUR.CHF","USD.CHF",
                  "GBP.CHF", "JPY.CHF"),
       pch = 16, col = 1:4, bty = "n")

#####
# 3

# a)
?airquality
head(airquality)
str(airquality)

airquality$windstatus <- 'nicht-windig'
airquality[airquality$Wind >= 12,'windstatus'] <- 'windig'


# b)
table(airquality$windstatus, airquality$Month)

# c)
aggregate(airquality[,2:3], list(airquality$Month),
          FUN = 'mean', na.rm= T)


# d)
apply(airquality[,1:4],2, mean, na.rm=TRUE)
