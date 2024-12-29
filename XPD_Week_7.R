#####
# 1

# a)
oil_data <- read.csv('./Data/oil-animals.csv')
head(oil_data)
summary(oil_data)
str(oil_data)

oil_data$Alive <- as.factor(oil_data$Alive)
oil_data$Type <- factor(oil_data$Type)

# b)
plot(Latitude ~ Longitude, data=oil_data,
     ylab = 'Längengrad', xlab='Breitengrad')

# c) d) e) f)
table(oil_data$Type)
levels(oil_data$Type)
table(oil_data$Alive)
levels(oil_data$Alive)
lat_oil <- 28.75389
long_oil <- -87.68528

palette(c("black","#377EB8","#E41A1C","#4DAF4A",
          "#984EA3","#FF7F00","gray"))
par(mfrow = c(1, 2))
plot(Latitude ~ Longitude, data=oil_data,
     ylab = 'Längengrad', xlab='Breitengrad',
     col=Type)
legend('bottomleft', legend = c('birds', 'turtles'), fill = 1:2, bty = "n")
points(long_oil,lat_oil, pch=8, cex=2)
text(long_oil,lat_oil,"Deepwater Horizon",cex=0.8, pos=4)

plot(Latitude ~ Longitude, data=oil_data,
     ylab = 'Längengrad', xlab='Breitengrad',
     col= as.numeric(oil_data$Alive)+3)
legend('bottomleft', legend = c('dead', 'alive'), fill = 4:5, bty = "n")
points(long_oil,lat_oil, pch=8, cex=2)
text(long_oil,lat_oil,"Deepwater Horizon",cex=0.8, pos=4)

# h)
pdf('./Saves/oil.pdf',height=6, width=6 *12/7)
plot(Latitude ~ Longitude, data=oil_data,
     ylab = 'Längengrad', xlab='Breitengrad',
     col=Type)
legend('bottomleft', legend = c('birds', 'turtles'), fill = 1:2, bty = "n")
points(long_oil,lat_oil, pch=8, cex=2)
text(long_oil,lat_oil,"Deepwater Horizon",cex=0.8, pos=4)
dev.off()

png('./Saves/oil.png',width=800)
plot(Latitude ~ Longitude, data=oil_data,
     ylab = 'Längengrad', xlab='Breitengrad',
     col= as.numeric(oil_data$Alive)+3)
legend('bottomleft', legend = c('dead', 'alive'), fill = 4:5, bty = "n")
points(long_oil,lat_oil, pch=8, cex=2)
text(long_oil,lat_oil,"Deepwater Horizon",cex=0.8, pos=4)
dev.off()

#####
#2
     
cast_data <- read.csv('./Data/oil-animals-cast.csv')
cast_data$Alive <- as.factor(cast_data$Alive)
turtles <- cast_data[cast_data$Type == "turtles",]
birds <- cast_data[cast_data$Type == "birds",]

par(mfrow=c(1,2))

plot(number ~ week.number, data=turtles[turtles$Alive == 'Y',],
     type='l', lty=1, xlab='week')
lines(turtles[turtles$Alive == 'N',]$week.number, turtles[turtles$Alive == 'N',]$number, lty=2)
legend('topleft', legend=c('Alive', 'Dead'), bty="n", lty=1:2)

plot(number ~ week.number, data=birds[birds$Alive == 'N',],
     type='l', lty=2, xlab='week')
lines(birds[birds$Alive == 'Y',]$week.number, birds[birds$Alive == 'Y',]$number, lty=1)
legend('topleft', legend=c('Alive', 'Dead'), bty="n", lty=1:2)
