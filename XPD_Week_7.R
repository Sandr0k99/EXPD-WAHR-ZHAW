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

# c) d) e)
table(oil_data$Type)
levels(oil_data$Type)
table(oil_data$Alive)
levels(oil_data$Alive)
lat_oil <- 28.75389
long_oil <- -87.68528

par(mfrow = c(1, 2))
plot(Latitude ~ Longitude, data=oil_data,
     ylab = 'Längengrad', xlab='Breitengrad',
     col=Type)
legend('bottomleft', legend = c('birds', 'turtles'), fill = 1:2, bty = "n")
points(long_oil,lat_oil, pch=8, cex=2)
text(long_oil,lat_oil,"Deepwater Horizon",cex=0.8, pos=4)

plot(Latitude ~ Longitude, data=oil_data,
     ylab = 'Längengrad', xlab='Breitengrad',
     col= as.numeric(oil_data$Alive))
legend('bottomleft', legend = c('dead', 'alive'), fill = 1:2, bty = "n")
points(long_oil,lat_oil, pch=8, cex=2)
text(long_oil,lat_oil,"Deepwater Horizon",cex=0.8, pos=4)

# e)


     