#####
# 1
data <- read.table('./Data/imports85.txt', sep = '\t', header = T)

data$cyl <- factor(data$cyl) #damit kategorisiert -> nicht einfach strings

# a)
m <- tapply(data$price, INDEX = data$cyl, FUN = "mean")
s <- tapply(data$price, INDEX = data$cyl, FUN = "sd")
cbind(Mittelwert = m, Standardabweichung = s)

# b)
par(mfrow=c(1,2))
boxplot(price ~ cyl, data = data)
stripchart(price ~ cyl, data = data, vertical=T, method = 'stack')

# c)
