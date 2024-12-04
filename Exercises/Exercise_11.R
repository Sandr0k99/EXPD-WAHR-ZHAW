#####
# a)
library(ggplot2)
data = read.csv('./Data/private-unfall-versicherung.csv', sep = ';', header = T)
nrow(data)
sqrt(nrow(data))

# b)
ggplot(data, aes(Schaden)) +
  geom_histogram(binwidth = 150)

ggplot(data, aes(Schaden)) +
  geom_histogram(boundary =100000)

# c)
data$lg <- log10(data$Schaden)
ggplot(data, aes(lg)) +
  geom_histogram()
#  geom_histogram(binwidth = 150)+
#  scale_x_log10(breaks=10)

# d)
sd(data$Schaden)
median(data$Schaden)
mean(data$Schaden)
mad(data$Schaden)

median(data$lg)
mean(data$lg)


# e)
ggplot(data,aes(Typ, Schaden))+
  geom_boxplot()



