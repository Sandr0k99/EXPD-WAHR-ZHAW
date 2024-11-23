#####
# 1)
library("ggplot2")
head(iris)

plot(Sepal.Width ~ Sepal.Length,data=iris, col= c('red', 'green', 'blue')[iris$Species])
legend('topright' ,legend = unique(iris$Species), fill = c('red', 'green', 'blue'))

ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, size=Petal.Width, col=Species)) +
  geom_point()

#####
# 2)
# a)
ggplot(iris, aes(x=Species, fill=Species)) +
  geom_bar()

# b)
ggplot(iris, aes(y=Sepal.Width, x=Species))+
  geom_boxplot(fill='pink')

# c)
ggplot(iris, aes(x=Sepal.Width, fill = Species))+
  geom_density(alpha = 0.2)

# d)
ggplot(iris, aes(x=Species, y=Sepal.Width, fill=Species))+
  geom_violin()+
  geom_jitter(col = 'black', size = 1.5,width = 0.1, height = 0.05)
