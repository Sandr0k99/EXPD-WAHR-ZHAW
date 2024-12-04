#####
# 1)
library(ggplot2)

ggplot(diamonds,aes(x=carat, y=price, colour=clarity))+
  geom_point()+
  scale_x_continuous("Karat") +
  scale_y_continuous("Preis") +
  facet_grid(color ~ cut)

# 2)
ggplot()