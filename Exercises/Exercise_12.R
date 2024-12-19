velo_data <-readRDS('./Data/velozaehlung.rds')

mean(velo_data$`Mythenquai Richtung Innenstadt`, na.rm = T)
sd(velo_data$`Mythenquai Richtung Innenstadt`, na.rm = T)
