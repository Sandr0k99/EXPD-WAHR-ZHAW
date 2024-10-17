####
#1
library(readxl)
dat <- readxl::read_xlsx("../R/Data/Fragebogen_ExpD_hs24.xlsx")
dat <- dat[dat$Augenfarbe!="",]
dat <- dat[!is.na(dat$Schlafzimmer),]

# a)
AugFaFact <- factor(dat$Augenfarbe)
str(AugFaFact)
nlevels(AugFaFact)
summary(AugFaFact)
#7 levels

# b)
levels(AugFaFact)

# c)
AugFaFactSimple <- factor(
  dat$Augenfarbe,
  levels = levels(AugFaFact),
  labels = c('blau','blau', 'braun', 'braun', 'braun', 'gruen', 'gruen')
  )
summary(AugFaFactSimple)

# d)
AugFaFactSimple <- factor(
  AugFaFactSimple,
  levels = c('braun', 'blau', 'gruen')
)

barplot(table(AugFaFactSimple) / length(AugFaFactSimple))

# e)
AugHaarTable
AugHaarTable <- table(AugFaFactSimple, dat$Haarfarbe)
barplot(
  AugHaarTable,
  beside = TRUE,
  xlab = 'Haarfarbe',
  col=c('brown', 'blue', 'green'))

legend('topright',title='Augenfarbe', legend=c('Braun','Blau', 'Grün'), fill = c('brown', 'blue', 'green'))
  
# f)







