#####
#1
library(readxl)
dat <- readxl::read_xlsx("./Data/Fragebogen_ExpD_hs24.xlsx")
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
SchlafFact <- factor(
  dat$Schlafzimmer
)
levels(SchlafFact)

SchlafSelect <- dat$Schlafzimmer == "3. Stock" | dat$Schlafzimmer == "4. Stock"
n <- nrow(dat[!is.na(dat$Schlafzimmer),])
sum(SchlafSelect, na.rm = T) / n
#oder

sum(dat$Schlafzimmer %in% c(levels(SchlafFact)[4:5])) / n


#####
#2
censData <- read.table('./Data/censUSA.txt',sep = "\t", header = T)

# a)
censData$education <- gsub(" ","",censData$education)
censData$occupation <- gsub(" ","",censData$occupation)
censData$education <- factor(censData$education)
censData$occupation <- factor(censData$occupation)

# b)
EduOccTable <- table(censData$education, censData$occupation)

mosaicplot(EduOccTable, las = 2,
           xlab = 'education', ylab = 'occupation',
           main = "education vs occupation")

# c)
levels(censData$education)

eduLev <- c("Preschool", "1st-4th", "5th-6th", "7th-8th", "9th",
            "10th", "11th", " 12th", "HS-grad", "Prof-school",
            "Assoc-acdm", "Assoc-voc", "Some-college", "Bachelors",
            "Masters", "Doctorate")
eduRedLev <- c(rep("basicEdu", 8), "HS-grad",
               "Prof-school-Doctorate", rep("Assoc-acdm-voc",2),
               "Some-college","Bachelors", "Masters", "Prof-school-Doctorate")

censData$eduRed <-factor(censData$education,
       levels = eduLev,
       labels = eduRedLev)

OccLevSort <- names(sort(table(censData$occupation)))

censData$occupation <- factor(censData$occupation, levels=OccLevSort)


mosaicplot(censData$eduRed ~ censData$occupation, las = 2,
           xlab = 'education', ylab = 'occupation',
           main = "education vs occupation")
