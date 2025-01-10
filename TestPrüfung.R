library(MASS)

cats <- cats


boxplot(Bwt ~ Sex ,data=cats)

men <- cats[cats$Sex == 'M',]
mean(men$Bwt)
median(men$Bwt)
sd(men$Bwt)
mad(men$Bwt)

quantile(cats[cats$Sex == 'F', "Bwt"], probs=c(0.75))

# 2
urin <- GAGurine

plot(GAG ~ Age,data=urin)
plot(log(GAG) ~ Age,data=urin)

cor(urin$Age, urin$GAG)
cor(urin$Age, urin$GAG, method = "spearman")


#3
aids <- Aids2
nrow(aids)
str(aids)

nrow(aids[aids$sex == 'M',])
nrow(aids[aids$sex == 'F',])


barplot(table(aids[,c('sex')]))
levels(aids$T.categ)

mosaicplot(sex ~ T.categ,data=aids, las = 1)
pairs(aids)


#4
y1 <- function(x){
  return(5000 + 0.0025 * x)
}
y2 <- function(x){
  return(7750 + 0.002 * x)
}

lohn <- rep(55000000, 12)
sumy1 <- sapply(lohn, FUN=y1)
sumy2 <- sapply(lohn, FUN=y2)

mean(sumy1)
mean(sumy2)



#5
load('./Data/wine.Rdata')

#a)
str(wine)

#b)
library(ggfortify)
pca <- prcomp(wine, scale = T)
summary(pca) # Durch die ersten beiden Hauptkomponenten wird 55% der Varianz erklärt
plot(pca$x)

biplot(pca)
autoplot(pca, loadings = T, loadings.label = T, label=T, label.hjust=-0.3)

#6

load(file = './Data/custMat.Rdata')

custMat <- data.frame(knr = custMat[,1], artnr = custMat[,2])

s_artnr <-sapply(custMat[,2], FUN=function(x) gsub(pattern="artnr.", replacement = "", x=x))
custDat <- data.frame(knr = custMat[,1], artnr = s_artnr)

custDat$artnr <- as.integer(custDat$artnr)

load(file = './Data/prodNames.Rdata')

custDat <- merge(custDat, prodNames, by="artnr", all.x = T)

mergedata <- function(d1, d2, col){
    s_artnr <-sapply(d1[,col], FUN=function(x) gsub(pattern="artnr.", replacement = "", x=x))
    custDat <- data.frame(knr = d1[,1], artnr = s_artnr)
    custDat[,col] <- as.integer(custDat[,col])
    custDat <- merge(d1, d2, by=col, all.x = T)
    return(custDat)
}


#7

load(file = './Data/impfstoff.Rdata')

mosaicplot(alter ~ wirksamkeit,data=impfstoff)

nrow(impfstoff[impfstoff$alter == '65+' & impfstoff$wirksamkeit == 'ja',]) / nrow(impfstoff)
nrow(impfstoff[impfstoff$alter == '<=65' & impfstoff$wirksamkeit == 'ja',]) / nrow(impfstoff)

a <- impfstoff[impfstoff$impfstoff == 'A',]
b <- impfstoff[impfstoff$impfstoff == 'B',]

nrow(a[a$wirksamkeit == 'ja',]) / nrow(a)
nrow(b[b$wirksamkeit == 'ja',]) / nrow(b)

mosaicplot(alter ~ wirksamkeit,data=impfstoff)



























