#####
# 1

# a)
library(readxl)
fun_read_data <- function(file){
  abs <- as.data.frame(read_excel(file, skip = 8, n_max = 26,col_names = FALSE))
  nr_abs <- read_excel(file, col_names = FALSE, skip = 2, n_max = 1)
  rownames(abs) <- abs[,2]
  colnames(abs)[11] <- nr_abs
  v <- abs[,11]
  names(v) <- rownames(abs)
  return(v)
}
fun_read_data(file="./Data/Abstimmungen/je-d-17.03.03.bd.6280.k.xlsx")

# b)
files <- list.files("./Data/Abstimmungen/", full.names = T)

# c)
abst <- mapply(fun_read_data, files)

# d)
is(abst)
dim(abst)

# e)
colnames(abst)<-substr(colnames(abst),nchar(colnames(abst))-10, nchar(colnames(abst))-8)

summary(apply(abst,MARGIN=2,FUN=median))
summary(apply(abst,MARGIN=2,FUN=sd))

boxplot(abst,las=2,ylim=c(0,100),ylab="Ja-Anteilin%",cex.axis=0.8)
abline(h=50,lty=6)

# f)
rownames(abst) <- c("ZH","BE","LU","UR","SZ","OW","NW","GL",
                    "ZG","FR","SO","BS","BL","SH","AR","AI",
                    "SG","GR","AG","TG","TI","VD","VS","NE",
                    "GE","JU")
abst.pcS <- prcomp(abst,scale=F)

plot(abst.pcS$x[,1], abst.pcS$x[,2], type="n", xlab="PC1", ylab="PC2")
text(abst.pcS$x[,1], abst.pcS$x[,2], labels=row.names(abst), cex=0.8)


#Da die Variablen/Merkmale alle in Prozent sind, kann man auch mit den unskalierten Varia
#blen arbeiten. Allerdings werden dann die Variablen/Merkmale mit grosser Streuung die ersten
#Hauptachsen gegebenenfalls dominieren. Das kann in diesem Fall sogar gewünscht sein, weil Abstim
#mungen, bei denen sich alle Kantone gleich verhalten, ja nicht wirklich die Unterschiede zwischen
#den Kantonen aufzeigen.
#Stellt man die Kantone aufgrund der Daten aus den Volksabstimmungen in den ersten beiden
#Hauptkoordinaten dar, so zeigen sich Gruppen. Auch den berüchtigte “Röstigraben” kann man
#sehen (Westschweiz rechts, ost- und zentralschweizerischen Kantone links). Die Richtung der
#zweiten Hauptkomponente scheint Kantone mit grösseren Städten von eher ländlichen Gegenden
#zu unterscheiden.

# g)
# Genügen die beiden ersten Hauptkomponenten, um die Variabilität der Daten sinnvoll zu approximieren?
summary(abst.pcS)
#Die ersten beiden Hauptkomponenten erfassen zusammen 84% der Varianz, das liegt über dem
#Faustregel-Schwellwert von 80%.

# h)
names(abst.pcS$sdev) <- paste("PC", 1:length(abst.pcS$sdev), sep="")
screeplot(abst.pcS)
# oder
plot(abst.pcS$sdev**2, type="o")
abline(h=0, lty=2)
# Ein relativ klarer Knick ist bei Komponente 3 zu sehen, entsprechend reichen 2 Hauptkomponenten.














