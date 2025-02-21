#####
# 1

# a)
kreis <- function(r = 1){
  U <- 2*pi*r
  A <- pi * r**2
  return(list(U=U, A=A))
}
kreis(2)

# b)
mittelwert <- function(v){
  return(sum(v)/length(v))
}
mittelwert(1:100)
mean(1:100)
mittelwert(3)
mean(3)
mittelwert(c(2, 4, 5, NA, 6))
mean(c(2, 4, 5, NA, 6))

#####
# 2

# a)
eng <- read.table("./Data/NotenSek3-SO-englisch.csv", sep = ";", header = F, stringsAsFactors = F)
eng <- eng[,-3]
names(eng) <- c("name", paste0("e", 1:6))

french <- read.table("./Data/NotenSek3-SO-franzoesisch.csv", sep = ";", header = F, stringsAsFactors = F)
names(french) <- c("name", paste0("f", 1:7))

math <- read.table("./Data/NotenSek3-SO-mathematik.csv", sep = ";", header = F, stringsAsFactors = F)
names(math)<-c("name",paste0("m",1:6))

nat <- read.table("./Data/NotenSek3-SO-naturlehre.csv", sep = ";", header = F, stringsAsFactors = F)
names(nat) <- c("name", paste0("n", 1:3))

# b)
marks0 <- list(eng = eng, french = french, math = math, nat = nat)
str(marks0)

# c)
sapply(marks0, FUN = nrow)

# d)
reorderDF <- function(x) {
  return(x[order(x[, 1]), ])
}

marks1 <- lapply(marks0, FUN = reorderDF)

# e)
getName <- function(x){
  return(x[, 1])
}

namMat <- sapply(marks1, FUN = getName)
apply(namMat, MARGIN = 1, function(x) {
  length(unique(x)) == 1
  })

# f)
endnoten <- sapply(marks1, FUN=function(x) rowMeans(x[,-1], na.rm=TRUE))
rownames(endnoten) <- getName(marks1$eng)
endnoten

# g)
fun_fehlende<-function(x){
  apply(x[,-1],1,FUN=function(x) sum(is.na(x)))
}
fehlende<-sapply(marks1,FUN=fun_fehlende)
rownames(fehlende)<-getName(marks1$eng)
fehlende

#####
# 3

# a)
dt <- Sys.time()

# b)
mode(dt)
typeof(dt)
class(dt)

# c)
dt <- as.POSIXct(dt, tz = "UTC")

# d)
gb <- as.Date("01.12.1999", "%d.%m.%Y")
weekdays(gb)
quarters(gb)
months(gb)

Ygb <- as.numeric(format(gb,"%Y"))
Ydt <- as.numeric(format(dt,"%Y"))
Ydt- Ygb

difftb <- difftime(time1 = dt,time2= gb, units = "weeks")
difftb <- as.numeric(difftb)
round(difftb/52,0)

#####
# 4

# a)
install.packages("SwissAir")
library(SwissAir)
data(AirQual)

AirQual$time <- as.POSIXct(AirQual$start, format = "%d.%m.%Y")
plot(lu.O3 ~ time, data = AirQual, type = "l", main = expression(O[3]-Konzentration~Luzern),
     ylab = expression(ppm~~O[3]))

# b)
plot(lu.O3 ~ time, data = AirQual, type = "l", main = expression(O[3]-Konzentration~Luzern),
     ylab = expression(ppm~~O[3]), xaxt = "n")
axis.POSIXct(1, at = seq(min(AirQual$time), max(AirQual$time), "weeks"), format = "%W")