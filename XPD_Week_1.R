v <- 1:12
m1 <- matrix(data = v, nrow = 3, ncol = 4, byrow = FALSE)
m1
help(matrix)
m2 <- matrix(data = v, nrow = 3)
m2 # Was ist der Unterschied zu m1?
nrow(m1)
ncol(m1)
dim(m1)
colnames(m1)
rownames(m1)
colnames(m1) <- c("Spalte 1", "Spalte 2", "Spalte 3", "Spalte 4")
rownames(m1) <- c("Zeile 1", "Zeile 2", "Zeile 3")
m1
2 * m1
m1 - m2
m1 * m2
m3 <- matrix(data = 1:8, ncol = 2, byrow = TRUE)
m3
m1 * m3

x <- matrix(1:8,5,8,TRUE)
colnames(x) <- c("einsen","zweien","dreien","vieren","fünfen","sechsen","sieben","achten")
rownames(x) <- c("r1","r2","r3","r4","r5")
x
sum(x)

x1 <- seq(10,100,10)
x1
