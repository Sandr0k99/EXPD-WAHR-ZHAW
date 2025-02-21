load('./Data/RetailDataA.rdata')
str(RetailData1)

nrow(RetailData1)

RetailData1$Country <- factor(RetailData1$Country)

table(RetailData1$Country)

table(RetailData1[RetailData1$Description == "PHOTO CUBE",]$Country)


nrow(RetailData1[RetailData1$Quantity < 0,])


RetailData1$Umsatz <- RetailData1$Quantity * RetailData1$UnitPrice

RetailData1$Sum <- RetailData1$Umsatz * RetailData1$Quantity

apply(RetailData1$Umsatz, MARGIN = 1, FUN = sum)

quantile(RetailData1$Umsatz, probs = c(.85))


hist(RetailData1$UnitPrice,
     breaks = seq(0,200,5),
     ylab = 'Anzahl',
     xlab = 'Preis',
     main='Preisverteilung')


sort(table(RetailData1$CustomerID))


RetailData1[RetailData1$CustomerID == '14911','Quantity']

apply(RetailData1[RetailData1$CustomerID == '14911','Quantity'], Margin = 1,FUN = sum)
sum(RetailData1[RetailData1$CustomerID == '14911','Quantity'])


plot(Quarter ~ Country,data=RetailData1, las=1)

sum(RetailData1$Umsatz[RetailData1$Quantity >0]) - sum(RetailData1$Umsatz[RetailData1$Quantity >0])

rdat <- RetailData1[RetailData1$Country == 'Switzerland' & RetailData1$Quantity > 0,]
plot(Umsatz ~Quarter,data=rdat)
