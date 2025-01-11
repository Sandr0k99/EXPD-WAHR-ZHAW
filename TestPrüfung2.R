#1
load('./Data/daten_propepruefung2.rda')

nrow(cyber)

nrow(cyber[cyber$Type == 'Theft',])

sort(table(cyber$State), decreasing = T)[1:3]

sum(cyber$Affected) / 52

cyber$YearNumeric2 <- as.numeric(gsub(x=cyber$MonthYear, pattern="MMM_", replacement = ""))


levels()
plot(Affected ~ YearNumeric2,data=cyber, col=c(1:29)[cyber$Type])
cor(cyber$Affected, cyber$YearNumeric2, method = 'spearman')

df_cyber <- data.frame(cyber[cyber$Location %in% c('Laptop', 'Desktop Computer', 'Network Server'),])


k <- 1.58 * IQR(df_cyber$Affected) / sqrt(sum(cyber$Affected))


# 3
Gigglezol

boxplot(Lachwert ~ Gruppe,data=Gigglezol, notch=T)


#4
gold_news$Forecast <- as.numeric(gold_news$Forecast)
gold_price$Price <- as.numeric(gold_price$Price)

gold_news$Date <- as.Date(gold_news$Date)
gold_price$Date <- as.Date(gold_price$Date)

gold_news <- gold_news[gold_news$Inflation_news == T,]
gold_news <- gold_news[gold_news$Inflation_news == T 
                       & grepl(x=gold_news$Event, pattern="inflation", ignore.case = T),]

gold_merged <- merge(x=gold_news, y=gold_price, by="Date", all.x=T)

t <- quantile(gold_merged$Price, probs=c(0.1,0.9), na.rm=T)


get_80pc_span <- function(goldprices){
  quantile(goldprices, probs=c(0.1,0.9), na.rm=T)
  span <- abs(t[1]$`10%` - t[2]$`90%`)
  return(span)
}

get_80pc_span(gold_merged$Price)

sum_country <- aggregate(Price ~ Country ,data=gold_merged, FUN = sum)
sum_country
sapply()

#5

pca <- prcomp(Siebenkampf[,5:11], scale = F)

library(ggfortify)

autoplot(pca, loadings = T, loadings.label = T, label=T, label.hjust=-0.3)

summary(pca)
















