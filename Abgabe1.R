data <- read.csv('Data/MBA.csv')

median(data$gmat)
quantile(data$gmat)

boxplot(data$gmat~data$major,
        ylab="GMAT score",
        xlab="Undergraduate Major")


barplot(table(data$gmat),
        ylab = 'Count',
        xlab = 'GMAT score')


data_clean <- data$race[!is.na(data$race) & data$race != ""]
pie(table(data_clean))


cor(data$gmat, data$gpa)
