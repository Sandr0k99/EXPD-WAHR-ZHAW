data <- read.csv('./Data/MBA.csv')

install.packages("GGally")
library("GGally")
library("ggplot2")
library(dplyr)
ggpairs(data[,c('gender','gpa', 'gmat')])

median(data$gmat)
quantile(data$gmat)

boxplot(data$gmat~data$major,
        ylab="GMAT score",
        xlab="Undergraduate Major")


barplot(table(data$gmat),
        ylab = 'Count',
        xlab = 'GMAT score')

barplot(gmat ~ gender,data=data)

races <- data$race[!is.na(data$race) & data$race != ""]
pie(table(races))


cor(data$gmat, data$gpa)

ggplot(data, aes(x = gmat, fill = gender)) +
  geom_histogram(bins = 30, position = "identity", color = "white", alpha = 0.7) +
  facet_grid(gender ~ .) +
  coord_flip() +
  theme_minimal() +
  labs(x = "GMAT Score", y = "Count", title = "Distribution of GMAT Scores by Gender")



barplot(table(data$gmat),
        ylab = 'Count',
        xlab = 'GMAT score')

ggplot(data, aes(x=gmat))+
  geom_histogram(color='black', fill='lightblue', binwidth = 10,boundary = 560)+
  scale_x_continuous(breaks = seq(540,800, by = 20))+
  theme_minimal() +
  labs(title = "GMAT Score of Applicants of Warthon Class", y='Count', x='GMAT Score')



races <- data$race[!is.na(data$race) & data$race != ""]
race_counts <- table(races) %>%
  as.data.frame() %>%
  rename(Race = races, Count = Freq) %>%
  mutate(Percentage = Count / sum(Count) * 100)

ggplot(race_counts, aes(x = "", y = Count, fill = Race)) +
  geom_bar(stat = "identity", width = 1)+
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  coord_polar(theta = "y") +
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  labs(title = "Distribution of Races", fill = "Race", x="", y ="") +
  scale_fill_brewer(palette = "Set2")
