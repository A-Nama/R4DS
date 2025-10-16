titanic <- read.csv("titanic.csv", stringsAsFactors = TRUE)

str(titanic)
titanic$parch <-as.numeric(as.character(titanic$Parch))
summary(titanic)

colSums(is.na(titanic))

hist(titanic$parch, 
     main = "Distribution of a Number of Parents/Children Aboard",
     xlab = "Number of Parents/Children (parch)",
     ylab = "Frequency",
     col = "lightblue",
     border = "darkblue")


table(titanic$Survived)
prop.table(table(titanic$Survived))

barplot(table(titanic$Sex, titanic$Survived),
        beside = TRUE,
        col = c("lightcoral", "lightblue"),
        legend = c("Female", "Male"),
        main = "Survival by Gender",
        xlab = "Survival ( 0 = Died, 1 = Survived",
        ylab = "Count")

aggregate(Survived ~ Sex, data = titanic, mean)

barplot(table(titanic$Pclass, titanic$Survived),
        beside = TRUE,
        col = c("gold", "lightgreen", "lightpink"),
        legend = c("1stclass", "2ndclass", "3rdclass"),
        main = "Survival by Passenger Class",
        xlab = "Survival ( 0 = Died, 1 = Survived",
        ylab = "Count")

aggregate(Survived ~ Pclass, data = titanic, mean)





