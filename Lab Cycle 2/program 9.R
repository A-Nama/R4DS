library(ggplot2)
library(GGally)
library(dplyr)

data("iris")

str(iris)
summary(iris)

colSums(is.na(iris))

anova_result <- aov(Sepal.Length ~ Species, data = iris)
summary(anova_result)
print(anova_result)

ggpairs(iris, aes(color = Species, alpha = 0.7))
p <- ggplot(iris, aes(x=Species, y = Sepal.Length, fill = Species))+
  geom_boxplot(alpha = 0.7) +
  labs(title = "Sepal length by Species", y = "Sepal Length (cm)") +
  theme_minimal()

print(p)