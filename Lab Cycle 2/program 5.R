data(mtcars)

mtcars$gear <- as.factor(mtcars$gear)

boxplot(hp ~gear, 
        data = mtcars, 
        main = "Horsepower Distribution by number of Gears",
        xlab = "NUmber of gears",
        ylab = "Hoorsepower (hp)",
        col = c("lightblue", "lightgreen", "pink"),
        border = "darkblue")

for (g in levels(mtcars$gear)){
  gear_data <- mtcars$hp[mtcars$gear == g]
  outliers <- boxplot.stats(gear_data)$out
  
  if(length(outliers) > 0){
    outlier_positions <- which(gear_data %in% outliers)
    
    text(x = as.numberic(g) + 0.05,
         y = outliers,
         labels = rownames(mtcars[mtcars$gear == g, ])[outlier_positions],
         pos = 4, cex = 0.8, col = "red")
  }
}

legend("topright",
       legend = paste(levels(mtcars$gear), "Gears"),
       fill = c("lightblue", "lightgreen", "lightpink"),
       border = "darkblue",
       title = "Gear Legend")
