data(mtcars)

colors <- c("maroon", "pink","gold")[as.factor(mtcars$cyl)]

plot(mtcars$disp, mtcars$mpg,
     col = colors,
     pch= 19,
     main = "Relationship between Displacement and MPG in Cars",
     xlab = "Displacement (cu. in.)",
     ylab = "Miles per Gallon (MPG)")

lines(lowess(mtcars$disp, mtcars$mpg), col = "black", lwd = 2)

legend ("topright",
        legend = paste(unique(mtcars$cyl), "Cylinders"),
        col = colors,
        pch = 19,
        title = "Number of cylinders")
