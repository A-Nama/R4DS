data(mtcars)

# Calculate mean and standard deviation
mpg_mean <- mean(mtcars$mpg)
mpg_sd <- sd(mtcars$mpg)


breaks <- seq(floor(min(mtcars$mpg)), ceiling(max(mtcars$mpg)), by=2)


hist_data <- hist(mtcars$mpg, breaks=breaks, plot=FALSE)


colors <- colorRampPalette(c("lightblue", "blue", "darkblue"))(length(hist_data$counts))

# Plot the histogram 
hist(mtcars$mpg, breaks=breaks, col=colors, border="black",
     main="Histogram of Miles per Gallon (mpg) in mtcars",
     xlab="Miles per Gallon (mpg)",
     ylab="Frequency")


abline(v = mpg_mean, col = "red", lwd = 2, lty = 2)   
abline(v = mpg_mean + mpg_sd, col = "darkgreen", lwd = 2, lty = 3) 
abline(v = mpg_mean - mpg_sd, col = "darkgreen", lwd = 2, lty = 3) 

# Add legend
legend("topright", legend=c(paste("Mean =", round(mpg_mean, 2)),
                            paste("Mean ± SD =", round(mpg_sd, 2))),
       col=c("red", "darkgreen"), lty=c(2,3), lwd=2)

# Display mean and standard deviation 
cat("Mean MPG:", mpg_mean, "\n")
cat("Standard Deviation of MPG:", mpg_sd, "\n")
