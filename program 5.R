n_str <- readline(prompt = "Enter the number of terms: ")
n <- as.integer(n_str)

if (!is.na(n) && n > 0) {
  total_sum <- 0
  
  for (i in 1:n) {
    term <- ((-1)^(i + 1) * i) / (2 * i - 1)
    total_sum <- total_sum + term
  }
  
  cat("The sum of the series up to", n, "terms is:", total_sum, "\n")
  
} else {
  cat("Invalid input. Please enter a positive integer.\n")
}
