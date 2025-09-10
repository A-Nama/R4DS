n <- as.integer(readline(prompt = "Enter the number of terms: "))

if(is.na(n) || n <= 0){
  cat("Please enter a positive integer.\n")
} else {
  fib_series <- c(0, 1, 1)
  
  if(n <= 3){
    print(fib_series[1:n])
  } else {
    for (i in 4:n){
      next_term <- sum(fib_series[(i-3): (i-1)])
      fib_series <- c(fib_series, next_term)
    }
    
    print(fib_series)
  }
}
