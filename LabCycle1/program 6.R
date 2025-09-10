is_prime <- function(n){
  if (n <= 1){
    return(FALSE)
  }
  for(i in 2:sqrt(n)){
    if(n %% i == 0){
      return(FALSE)
    }
  }
  return(TRUE)
}

primes_in_range <- function(start, end){
  primes <- c()
  for(n in start:end){
    if(is_prime(n)){
      primes <- c(primes, n)
    }
  }
  return(primes)
}

cat("Prime NUmber Checker \n")
cat("1. Check if a number is prime \n")
cat("2. FInd all primes in a range \n")

choice <- as.integer(readline(prompt = "Enter a number: "))

if(choice == 1){
  number <- as.integer(readline(prompt = "Enter a number: "))
  if(is_prime(number)){
    cat(number, "is a prime number. \n")
  } else {
    cat(number, "is not a prime number. \n")
  }
} else if (choice == 2){
  start <- as.integer(readline(prompt = "Enter start of range: "))
  end <- as.integer(readline(prompt = "Enter the end of range: "))
  if (start > end){
    cat("invalid range. Start must be less than or equal to end. \n")
  } else {
    primes <- primes_in_range(start, end)
    cat("Prime numbers between ", start, "and ", end, "are:\n")
    print(primes)
  }
} else {
  cat("Invalid choice. Please run the script again and select 1 or 2. \n")
}
