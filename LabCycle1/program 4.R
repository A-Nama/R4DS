generate_password<-function(length){
  upper<-LETTERS
  lower<-LETTERS
  digits<-as.character(0:9)
  special<-strsplit("!@#$%^&*()_+-=[]{};:,.<>?", "")[[1]]
  
  all_chars<-c(upper, lower, digits, special)
  
  if(length<4){
    stop("Password length must be at least 4 and include uppercase, lowercase, numerical and special character.")
  }
  
  password<-c(
    sample(upper,1),
    sample(lower,1),
    sample(digits,1),
    sample(special,1),
    sample(all_chars, length-4, replace=TRUE)
  )
  
  password<-sample(password)
  
  paste(password,collapse = "")
}

len<-as.integer(readline(prompt = "Enter required length of password: "))

if(is.na(len) || len<4){
  cat("Invalid length. Must be a number.")
}else {
  pwd<-generate_password(len)
  cat("Generated password: ", pwd, "\n")
}
