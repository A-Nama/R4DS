text_input <- readline(prompt = "Enter a text: ")

is_palindrome <- function(text){
  clean_text <- tolower(gsub("[^a-zA-Z0-9]", "", text))
  
  reversed_text <- paste(rev(strsplit(clean_text, NULL)[[1]]), collapse = "")
  
  return(clean_text == reversed_text)
}

if(is_palindrome(text_input)){
  cat("The text is a palindrome. \n")
} else {
  cat("The text is not a palindrome. \n")
}
