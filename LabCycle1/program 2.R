caeser_cipher<- function(text, shift){
  result<- ""
  for (i in seq_along(strsplit(text, "")[[1]])){
    char<- substr(text, i, i)
    
    if(grepl("[A-Z]", char)){
      base<- utf8ToInt("A")
      new_char<- intToUtf8((utf8ToInt(char) - base + shift) %% 26 + base)
    } else if(grepl("[a-z]", char)){
      base<- utf8ToInt("a")
      new_char<- intToUtf8((utf8ToInt(char) - base + shift) %% 26 + base)
    } else{
      new_char <- char
    }
    
    result<- paste0(result, new_char)
  }
  return(result)
}

cat("Enter a string to encyrpt:\n")
sentence <- readLines(con = stdin(), n = 1)

cat("Enter the shift value:\n")
shift <- as.integer(readLines(con = stdin(), n = 1))

encrypted <- caeser_cipher(sentence, shift)
cat("Encrypted sentence:\n", encrypted,"\n")
