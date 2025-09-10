compress_string_loop <- function(input_str) {
  if (nchar(input_str) == 0) {
    return("")
  }
  if (nchar(input_str) == 1) {
    return(paste0(input_str, "1"))
  }
  
  chars <- strsplit(input_str, "")[[1]]
  
  compressed_str <- ""
  count <- 1
  
  for (i in 1:(length(chars) - 1)) {
    if (chars[i] == chars[i+1]) {
      count <- count + 1
    } else {
      compressed_str <- paste0(compressed_str, chars[i], count)
      count <- 1
    }
  }
  
  compressed_str <- paste0(compressed_str, chars[length(chars)], count)  
  return(compressed_str)
}

user_input <- readline(prompt = "Enter a string to compress: ")
compressed_output <- compress_string_loop(user_input)
cat("Original string:   ", user_input, "\n")
cat("Compressed string: ", compressed_output, "\n")
