cat("Enter a paragraph: \n")
paragraph<-tolower(readLines(con=stdin(), n=1))

words<-unlist(strsplit(paragraph, "\\W+"))
words <-words[words != ""]

word_count<-length(words)
cat("Total number of words: ", word_count, "\n")

word_lengths<-nchar(words)
avg_word_length<-mean(word_lengths)
cat("Average word length:", round(avg_word_length, 2), "\n")

longest_word<-words[which.max(word_lengths)]
cat("Longest word: ", longest_word, "\n")

cat("Enter the word to be replaced: \n")
old_word<-tolower(readLines(con=stdin(), n = 1))

cat("Enter the replacement word: \n")
new_word<-readLines(con=stdin(), n = 1)

modified_paragraph<-gsub(paste0("\\b", old_word, "\\b"), new_word, paragraph)

cat("Modified paragraph: \n",modified_paragraph, "\n")
