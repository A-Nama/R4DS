reverse_list <- function(lst){
  if(length(lst) <= 1){
    return(lst)
  } else {
    return(c(reverse_list(lst[-1]), lst[1]))
  }
}

original_list <- list("a", "b", "c", "d")
reversed_list <- reverse_list(original_list)

print("Original list: ")
print(original_list)
print("Reversed list: ")
print(reversed_list)
