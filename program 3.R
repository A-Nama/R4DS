is_valid_age<-function(age){
  return(!is.na(age)&& age > 0 && age == as.integer(age))
}

is_valid_grade<-function(grade){
  return(toupper(grade) %in% c("A", "B", "C", "D", "F"))
}

valid_students<-list()

num_students<-as.integer(readline(prompt = "Enter the number of students: "))

for (i in 1:num_students){
  cat("\nStudent", i, "\n")
  
  name<-readline(prompt = "Enter name of student: ")
  age<-as.integer(readline(prompt = "Enter age of student: "))
  grade<-readline(prompt = "Enter grade of student: ")
  
  if(is_valid_age(age) && is_valid_grade(grade)){
    valid_students[[length(valid_students)+1]]<-list(name=name, age=age, grade=toupper(grade))
  } else{
    cat("invalid input. Record skipped. \n")
  }
}

cat("\nValid student records: \n")
for (student in valid_students){
  cat(paste0("Name: ", student$name, " Age: ", student$age, " Grade: ", student$grade, "\n"))
}

if(length(valid_students)>0){
  ages<-sapply(valid_students, function(x) x$age)
  avg_age<-mean(ages)
  cat("\n Average age of valid students is ", avg_age, "\n")
} else{
  cat("\n No valid student records to calculate average age. \n")
}
