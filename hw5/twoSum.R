#This code was written for the 5th programming assignement in the algorithms class at Stanford,
#Spring 2012
#Written by Vitalina Komashko, PhD
#Date last modified: April, 17, 2012



twoSum = function(inputVector, targetSum){
  require(hash)
  inputKeys = as.character(inputVector)
  h = hash(keys = inputKeys,values = 1)
  #output = list()
  output = sapply(keys(h),function(x) pairFinder(x,h,targetSum))
#   for (i in keys(h)){
#     nemo = targetSum-as.integer(i)
#     if(has.key(as.character(nemo),h)==TRUE){
#       targets = c(i, as.character(nemo))
#       output[[length(output)+1]] = targets
#       #cat("The target sum is found","\n")
#       #print(paste("the first number is",i,sep=" "))
#       #print(paste("the second number is",as.character(nemo),sep=" "))
#     }
#     #else{
#     #  print(paste("the target sum hasn't been found",as.character(targetSum),sep=" "))
#     #}
#   }
  if(length(which(output=="yes"))!=0){
    cat("Target has been located!","\n")
    #return(output) 
  }
  else{
    cat("Target has not been located! you are fired","\n")
  }
  clear(h)
  rm(h)
}

pairFinder = function(keyInQuestion,hashTable,sumToFind){
  lostPair = sumToFind - as.integer(keyInQuestion)
  if(has.key(as.character(lostPair),hashTable)==TRUE){
    return("yes")
  }
  
  
}
