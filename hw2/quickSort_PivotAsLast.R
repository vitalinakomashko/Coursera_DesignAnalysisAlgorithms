##Quick sort algorith. Input - a vector with numbers
##Doesn't deal with duplicates just yet. 
##This version uses the last element as an input

qsPivotLast = function(input){
  #cat("here is my input")
  #print(input)
  ##Where to write the output and the number of comparisons according to the definiton in the hw description:
  result=vector("integer",length=length(input))
  compare=0
  
  ##The base case when the vector size is 1
  if(length(input)<=1){
    return(list(input,compare))
  }
  
  
  #cat("Here is the first element")
  #print(input[1])
  #cat("here is the last element")
  #print(input[length(input)])
  #Define the pivot:
  tempFirst = input[1]
  input[1] = input[length(input)]
  input[length(input)] = tempFirst
  pivot =  input[1]
  #cat("here is the first element after switching")
  #print(input[1])
  #cat("here is the last element after switching")
  #print(input[length(input)])
  #cat("Here is the array")
  #print(input)
  
  compare = length(input)-1
  
  ##The partitioning routine:
  i=2
  for (j in 2:length(input)){
    if(input[j] < pivot){
      temp = input[i]
      input[i] = input[j]
      input[j] = temp
      i=i+1
    }
  }
  tempNew = pivot
  input[1] = input[i-1]
  input[i-1] = tempNew

  #cat("right side")
  #print(input[0:(i-2)])
  #cat("left side")
  #print(input[-(1:(i-1))])
  resultLeft = qsPivotLast(input[0:(i-2)])
  resultRight = qsPivotLast(input[-(1:(i-1))])

  result = c(resultLeft[[1]],pivot,resultRight[[1]])
  compare = compare+resultLeft[[2]]+resultRight[[2]]
  
  return(list(result,compare))
}