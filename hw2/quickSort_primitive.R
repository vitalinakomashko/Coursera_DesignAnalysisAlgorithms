##Quick sort algorith. Input - a vector with numbers
##Doesn't deal with duplicates just yet. 
##This version selects first element as a pivot

quicksort = function(input){
  ##Where to write the output and the number of comparisons according to the definiton in the hw description:
  result=vector("integer",length=length(input))
  compare=0
  
  ##The base case when the vector size is 1
  if (length(input)<=1){
    return(list(input,compare))
  }
  
  #Define the pivot:
  pivot=input[1]
  #cat("here is my pivot")
  #print(pivot)
  compare=length(input)-1
  
  ##The partitioning routine:
  i=2
  for (j in 2:length(input)){
    if(input[j]<pivot){
      temp=input[i]
      input[i]=input[j]
      input[j]=temp
      i=i+1
    }
  }
  tempNew=pivot
  input[1]=input[i-1]
  input[i-1]=tempNew

  #cat("here is the left side")
  #print(input[1:(i-2)])
  #cat("here is the right side")
  #print(input[-(1:(i-1))])
  resultLeft=quicksort(input[0:(i-2)])
  resultRight=quicksort(input[-(1:(i-1))])
  #resultRight=quicksort(input[i:length(input)])
  
  result = c(resultLeft[[1]],pivot,resultRight[[1]])
  compare=compare+resultLeft[[2]]+resultRight[[2]]
  
  return(list(result,compare))
}