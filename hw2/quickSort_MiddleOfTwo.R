##Quick sort algorith. Input - a vector with numbers
##Doesn't deal with duplicates just yet. 
##This version selects the middle element as a pivot always

qsMiddlePivot = function(input){
  ##Where to write the output and the number of comparisons according to the definiton in the hw description:
  result=vector("integer",length=length(input))
  compare=0
  
  ##The base case when the vector size is 1
  if (length(input) <= 1){
    return(list(input,compare))
  }
  
  #Define the pivot as the median of three
  firstElement = input[1]
  
 
  lastElement = input[length(input)]
  
 
  medianElement = input[ceiling(length(input)/2)]
  
  potentialPivots = c(firstElement,lastElement,medianElement)
  medianOfThree = median(potentialPivots)
  cat("Median of three")
 
  indexMedian = which(input == medianOfThree)
  if(indexMedian != 1){
    tempMedian = input[1]
    input[1] = medianOfThree
    input[indexMedian] = tempMedian
  }
  pivot = input[1]
  
  #Start counting the comparisons:
  compare = length(input)-1
  
  ##The partitioning routine:
  i=2
  for (j in 2:length(input)){
    if(input[j]<pivot){
      temp = input[i]
      input[i] = input[j]
      input[j] = temp
      i = i+1
    }
  }
  tempNew = pivot
  input[1] = input[i-1]
  input[i-1] = tempNew

  resultLeft = qsMiddlePivot(input[0:(i-2)])
  resultRight = qsMiddlePivot(input[-(1:(i-1))])

  result = c(resultLeft[[1]],pivot,resultRight[[1]])
  compare = compare+resultLeft[[2]]+resultRight[[2]]
  
  return(list(result,compare))
}