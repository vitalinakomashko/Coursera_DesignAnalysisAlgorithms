###Implementation of Quicksort algorithm

###By Vitalina Komashko, PhD
###April, 3, 2012



###Choices of the pivot:
###1 - always first element
###2 - always last
###3 - median

####The algorithm returns sorted input and also the number of comparisons it had to make (definition of the 
###it is according to the Stanford Online Coursera class "Design and analysis of algorithm I" with Tim
###Roughgarden)
quicksort = function(input, pivotChoice){
  ##Where to write the output and the number of comparisons according to the definiton in the hw description:
  result=vector("integer",length=length(input))
  compare=0
  
  ##The base case when the vector size is 1
  if (length(input)<=1){
    return(list(input,compare))
  }
  
  if (pivotChoice==1){
    pivot = input[1]
  }
  else{
    if (pivotChoice == 2){
      tempFirst = input[1]
      input[1] = input[length(input)]
      input[length(input)] = tempFirst
      pivot =  input[1]
    }
    else{
      if (pivotChoice == 3){
        firstElement = input[1]
        lastElement = input[length(input)]
        medianElement = input[ceiling(length(input)/2)]
        potentialPivots = c(firstElement,lastElement,medianElement)
        medianOfThree = median(potentialPivots)
        indexMedian = which(input == medianOfThree)
        if(indexMedian != 1){
          tempMedian = input[1]
          input[1] = medianOfThree
          input[indexMedian] = tempMedian
        }
        pivot = input[1]
      }
      else{
        stop("please provide a valid choice for the pivot: 1 (always first), 2 (always last) or 3 (always median)")
      }
    }
  }
   
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
  
  resultLeft=quicksort(input[0:(i-2)], pivotChoice)
  resultRight=quicksort(input[-(1:(i-1))], pivotChoice)
  
  result = c(resultLeft[[1]],pivot,resultRight[[1]])
  compare=compare+resultLeft[[2]]+resultRight[[2]]
  
  return(list(result,compare))
}