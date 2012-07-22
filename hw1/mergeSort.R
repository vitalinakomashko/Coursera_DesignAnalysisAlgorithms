##This is inversion algorithm based on the merge sort algorithm.

mergeSort = function(input){
  
  inversions=0 #number of inversion
  result=vector("integer",length=length(input))
  if (length(input)==1){
    return(list(input,inversions))
  }
  #Recursive calls for the two sides of the input vector
  dataCatcherL = mergeSort(input[1:(length(input)/2)])
  dataCatcherR = mergeSort(input[-(1:(length(input)/2))])
  #The output of the merge sort count is a list that give the sorted array and the number of inversions
  #output for the left part of the array
  left = dataCatcherL[[1]]
  inversions=inversions+dataCatcherL[[2]]
  #Output for the right part of the array
  right = dataCatcherR[[1]]
  inversions = inversions+dataCatcherR[[2]]
  #Intitialize the counters for the each subarray
  i=1
  j=1
  for(k in 1:length(input)){
    #Keep track of where we are in each subarray
    pointL = left[i]
    pointR = right[j]
    #Make sure that we are not out of the boundaries of the array because otherwise we will be comparing an
    #NA and some number
    if(i>length(left)){
      result[k] = pointR
      j=j+1
    }
    #The difference between the if/else and else if statements. If I have only a bunch of if statements (or
    #one) and then else the program will check with every single one of them. If I have if/else if then the
    #program will check with the first one and if this one is true it won't even touch the other ones. If it
    # is false then it will check more of them until it finds one that is true and then will discard the rest
    #of them. This is good when in some cases your have if statements that may break the program like in my
    #situation here.If i is more than length of i you don't want to do any other comparisons between pointL and
    #pointR.
    else if(j>length(right)){
      result[k] = pointL
      i=i+1
    }
    else if(pointL < pointR){
      result[k] = pointL
      i=i+1
    }
    else if(pointL > pointR) {
      result[k] = pointR
      j=j+1
      #Count the number of inversions in this particular recursive call. 
      inversions=inversions+length(left)-i+1
    }
    else{
      cat("What are you doing here?!","\n")
    }
  }
  return(list(result,inversions))
}