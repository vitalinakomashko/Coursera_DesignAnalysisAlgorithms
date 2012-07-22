###This function takes a file as input, converts it into an adjacency list and
###finds the minimum cut in a graph 


mincut = function(input){
  
  #The function terminates when the length of the list is 2.
  
  while(length(input)>2){
    
    #select a random list element
    randListElement = sample(length(input),1)
    
    #select random node that will be part of the collapsed edge. Take into account a corner case with sample:
    if(length(input[[randListElement]][-1])==1){
      randNode=input[[randListElement]][-1]
      randNodeInd=2
    }
    else{
      randNodeInd = sample.int(length(input[[randListElement]][-1]),1)
      randNode = input[[randListElement]][randNodeInd+1]
    }
    #cat("Here is the random node","\n")
    #print(randNode)
    #cat("here is the index of the random node","\n")
    #print(randNodeInd)
    #readline()
    
    #Now remove the node
    input[[randListElement]] = input[[randListElement]][-(randNodeInd+1)]
    #cat("removing the node","\n")
    #print(input)
    #readline()
    
    #Save the node which will be substituted for the random node in the whole graph
    nodeToBeDeleted = input[[randListElement]][1]
    #cat("Here is the node to be deleted")
    #print(nodeToBeDeleted)
    
    #Now assign the selected random node to the first element of the vector
    input[[randListElement]][1] = randNode
    #cat("input before going to the checkup loop","\n")
    #print(input)
    #readline()
    
    #Now lets go through the whole list
    #We need to do the following things:
    #1. For each list element (integer vector) substitute nodeToBeDeleted for the randomNode
    #2. If we find that the left node maps to the same node on the right this mean a self loop, delete that
    #After we done with the loop we need to:
    #1. Delete self loops (2 element vectors that have the same node)
    #2. Delete single nodes (not pointing to anything)
    #3. Merge numeric vectors which have the same left element. 
    
    selfLoop = NULL #counter for the self loops that need to be deleted later
    noPointingNode = NULL #counter for the single nodes not pointing to anything
    
    #Begin the check-up loop through the array, this also has to be done until the array is not more than 2
    
    for(j in 1:length(input)){
      
      #Do we have a node which is the node to be deleted?
      #cat("element j","\n")
      #print(j)
      #print(length(input[[j]]))
      
      #Check if we have a single node, increase the counter for it:
      if(length(input[[j]])==1){
        noPointingNode=c(noPointingNode,j)
        #cat("the no pointing node","\n")
        #print(noPointingNode)
        #readline()
      }
      else{#Check if we have any nodes that we need to substitute for the selected random node:
        if(length(which(input[[j]]==nodeToBeDeleted))!=0){
          input[[j]][which(input[[j]]==nodeToBeDeleted)]=randNode
          }
      }
      
      #Now we need to check if we introduced a self loop by substituting for the random node
      if(length(which(input[[j]][1]==input[[j]][-1]))!=0 & length(input[[j]])>2){
        input[[j]]=input[[j]][-(which(input[[j]][1]==input[[j]][-1])+1)]
      }
      
      #Now check if we have self loops (numeric vectors that have 2 the same elements) and get the counter:
      if(length(input[[j]])==2 & input[[j]][1]==input[[j]][2]){
        selfLoop=c(selfLoop,j)
      }
    }
    
    #cat("here is the input before deleting self loops")
    #print(input)
    #cat("here is the selfloop ingex","\n")
    #print(selfLoop)
    #readline()
    
    #Now lets merge teh counter for self loops and for the node pointing nowhere and remove those list elements:
    listElementDel = c(selfLoop,noPointingNode)
    listElementDel = listElementDel[!is.null(listElementDel)]
    if(!is.null(listElementDel)){
      input=input[-listElementDel]
    }
    #cat("input after removing self loops and nodes","\n")
    #print(input)
    #readline()
    #Check for list elements that start from the same node and merge those together
    #Get the left elements of the input:
    leftElements = sapply(input,"[",1)
    #cat("first elements checkup","\n")
    #print(input)
    #readline()
    
    #Find if there is a duplicate and which one of them:
    if(length(which(duplicated(leftElements)))!=0){
      #What is the value of the element that is duplicated?
      dupIndex = which(duplicated(leftElements))
      dupValue = leftElements[dupIndex]
      nodesToMerge = which(leftElements==dupValue) # index of the list elements that will need to be merged
      #New values for the vectors
      newValues = unlist(sapply(input[nodesToMerge],"[",-1))
      if(length(which(dupValue==newValues))!=0){
        newValues = newValues[-which(dupValue==newValues)]
      }
      mergedListElement = c(dupValue,newValues)
      input=input[-nodesToMerge]
      input=c(input,list(mergedListElement))
      #cat("input after adding the new merge line","\n")
      #print(input)
      #readline()
    }

  }
  
  return(input)
}