###This function takes a file as input, converts it into an adjacency list and
###finds the minimum cut in a graph 


mincut = function(input){
  for(i in 1:(length(input)-2)){
    #set.seed(i)
    #select a random list element
    randListElement = sample(length(input),1)
    #select random edge
    if(length(input[[randListElement]][-1])==1){
      randNode=input[[randListElement]][-1]
    }
    else{
      randNode = sample(input[[randListElement]][-1],1)
    }
    cat("Here is the random node")
    print(randNode)
    #Now collapse the selected edge of the graph and remove the node
    input[[randListElement]] = input[[randListElement]][-which(input[[randListElement]]==randNode)]
    #Save the node which will be substituted for the random node in the whole graph
    nodeToBeDeleted = input[[randListElement]][1]
    cat("Here is the node to be deleted")
    print(nodeToBeDeleted)
    #Now assign the selected random node to the first element of the vector
    input[[randListElement]][1] = randNode
    selfLoop = NULL
    noPointingNode = NULL
    for(j in 1:length(input)){
      #Do we have a node which is the node to be deleted?
      cat("element j","\n")
      print(j)
      print(length(input[[j]]))
      if(length(input[[j]])==1){
        noPointingNode=c(noPointingNode,j)
        cat("the no pointing node","\n")
        print(noPointingNode)
      }
      else{
        if(length(which(input[[j]]==nodeToBeDeleted))==1){
          input[[j]][which(input[[j]]==nodeToBeDeleted)]=randNode
        }
      }
      if(length(which(input[[j]][1]==input[[j]][-1]))==1 & length(input[[j]])>2){
        input[[j]]=input[[j]][-which(input[[j]][1]==input[[j]][-1])]
      }
      if(length(input[[j]])==2 & input[[j]][1]==input[[j]][2]){
        selfLoop=c(selfLoop,j)
      }
    }
    #Now remove all self loops from the input list
    cat("here is the input before deleting self loops")
    print(input)
    cat("here is the selfloop ingex","\n")
    print(selfLoop)
    listElementDel = union(selfLoop,noPointingNode)
    listElementDel = listElementDel[!is.null(listElementDel)]
    if(!is.null(listElementDel)){
      input=input[-listElementDel]
    }
    cat("no pointing node","\n")
    print(noPointingNode)
    cat("here is the input after deleting the self loops andnot point Nodes","\n")
    print(input)
  }
  return(length(input))
}