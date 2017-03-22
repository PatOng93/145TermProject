###################################
######## GENERIC FUNCTIONS ########
###################################

push <- function(struct, xin) {
	UseMethod('push')
}

pop <- function(struct, name) {
	UseMethod('pop')
}

###################################
############# BINTREE #############
###################################

#' Create a new Binary Tree data structure
#' 
#' @param none
#' @return An S3 class of type "bintree", with a member matrix initialized to one row of NAs
#' @examples 
#' mytree <- newbintree()
newbintree <- function() {
  mat <- matrix(c(NA,NA,NA),nrow=1,ncol=3)
  tree <- list(data = mat)
  class(tree) <- "bintree"
  return(tree)
}

#' Prints a binary tree in sorted order, with elements separated by commas
#' 
#' @param treein A bintree class object
#' @return none
#' @examples 
#' print(mytree)
print.bintree <- function(treein) {
  if (length(treein$data) != 0 && !is.na(treein$data[1,1])) {
    recursive_print(treein, 1)
  }
}

recursive_print <- function(treein, i){
  if(!is.na(treein$data[i,2])){
    recursive_print(treein, treein$data[i,2])
  }
  cat(toString(treeein$data[i,1]))
  cat(", ")
  if(!is.na(treein$data[i,3])){
    recursive_print(treein, treein$data[i,3])
  }  
}

#' Return and remove the smallest element in the binary tree
#' 
#' @param treein A bintree class object
#' @param name The string object equivalent to the name of the treein parameter
#' @return rtrn The smallest element in treein
#' @examples 
#' element <- pop(mytree, "mytree")
pop.bintree <- function(treein, name) {
 	#Starting at Root, Continue accessing left child until it is NA (and therefore the lowest value)
  previ <- 0
  i <- 1
  while(!is.na(treein$data[i,2])){
     previ <- i
     i=treein$data[i,2]
  }
  #Set the parent node's left child to NA
  treein$data[previ,2]<-NA
  #Get the value of the lowest element:
  rtrn = treein$data[i,1]
  #"Remove" the child and update the original tree:
  treein$data[i,]<-c(NA,NA,NA) #Lazy delete
 	assign(name, treein,parent.frame())
 	return(rtrn)
}

#' Insert an element into the binary tree
#' 
#' @param treein A bintree class object
#' @param xin The element to add to the tree
#' @return treein The modified treein object with the new element inserted 
#' @examples 
#' mytree <- push(mytree, 10)
push.bintree <- function(treein, xin) {
  if (is.na(xin)) {
    stop("Elements of a binary tree may not be NA")
  }
  #Check if root is initialized
  if(is.na(treein$data[1,1])){
    treein$data[1,1] <- xin
    return(treein)
  } else{#call recursive insert function
    treein <- tree_insert(treein, xin, 1)
    return (treein)
  }  
}
#Recursive insert function
tree_insert <- function(treein, xin, i){
  if(treein$data[i,1] > xin){ #Check if left child
    if(!is.na(treein$data[i,2])){ #If not na, recurse on child
      tree_insert(treein,xin, treein$data[i,2])
    }else{#No child, insert new row and set to inserted value
      if(anyNA(treein$data[,1])){#Check if there are any lazy-deleted rows in matrix
        rownum_to_replace <- which(is.na(treein$data[,1]))[1]#Find first lazy-deleted row
        treein$data[rownum_to_replace,] <- c(xin,NA,NA)#Assign new value to row
        treein$data[i,2]<-rownum_to_replace #Updated parent node to point to new row
        return(treein)
      }else{
        treein$data <- rbind(treein$data,c(xin,NA,NA))#Add new row
        treein$data[i,2] <- dim(treein$data)[1]#Update parent to point to last row (which is the one just created)
        return(treein)
      }    
    }
  } else{#Right child
    if(!is.na(treein$data[i,3])){#If not na, recurse on child
      tree_insert(treein,xin,treein$data[i,3])
    }else{#No child, insert new row and set to inserted value
      if(anyNA(treein$data[,1])){#Check if there are any lazy-deleted rows in matrix
        rownum_to_replace <- which(is.na(treein$data[,1]))[1]#Find first lazy-deleted row
        treein$data[rownum_to_replace,] <- c(xin,NA,NA) #Assign new value to row
        treein$data[i,3]<-rownum_to_replace #Updated parent node to point to new row
        return(treein)
      }else{#Add new row to matrix
        treein$data <- rbind(treein$data,c(xin,NA,NA))#Add new row
        treein$data[i,3] <- dim(treein$data)[1]#Update parent to point to last row (which is the one just created)
        return(treein)
      }
    }
  }
}

###################################
############## STACK ##############
###################################

newstack <- function() {
	rtn <- list(data=c(NA))
	class(rtn) <- "stack"
	return (rtn)
}

print.stack <- function(stackin) {
  if (!is.na(stackin$data[1])) {
    for (i in 1:length(stackin$data)) {
      cat(toString(stackin$data[i]))
      cat(", ")
    }
	  cat("\n")
  }
}

pop.stack <- function(stackin, name) {
	rtn <- stackin$data[1]
	stackin$data <- stackin$data[-1]
  if (length(stackin$data) == 0) {
    stackin$data[1] <- NA
  }
	assign(name, stackin, parent.frame())
	return (rtn)
}

push.stack <- function(stackin, xin) {
	if (is.na(xin)) {
		stop("Elements of a stack may not be NA")
	}

  if (is.na(stackin$data[1])) {
    stackin$data[1] <- xin
  }
  else {
    stackin$data <- c(xin, stackin$data)
  }
  
	return (stackin)
}

###################################
############## QUEUE ##############
###################################

newqueue <- function() {

	rtn <- list(data=c(NA))
	class(rtn) <- "queue"
	return (rtn)
}

print.queue <- function(qin) {
  if (!is.na(qin$data[1])) {
    for (i in 1:length(qin$data)) {
      cat(toString(qin$data[i]))
      cat(", ")
    }
    cat("\n")
  }
}

pop.queue <- function(qin, name) {
	rtn <- qin$data[1]
	qin$data <- qin$data[-1]
  if (length(qin$data) == 0) {
    qin$data[1] <- NA
  }
	assign(name, qin, parent.frame())
	return (rtn)
}

push.queue <- function(qin, xin) {
	if (is.na(xin)) {
		stop("Elements of a queue may not be NA")
	}

  if (is.na(qin$data[1])) {
    qin$data[1] <- xin
  }
  else {
    qin$data <- c(qin$data, xin)  
  }
	
	return (qin)
}


