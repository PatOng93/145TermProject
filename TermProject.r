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

newbintree <- function() {
  mat <- matrix(c(NA,NA,NA),nrow=1,ncol=3)
  tree <- list(data = mat)
  class(tree) <- "bintree"
  return(tree)
}

print.bintree <- function(treein) {
  if (length(bintree$data != 0)) {
    print(treein$data)
  }
}

pop.bintree <- function(treein, name) {
 	#Starting at Root, Continue accessing left child until it is NA (and therefore the lowest value)
  previ <- 0
  i <- 1
  while(!is.na(treein$data[i,2])){
     previ <- i
     i=treein$data[i,2]
   }
  #Get the value of the lowest element:
  treein$data[previ,2]<-NA
  rtrn = treein$data[i,1]
  treein$data[i,]<-c(NA,NA,NA) #Lazy delete 
  #Remove the child and update the original tree:
 	#treein$data = treein$data[-i,,drop=F]
 	assign(name, treein,parent.frame())
 	return(rtrn)
}

push.bintree <- function(treein, xin) {
  if (is.na(xin)) {
    stop("Elements of a binary tree may not be NA")
  }
  #Check if root is initialized
  if(is.na(treein$data[1,1])){
    treein$data[1,1] <- xin
    return(treein)
  } else{#call recursive insert function
    treein <- tree_insert(treein, xin, name, 1)
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
	rtn <- list(data=numeric(0))
	class(rtn) <- "stack"
	return (rtn)
}

print.stack <- function(stackin) {
  if (length(stackin$data) != 0) {
	  print(stackin$data)
  }
}

pop.stack <- function(stackin, name) {
	rtn <- stackin$data[1]
	stackin$data <- stackin$data[-1]
	assign(name, stackin, parent.frame())
	return (rtn)
}

push.stack <- function(stackin, xin) {
	if (is.na(xin)) {
		stop("Elements of a stack may not be NA")
	}

	stackin$data <- c(xin, stackin$data)
	return (stackin)
}

###################################
############## QUEUE ##############
###################################

newqueue <- function() {
	rtn <- list(data=numeric(0))
	class(rtn) <- "queue"
	return (rtn)
}

print.queue <- function(qin) {
  if (length(qin$data) != 0) {
    print(qin$data)
  }
}

pop.queue <- function(qin, name) {

	rtn <- qin$data[1]
	qin$data <- qin$data[-1]
	assign(name, qin, parent.frame())
	return (rtn)
}

push.queue <- function(qin, xin) {

	if (is.na(xin)) {
		stop("Elements of a queue may not be NA")
	}

	qin$data <- c(qin$data, xin)
	return (qin)

}


