# it feels weird as shit to pass the 
# data structure into a member function

# constructors?
# each class in its own file?



push <- function(struct, xin, name) {
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
  print(treein$data)
  
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
 	assign(name, treein,.GlobalEnv)
 	return(rtrn)
}

push.bintree <- function(treein, xin, name) {
  if (is.na(xin)) {
    stop("Elements of a stack may not be NA")
  }
  #Check if root is initialized
  if(is.na(treein$data[1,1])){
    treein$data[1,1] <- xin
    assign(name,treein,.GlobalEnv)
  } else{#call recursive insert function
    treein <- tree_insert(treein, xin, name, 1)
    assign(name,treein,.GlobalEnv)
  }  
}
#Recursive insert function
tree_insert <- function(treein, xin, name, i){
  if(treein$data[i,1] > xin){ #Check if left child
    if(!is.na(treein$data[i,2])){ #If not na, recurse on child
      tree_insert(treein,xin,name,treein$data[i,2])
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
      tree_insert(treein,xin,name,treein$data[i,3])
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

## ***Testing Code***
mytree <- newbintree()
push(mytree,5,"mytree")
push(mytree,4,"mytree")
push(mytree,6,"mytree")
push(mytree,6,"mytree")
push(mytree,7,"mytree")
push(mytree,8,"mytree")

print(mytree$data)
pop(mytree, "mytree")
print(mytree$data)
push(mytree,3,"mytree")
print(mytree$data)

element <- pop(mytree, "mytree")
#print(element)
#print(attributes(mytree))


###################################
############## STACK ##############
###################################

newstack <- function() {
	rtn <- list(NA)
	class(rtn) <- "stack"
	return (rtn)
}

print.stack <- function(stackin) {
	if (stackin[1] != NA)
	{
		print(paste(stackin, collapse = ', '))
	}
}

pop.stack <- function(stackin) {
	rtn <- stackin[1]
	stackin[1] <- NULL
	return (rtn)
	# does this update stackin? would we need to return that as well?
}

push.stack <- function(stackin, xin) {
	if (xin == NA) {
		stop("Elements of a stack may not be NA")
	}

	rtn <- list(xin)
	if (stackin[1] != NA) {
		l <- length(stackin)
		for (i in 1:l) {
			rtn[i+1] <- stackin[i]
		}
		# probably exists a more efficient way besides a loop
	}
	return (rtn)
}

###################################
############## QUEUE ##############
###################################

newqueue <- function() {
	rtn <- list(NA)
	class(rtn) <- "queue"
	return (rtn)
}

print.queue <- function(qin) {
	if (qin[1] != NA)
	{
		print(paste(qin, collapse = ', '))
	}
}

pop.queue <- function(qin) {
	rtn <- qin[1]
	qin[1] <- NULL
	return (rtn)
	# does this update qin? would we need to return that as well?
}

push.queue <- function(qin, xin) {
	if (xin == NA) {
		stop("Elements of a queue may not be NA")
	}

	if (qin[1] == NA) {
		qin[1] <- xin
	}
	else {
		nxt <- length(qin) + 1
		qin[nxt] <- xin
	}
	
	return (qin)
}

