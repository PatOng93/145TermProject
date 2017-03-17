# it feels weird as shit to pass the 
# data structure into a member function

# constructors?
# each class in its own file?

push <- function(struct, xin) {
	UseMethod('push')
}

pop <- function(struct) {
	UseMethod('pop')
}

###################################
############# BINTREE #############
###################################

newbintree <- function() {

}

print.bintree <- function(treein) {

}

pop.bintree <- function(treein) {
	
}

push.bintree <- function(treein, xin) {

}

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

