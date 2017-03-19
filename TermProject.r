# it feels weird as shit to pass the 
# data structure into a member function

# constructors?
# each class in its own file?

push <- function(struct, xin, name) {
	#print(attributes(struct))
	UseMethod('push', struct)
}

pop <- function(struct, name) {
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
	rtn <- list(items=numeric(0))
	class(rtn) <- "stack"
	return (rtn)
}

print.stack <- function(stackin) {

	print(stackin$items)

	# if (length(stackin$items) > 0)
	# {
	# 	print(paste(stackin$items, collapse = ', '))
	# }
}

pop.stack <- function(stackin, name) {
	rtn <- stackin$items[1]

	newlist = numeric(0)
	if (length(stackin$items) >= 2) {
		l <- length(stackin$items)
		for (i in 2:l) {
			newlist <- c(newlist, stackin$items[i])
		}
		# probably exists a more efficient way besides a loop
	}

	stackin$items <- newlist
	assign(name, stackin, .GlobalEnv)

	return (rtn)
}

push.stack <- function(stackin, xin, name) {
	if (is.na(xin)) {
		stop("Elements of a stack may not be NA")
	}
	rtn <- xin


	if (length(stackin$items) > 0) {
		l <- length(stackin$items)
		for (i in 1:l) {
			rtn <- c(rtn, stackin$items[i])
		}
		# probably exists a more efficient way besides a loop
	}

	stackin$items <- rtn
	assign(name, stackin, .GlobalEnv)
}

###################################
############## QUEUE ##############
###################################

newqueue <- function() {
	rtn <- list(items = c(NA))
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

