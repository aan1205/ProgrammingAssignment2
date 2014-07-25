## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will return a list with the setters and getters functions of
## a matrix and it inverse. 

makeCacheMatrix <- function(x = matrix()) {
	xinverse<-NULL
	set<-function(y) {
		x <<- y
		xinverse <<- NULL
	}
	get<-function() {
		x
	}
	setinverse<-function(inverse = matrix()) {
		xinverse<<-inverse
	}
	getinverse<-function() {
		xinverse
	}
	list(set=set,
		get=get,
		setinverse=setinverse,
		getinverse=getinverse)
}


## Write a short comment describing this function
## This function will return the inverse of matrix x, but it don't calculate it if 
## the inverse is allready calculated and stored in the cache of special matrix
## 'makeCacheMatrix(x)'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	if (!is.null(x$getinverse())) {
		message('Getting data from Cache')
		xinverse<-x$getinverse()
	} else {
		info<-x$get()
		message('Calculating the inverse...')
		xinverse<-solve(info, ...)
		message('Storing in Cache...')
		x$setinverse(xinverse)
		message('DONE!!')
	} 
	xinverse
}
