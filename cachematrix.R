## The first function, makeCacheMatrix creates a list containing functions to
## 1. set the matrix variable to be stored persistently in the calling environment
## 2. get the matrix variable contents
## set the cached inverse of the matrix
## get the cached inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inv_inp) inv <<- inv_inp
    getInverse <- function() inv
    list(set=set, get=get, setInverse=setInverse, getInverse = getInverse)
}

## The following function calculates the inverse of the matrix created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of the input matrix and 
## sets the value of the inverse in the cache via the 'setInverse' function.
cacheSolve <- function(x, ...) {
	usage<-"input must be a 'list' created using the makeCacheMatrix function"
	if (missing(x)) {
		message(usage)
		return(NULL)
	} else if (!exists('x')) {
		message(usage)
		return(NULL)
	} else if (!"getInverse" %in% names(x)) {
		message(usage)
		return(NULL)
	}
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    ## check if inverse has already been computed & assigned
    if(!is.null(inv)) {
    	## if yes, return cached inverse matrix
        message("getting cached inverse matrix solution")
        return(inv)
    }
    ## if no, compute inverse matrix & store result in persistent variable
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
## EOF