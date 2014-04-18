## These two functions are for calculating the inverse of a matrix and 
## caching it for later use

## This function creates 4 functions and returns them in a list 
## Functions are accessed by assigning the returned list to a variable
## eg variable <- makeCacheMatrix(myMatrix)
## variable$set() will then run the set function etc
## see comments within function for explanations of each one

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        # set is used to set (or reset) the matrix to be used
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        # get returns the matrix currently being used
        get <- function() x
        # setinv is for assigning a result to the cache
        setinv <- function(result) i <<- result
        # getinv returns the cached result (if it exists)
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function uses the functions from the list of functions 
## created in makeCacheMatrix to actually solve the inverse of the matrix
## or retreive it from the cache if it has been cached

cacheSolve <- function(mcm, ...) {
	# Check if there is a cached result, return it if there is
        inv <- mcm$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # Get the matrix and solve the inverse
        data <- mcm$get()
        inv <- solve(data, ...)
        # Store the result in the cache
        mcm$setinv(inv)
        inv

}

## Test function for checking the inverse and the behaviour of makeCacheMatrix

inverseCheck <- function(mcm, ...) {
	# multiply the matrices together to see if we really have an inverse
	inv <- mcm$getinv()
	mat <- mcm$get()
	if(is.null(inv)) {
		message("There's no cache yet, calculating inverse")
    	inv <- solve(mat, ...)
    	mcm$setinv(inv)
    }
    res <- inv %*% mat
    res
}
