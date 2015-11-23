## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly. In this assignment, the objective is to write a pair of 
## functions that cache the inverse of a matrix.

## LIMITATION: The function cacheSolve() used solve() R function which 
##             can only invese square matrices. Non-square matrices don't
##			   have inverses. 

## This function creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	} 
	get <- function() x
	
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	
	list(set = set,
	     get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## This function computes the inverse of the matrix created by 
## makeCacheMatrix function above. If the inverse is not NULL &
## the matrix has not changed (important), then this function
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
		if (!is.null(inv))
		{
			message("getting cached data")
			return(inv)
		}
		## solve() only works for square matrix
		inputMatrix <- x$get()
		inv <- solve(inputMatrix,...)
		x$setinverse(inv)
		inv
}		

