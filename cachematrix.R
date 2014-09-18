## This pair of functions cache the inverse of a matrix.
## The code is inspired by the original examples of the Assignement.

## The function 'makeCacheMatrix' creates a list, which contains function. 
## This list can be tought as a special 'matrix' object, which can cache the inverse of the matrix. The elements of the list are
## 1. set, which sets the value of the matrix.
## 2. get, which enables to get the current value of the matrix.
## 3. setinv, which sets the inverse of the matrix.
## 4. getinv, which gets the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	mat <- NULL
	set <- function(y) {
		x <<- y
		mat <<- NULL ## if the matrix changes, the inverse is set to null; it will have to be computed again
		}
	get <- function() x
	setinv <- function(inv) mat <<- inv
	getinv <- function() mat
	list(set = set, get = get, 
		setinv = setinv,
		getinv = getinv)


}


## The function 'cacheSolve' computes the inverse of the special "matrix" returned by 'makeCacheMatrix'. If the inverse has already been calculated, 'cacheSolve' retrive the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        ## If the inverse has already been computed, then x$getinv() will not be null, and will give the expected value.       
        if(!is.null(inv)){
        	message("Getting previously computed inverse") ## Message for tests purpose.
        	return(inv)
        }
        ## if the inverse has not been calculated before, then it is computed and cached. 
        data <- x$get() ## retrieve the actual value of the matrix. data is a matrix object.
        inv <- solve(data) ## compute the inverse of data.
        x$setinv(inv) ## cache the value of the inverse.
        inv ## return the inverse
}
