## The following functions, makeCacheMatrix() and cacheSolve(),
## allow us to cache the inverse of a matrix.

## The function makeCacheMatrix() includes four nested functions,
## which provide the ability to store a matrix, to retrieve it,
## to store its inverse, and to retrieve that inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse = setinverse,
	getinverse = getinverse)
}

## This function returns the cached inverse matrix from 
## makeCacheMatrix() if such an inverse exists (i.e. is not NULL);
## otherwise, it calculates the inverse of the stored matrix, caches
## it in the environment of makeCacheMatrix(), and then returns it. 

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	matrix <- x$get()
	inv <- solve(matrix, ...)
	x$setinverse(inv)
	inv
}