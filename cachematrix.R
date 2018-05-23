## The makeCacheMatrix creates a special matrix object and can cache inverse of the matrix. 
## The cacheSolve matrix will return inverse matrix from cache if it is available. If the inverse is not in cache, then it calculates the inverse, sets the cache and returns the inverse.

## This function takes in a matrix as an argument and can cache inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set <- function(y) {
                x <<- y
                m <<- NULL
        }
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m<-x$getinverse()
	if(!is.null(m)) {
		print("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
