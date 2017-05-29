## Matrix inversion is usually a costly computation and there maybe some benefit to caching the inverse
## of a matrix rather than compute it repeatedly.

## The main idea for the function below is to make a matrix, which is able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   	m <- NULL
  	set <- function(y){
    x <<- y
    m <<- NULL
  	}
  	get <- function() x
  	setInverse <- function(inverse) m<<- inverse
 	getInverse <- function() m
 	list(set = set, get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
 		if(!is.null(m)){
  				message("getting cached data")
  				return(m)
 		}
 		data <- x$get()
		 m <- solve(data, ...)
 		x$setInverse(m)
 		m
}
