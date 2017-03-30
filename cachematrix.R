## The functions below are used to cache the inverse of a matrix. 
## Instead of calculating it several times

## The function makeCacheMatrix receives a matrix 
## The process is responsible for generating the following functions:
## Set: set the value of the matrix
## Get: get the value of the matrix
## SetInverse: set the value of inverse of the matrix
## GetInvese: get the value of inverse of the matrix

makeCacheMatrix <- function(m = matrix()) {
     i <- NULL
     set <- function(y) {
          m <<- y
          i <<- NULL
     }
     get <- function() m
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function() i
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## The function cacheSolve receives an makeCacheMatrix as a parameter, 
## if the function getinverse returns null, 
## the solve calculation is performed, 
## returning the value for the setinverse function of the makeCacheMatrix function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
     
     i <- x$getinverse()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     s <- solve(data)
     x$setinverse(s)     
}
