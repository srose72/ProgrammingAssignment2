## These two functions create and store the inverse of a matrix, then
## returns the stored inverse if the input matrix hasn't changed.


## Creates matrix object that caches its inverse
makeCacheMatrix <- function(x = matrix()){
     inv <- NULL
     set <- function(y){
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) inv <<- solve
     getinverse <- function() inv
     list(set = set, get = get, setinverse = setinverse,
          getinverse = getinverse)
}

## Computes the inverse of the makeCacheMatrix returned object. Returns
## computation or cached inverse if already been calculated
cacheSolve <- function(x, ...){
     inv <- x$getinverse()
     if(!is.null(inv)){
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinverse(inv)
     return(inv)
     
}