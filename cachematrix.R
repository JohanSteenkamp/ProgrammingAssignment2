## Two functions makeCacheMatrix and cacheSolve 
## makeCacheMatrix creates a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    invrs <- NULL
    set <- function(y) {
      x <<- y
      invrs <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invrs <<- inverse
    getinverse <- function() invrs
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve is a function that calculates the inverse of the matrix created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invrs <= x$getinverse()
    if(!is.null(invrs)) {
      message("Getting cached data...")
      return(invrs)
    }
    data <- x$get()
    invrs <- solve(data, ...)
    x$setinverse(invrs)
    invrs
}
