## Functions to cache the inverse of a supplied Matrix
## Example:
## > x<-Matrix(1:4)
## > y<-makeCacheMatrix(x)
## > cacheSolve(y)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(y)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


## makeCacheMatrix(x = matrix())
## This function takes one paramter which must be a Matrix and
## stores that matrix locally in variable x
## the returned list provides four functions:
##      set(y) allows you to overwrite the stored matrix and resets the cached inverse to NULL
##      get() returns the stored matrix
##      setInverse(inverse) stores the passed in parameter into the local variable inv. This
##          function is used by cacheSolve and should not be called directly by the user
##      getInverse() returns the local variable inv that contains the inverse of x
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    
    getInverse <- function() {
        inv
    }
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve(x, ...)
## This function takes at least one paramter x which should be a list created by makeCacheMatrix
## the result of this function is the inverse of the Matrix that was passed into makeCacheMatrix.
## If the inverse has already been calculated, the cached version will be returned without re-calculation
## if the cached value is NULL (has not been calculated) then the inverse will be calculated
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
