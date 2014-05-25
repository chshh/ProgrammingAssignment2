## makeCacheMatrix creates a special "matrix" object that can cache
## its inverse, which is a list containing function to:
     ## 1. set the value of matrix --> set
     ## 2. get the value of matrix --> get
     ## 3. set the value of inverse --> setinv
     ## 4. get the value of inverse --> getinv

makeCacheMatrix <- function (x = matrix()) {
     inv <- NULL
     
     set <- function (y) {
          x <<- y
          inv <<- NULL
     }
     get <- function () x
     setinv <- function (inverse) inv <<- inverse
     getinv <- function () inv
     
     list (set = set, get = get, 
           setinv = setinv, 
           getinv = getinv)
}

## cacheSolve computes the inverse of the special "matrix" returned
## by the makeCacheMatrix.

     ## If the inverse has already been calculated, then would
     ## retrieve directly from cache.
     ## Otherwise, it calculates and sets the inverse in the cache
     ## via the setinv function above.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inv <- x$getinv()
     
     if (!is.null(inv)) {
          message ("getting cached data")
          return (inv)    
     } else {
          inv <- solve (x$get(), ...)
          x$setinv (inv)
          return (inv)
     }
}