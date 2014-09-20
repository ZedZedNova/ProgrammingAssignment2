#
## makeCacheMatrix() caches the inverse of a square invertable matrix.
## Because calculating inverses of large matrices can be expensive
## it makes sense to solve the inverse of a matrix once and use the
## cached inverse, rather than recalculating the inverse each time.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # get the value of the matrix
    get <- function() x
    
    # set the value of the matrix inverse
    setsolve <- function(solve) m <<- solve
    
    # get the value of the matrix inverse
    getsolve <- function() m
    
    list(set = set,
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve() will calculate and return the inverse of a square
## invertable matrix. When cacheSolve() is called it first checks
## to see if the matrix inverse has already been calculated. If
## the inverse of the matrix has not already been calculated, the
## inverse is calculated and cached for later use. If, however,
## the inverse has already been calculated, the cached result is
## returned rather than re-calculating the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    # retrieve the current matrix inverse
    m <- x$getsolve()
    
    # check to see if the matrix inverse exists
    if(!is.null(m)) {
        # The inverse was already calculated
        message("getting cached data...")
        return(m)
    }
    
    # There is no cached matrix inverse, so let's
    # calculate the inverse and cache it.
    
    # retrieve the matrix data
    data <- x$get()
    
    # calculate the matrix inverse
    m <- solve(data, ...)
    
    # update the cache with the matrix inverse
    x$setsolve(m)
    
    # return the matrix inverse
    m
    
}
