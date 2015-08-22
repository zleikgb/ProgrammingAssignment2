## This function creates a special "matrix" object that can cache its inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinversion <- function(inversion) inv <<- inversion
    getinversion <- function() inv
    list(set = set, get = get, setinversion = setinversion, getinversion = getinversion)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinversion()
    if(!is.null(inv)) {
        message("getting cached data")
        return inv
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinversion(inv)
    inv
}
