## This function creates a special "matrix" object that can cache its inverse

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

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
    inv <- solve(data)
    x$setinversion(inv)
    inv
}
