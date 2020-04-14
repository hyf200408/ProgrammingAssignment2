## The two functions below work collaboratively to store a matrix, solve and
## cache its inverse matrix, and use the cache afterwards to save resources.

## The first function `makeCacheMatrix` creates a special "matrix", which is
## actually a list of functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The second function `cacheSolve` calculates the inverse matrix of
## the special "matrix" defined with `makeCacheMatrix`. It first
## checks to see if the inverse matrix has already been calculated.
## If so, it `getinv`s the inverse matrix from the cache and skips
## the computation. Otherwise, it calculates the inverse matrix and
## sets the value in the cache via the `setinv` function.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
