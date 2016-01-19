## Compute the inverse of a square matrix and store it in the cache.

## create a matrix to set the value of the matrix, get the value of the matrix, set the
## value of the inverse and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## calculate the inverse of the matrix created with the above function. It first checks
## if the inverse is already calculated. If yes, it gets the inverse from the cache and
## skips the calculation. Otherwise it calculates the inverse of the matrix and sets
## the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
