## Function to create and store a matrix and its inverse. Set stores
## and get returns either the matrix or the inverse

## This takes the matrix input and creates a list of 4 possible actions:
## set creates a matrix
## get returns the matrix
## setinverse creates the inversed matrix
## getinverse returns the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## this function looks to get the stores inverse if there is one, if the returned
## value is blank it then gets the initial matrix and solves it

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}