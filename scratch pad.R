makeCacheMatrix <- function(x = matrix()) {
    # create a blank variable m
    m <- NULL
    # define 'set' as a function where x is set to the same as y and m is cleared:
    # thse both pass to the parent environment because of the use of <<-
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # define 'get' as a function to return x which has previously been set
    get <- function() x
    # define 'setinverse' as the inverse of x using the solve function, use
    # <<- m to pass this value up to m in the aprent function
    setinverse <- function(solve) m <<- solve
    # define 'getinverse' as a function to return m (the inverse of x)
    getinverse <- function() m
    # create a list where the contents are the returned values of the above functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    # sets m to the value of the inverse of the matrix
    m <- x$getinverse()
    # check if the inversed matrix exists (double negative)
    if(!is.null(m)) {
        # if it does then return the cached value
        message("getting cached data")
        return(m)
    }
    # if the inversed matrix does not exist then set data to the 'get' value
    data <- x$get()
    # solve (invert) the matrix just stored
    m <- solve(data, ...)
    # pass the inverted matrix back to 'setinverse'
    x$setinverse(m)
    # print the inverse of the matrix
    m
}