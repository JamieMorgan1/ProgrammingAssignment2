# makeCacheMatrix creates a "matrix" object that can cache its inverse.
# The object can be used to: set the value of the matrix, get the value of the matrix,
# set the value of the inverse, and get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# cacheSolve accepts a cacheMatrix object and returns the matrix inverse  
# If the matrix has already been calculated it gets the inverse from the cache and skips the computation. 
# If not, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.
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
