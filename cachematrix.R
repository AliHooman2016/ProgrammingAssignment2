## Builds a special matrix that can cache the inverse of itself
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL          #initial Null
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function () x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Computes the inverse of the matrix returned by `makeCacheMatrix`

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached inverse")
        return(m)
    }
    data <- x$get()
    m <- solve(data) %*% data
    x$setinverse(m)
    m
}

