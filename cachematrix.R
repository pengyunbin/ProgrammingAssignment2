## Put comments here that give an overall description of what your
## functions do
## These functions cache the inverse of a matrix

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(matrix) {
        x <<- matrix
        m <<- NULL
    }
    get <- function() x

    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m

    list(set = set, get = get,
        getinverse = getinverse, setinverse = setinverse)
}


## Write a short comment describing this function
## This function first check whether the inverse is already calculated,
## if so, inverse is returned. Otherwise, it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()

        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
