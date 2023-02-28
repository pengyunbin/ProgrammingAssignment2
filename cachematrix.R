## Put comments here that give an overall description of what your
## functions do
## These functions cache the inverse of a matrix

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

        # set the matrix
        m <- NULL
        set <- function(matrix) {
                x <<- matrix
                m <<- NULL
        }
        # get the matrix
        get <- function() x

        # set the inverse of matrix
        setinverse <- function(inverse) m <<- inverse
        # get the inverse of matrix
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

        # get the matrix
        data <- x$get()

        # calculate the inverse of matrix
        m <- solve(data, ...)
        
        x$setinverse(m)
        m
}
