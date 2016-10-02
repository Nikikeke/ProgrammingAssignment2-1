## These functions cache the inverse of a matrix.

## Defines a matrix object with cachable inverse value
## Defines a Matrix object which can cache its inverse
## This function creates a special "matrix" object that can cache its inverse.
## Actually a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inverse <<- solve
        getsolve <- function() inverse
        list(set = set, get = get, 
             setsolve = setsolve,
             getsolve = getsolve)
}


## Takes a cache Matrix object, checks if the inverse has already been
## computed, calculates if it has not, and then returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getsolve()
        if (!is.null(inverse)) {
                message("Getting cached data")
                return (inverse)
        }
        data <- x$get()
        inverse <- solve(data,...)
        x$setsolve(inverse)
        inverse
}
