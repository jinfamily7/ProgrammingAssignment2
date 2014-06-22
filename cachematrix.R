## the first function creates a list that stores a matrix and its inverse, the second function returns the inverse of the matrix
## functions do

## this function creats a list that stores an invertible numeric square matrix 'x' and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##  this function calculates the inverse of a square matrix 'x'
cacheSolve <- function(x, ...) {
       a <- makeCacheMatrix(x)
       m <- a$getinv()
       if(!is.null(m)) {
            message("getting cached data")
            return(m)
       }
       data <- a$get()
       m <- solve(data)
       a$setinv(m)
       m             ## Return a matrix that is the inverse of 'x'
}
