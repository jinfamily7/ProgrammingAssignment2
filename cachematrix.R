## the first function creates a list that stores a matrix and its inverse, the second function returns the inverse of the matrix


## this function creats a list that stores an invertible numeric square matrix 'x' and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        inv <- solve(x)                       ##inv is the inverse of the square matrix x
        get <- function() x                   ##this function gets the matrix x
        setinv <- function(inv) m <<- inv     ##this function sets the inverse of x
        getinv <- function() m                ##this function gets the inverse of x
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##  this function calculates the inverse of a square matrix 'x'
cacheSolve <- function(x, ...) {
       a <- makeCacheMatrix(x)
       m <- a$getinv()         ##this gets the inverse of the matrix x and sends it to m
       if(!is.null(m)) {
           message("getting cached data")
           return(m)
       }
       data <- a$get()         ##this finds the matrix x and sends x to the variable data
       m <- solve(data)        ##this finds the inverse of the matrix x
       a$setinv(m)             ##this updates the inverse of x in the setinv function
       m                       ##output the inverse of x to the cacheSolve function
}
