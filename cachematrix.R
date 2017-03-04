## This pair of functions caches the inverse of a matrix.

## This first function creates a special "matrix" object that can cache its 
## inverse. Note that this function isn't actually creating the inverse of the
## matrix, but setting up the object that enables the inverse to be cached  
## by using "setsolve" and "getsolve". The function that will actually solve 
## (or find the inverse of) the matrix is cacheSolve (if it hasn't been 
## previously calculated. If it has been calculated already, cacheSolve will
## simply pull the saved solve values (the matrix inverse) from the cache).

##makeCacheMatrix defines the cached matrix as 'x'.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)    
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix 'm' that is the inverse of 'x'
    m <- x$getsolve()
    ## this next step is where the time-saving magic happens: if m is already 
    ## cached, it prints the message "getting cached data" and then returns 'm'.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## if 'm' hasn't already been calculated, the next step calculates it.
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

## to test whether this function is working properly, create a matrix like so:
## m1 <- matrix(c(4, 5, 6, 7), nrow = 2, ncol = 2)
## then call the matrix to view it.
## then calculate the inverse with solve(m1)

## next run the matrix through the first function like so:
## a <- makeCacheMatrix(m1)
## then use cacheSolve to get the inverse like so:
## cacheSolve(a)
## The first time it's called, it will actually calculate and print the inverse.
## So to test whether the inverse has been cached, run it again:
## cacheSolve(a)
## If working properly, this should return the message "getting cached data"
## and then print the inverse of the m1 matrix (shown below)
##      [,1] [,2]
## [1,] -3.5    3
## [2,]  2.5   -2

## I hope my comments were clear. Thanks for taking the time to review my work!