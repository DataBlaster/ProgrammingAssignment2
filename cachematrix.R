## Overview: makeCacheMatrix and cacheSolve work together to optimize the processing
## time of calculating the inverse of an invertible matrix through the use of 
## memory cache.  Essentially, it will check to see if the inverse of an invertible
## matrix has been already calculated.  In which case, it will return the cache 
## calculation, else it will calculate the inverse through the use of solve function.


## Function name: makeCacheMatrix
## Purpose: Creates "special" matrix that can cache its inverse using solve function.
## Operations:
## 1. set value of matrix
## 2. get value of matrix
## 3. set the value of solve
## 4. get the value of solve

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


## Function name: cacheSolve
## Purpose: Will check cache to see if inverse of "special" matrix is already calculated,
## else, will calculate.
## Operations:
## 1. check cache to see if inverse has been calculated already.
## 2. If exist, use cache value
## 3. If not exist, call solve to calculate inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
