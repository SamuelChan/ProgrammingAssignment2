## The purpose of this code file is to compute the inverse matrix from specified
## invertible matrix by exploting the scope rules to cache the computed result,
## which is helpful to reduce the unnecessary re-computation.

## This function below takes the input parameter "x" of matrix, and define the 
## functions to access the variables of specified matrix and cached inverse matrix. 
## Then return these functions as a list.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # assign the value of specified matrix and clear the cache variable
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # return the value of specified matrix 
    get <- function() x
    # assign the value of computed inverse matrix to cache variable 
    setInverse <- function(InverseMatrix) m <<- InverseMatrix
    # return the value of computed inverse matrix from cache variable
    getInverse <- function() m
    list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function takes the returned object of "makeCacheMatrix" function as input
## parameter "x". Then check whether the value of inverse matrix is cached or not.
## If not, compute the inverse matrix by using solve function and cache this result.
## In the end, return the inverse matrix of specified matrix.
cacheSolve <- function(x, ...) {
   m <- x$getInverse()
   if (!is.null(m)) {
       message("getting cached data")
       return(m)
   }
   dat <- x$get()
   m <- solve(dat, ...)
   x$setInverse(m)
   m
}