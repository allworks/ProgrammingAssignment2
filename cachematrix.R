## This file contains two functions: makeCacheMatrix and cacheSolve

## makeCacheMatrix 
## This function creates a special "matrix" object that can cache its inverse.

## cacheSolve
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated and has not
## changed, then the inverse is returned from the cache.

## makeCacheMatrix
## input: a matrix
## return: a special "matrix object that can cache its inverse while providing
##         four functions: getData, setData, setInverse, and getInverse
makeCacheMatrix <- function(x = matrix()) {
    
    # private fields
    this.inverse <- NULL
    
    # public methods
    setData <- function(data) {
        x <<- data   
        this.inverse <<- NULL
    }
    
    getData <- function() x
    
    setInverse <- function(inverse) {
        this.inverse <<- inverse
    }
    
    getInverse <- function() { this.inverse }

    # return a list of public methods
    list(set = setData, 
         get = getData, 
         setInverse= setInverse, 
         getInverse= getInverse)
}

## cacheSolve
## input: "matrix" object returned from makeCacheMatrix
## return: the inverse of the matrix stored in the "matrix" object
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return (inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    
    # cache the inverse
    x$setInverse(inverse)
    inverse
}
