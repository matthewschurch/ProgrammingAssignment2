## Put comments here that give an overall description of what your
## functions do:
## Script written by Matthew Schurch 22/10/2014.

## Function Descritpions:
## makeCacheMatrix and cacheSolve are two functions that allow the user to calculate and cache the inverse
## of a square matrix.  See individual functions for additional explanations.

## Write a short comment describing this function
## makeCacheMatrix:  This function creats a special vector that stores a matrix and its cached inverse.  
## The individual functions contained within the special vector allow the user to set and retrieve the cached inverse matrix, as well as the original matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv.mat) inv <<- inv.mat
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    

}


## Write a short comment describing this function
## CacheSolve:  This function will return the inverse of a matrix.  Should the correct inverse exist in the cached memory then it will return it without computation.
## However, should the inverse not exist it will both calcualate it and then cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
            message("getting cached inverse matrix")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
    }
    
    
