## Author: Daniel Calife
## This is the solution for the Programming Assignment 2 of the R Programmming
## course
## These functions are use to create a new type of matrix that can store its
## inverse, eliminating the need to calculate it everytime, oncce the matrix
## is still the same

## this function create the matrix that has a attribute to store its inverse
## to manipulate this matrix you have to use set and get

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## stores the inverse matrix
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    
    ## return list
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## this function receives a matrix created with makeCacheMatrix and
## returns its inverse, cached if it exists or calculated

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## calculates the inverse matrix
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
