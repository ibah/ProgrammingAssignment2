## Following are two functions design to improve computing inverse of a matrix
## by storying inverted matrix for future use.
## If theres another call to compute an inverse for the same matrix
## the previous result is retrived and no more computation take place.

## Here is an example usuage of these functions:

##  m <- makeCacheMatrix()                      ## creates empty matrix
##  m$set(matrix(c(4,3,3,2),nrow=2,ncol=2))     ## sets the matrix
##  m$get()                                     ## view/get the matrix
##  cacheSolve(m)                               ## the first call to get the inverse
                                                ## here solve function is called
                                                ## to obtain the result
##  cacheSolve(m)                               ## the second call for the same matrix
                                                ## the inverse will be simply retrived

## This function creates a special matrix that stores its inverse for futer use.
## It's a list containing functions that allow to:
## set - set the matrix
## get - get the matrix
## setinverse - set the inverse of the matrix
## getinverse - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates inverse of a function created using
## the makeCacheMatix function.
## If available the function retrives the stored inverse matrix,
## else it uses solve function to calculate the inverse and stores it
## for future use.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
