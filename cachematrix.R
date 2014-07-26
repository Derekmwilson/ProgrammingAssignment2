## R Programming: Assignment 2
## Derek M Wilson

## This code creates a special object and function to cache and retrieve the inverse
##      of a provide matrix.
## The intention is to avoid the time required to find the inverse of large matrices

## f:makeCacheMatrix
## The following functon will create a special object with a list of associated functions
## set: sets the applied matrix and removes any existing cached inverse
##      the set method will check fro a square matrix
## get: returns the matix provided
## setinverse: sets the cache variable to the inverse matrix provided
## getinverse: retrieves the cached inverse value

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                if (nrow(y)==ncol(y)){
                        x <<- y
                        m <<- NULL
                        ## let the user know that a matrix has been applied and the dimensions
                        message(paste("Set ",nrow(y),"x",ncol(y)," matrix.",sep=""))
                }
                else {
                        ## Let the user know that they have provided a matrix that is not square 
                        ##      and for which an inverse is not possible
                        ## Yeah, I know. I was supposed to assume the matrix would
                        ##      always be invertible
                        message("**ERROR: you have not provided a square matrix **")  
                }      
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        
}

## f:cacheSolve
## The following function will retrieve the cached inverse if existing...
##      or exectue the required solve() function to cache and reutnr the inverse
## The function will notify the user if the inverse was already cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("...retrieving cached inverse..")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
