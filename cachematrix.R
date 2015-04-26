#########################################################################################
#Date: 26 Apr 2015
#Author: Anthi
#Course: Coursera - R Programming
#Programming assignment 2
#
#Description: The following two functions can be used to cache the inverse of a matrix
#             so that when we need it again, it can be looked up in the cache rather than
#             recomputed and thus avoid potentially time-consuming computations.
##########################################################################################

## Creates a special "matrix" to be used within the cacheSolve function

makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL                     
        # set the value of the matrix
        set <- function(y) {                      
                x <<- y
                m <<- NULL              
        }
        # get the value of the matrix
        get <- function() x                           
        # set the inverse matrix
        setinverse <- function(solve) m <<- solve 
        # get the inverse matrix    
        getinverse <- function() m        
        
        list(set = set, get = get,                    
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculates the inverse of the special "matrix" created with the above function

cacheSolve<- function(x, ...) {                 
        m <- x$getinverse()
        #if already calculated, get the inverse
        if(!is.null(m)) {                 
                message("getting cached data")
                return(m)
        }
        #calculate the inverse
        data <- x$get()                               
        m <- solve(data, ...)
        x$setinverse(m)
        
        m
}
