##  A collection of functions for the following:
## 'set in cache' and 'get from cache' the inverse of a matrix
#
## The following function create a special matrix object that can cache inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
        ## m holds the cached inverse matrix or NULL if nothing cached
        m <- NULL     #initially nothing cached, set it to null
        
        ## Store a matrix
        set <- function(y) {
                x <<- y
                m <<- NULL          ## as the inverse of the matrix is no longer valid  
        }
        ## Get the stored matrix    
        get <- function() { 
                x
        }
        ## Cache the inverse of matrix
        setmatinverse <- function(solve) {
                m <<- solve
        }
        ## get the cached inverse matrix
        getmatinverse <- function() {
                m
        }
        ## the following lists functions
        list(set = set, get = get,
             setmatinverse = setmatinverse,
             getmatinverse = getmatinverse)
}
## The following function calculates Matrix Inverse of matrix created with makeCacheMatrix
#
cacheSolve <- function(x, ...) {
        m <- x$getmatinverse()   ## get cached value     
        if(!is.null(m)) {
                message("getting cached data")
                return(m)        ## if cached value exists, return it
        }
        
        #else, get matrix, calculate its inverse and store it in cache m
        data <- x$get()               
        m <- solve(data)      
        ###inverse matrix in cache
        x$setmatinverse(m)
        m                     # return inverse matrix
}
## In order to test, following was used:
## q <- makeCacheMatrix( matrix(1:4, 2, 2) );
## 
## cacheSolve(q)          # get inverse of the matrix
##
## cacheSolve(q)          # get again, this time it is returned from cache - notice the message
#
#
