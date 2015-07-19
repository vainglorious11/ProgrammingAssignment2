
## R PROGRAMMING
## ASSIGNMENT 2

## This  is a pair of functions that:
## 1. Create a list of functions that store a matrix and its inverse
## 2. Access the cached inverse if it exists, or else find the inverse
##    and cache it

## 1. This function creates a special "matrix" object, which is
## really a vector of four functions used to store and 
## retrieve the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    # create the object m within the "matrix" object's environment
    m <- NULL
    
    # define a function to store the value of the matrix as x and
    # reset m to null, within the "matrix" object's environment
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # define a function to return the value of the matrix.  x is not 
    # defined within the function so R finds it within the parent
    # environment
    get <- function() x
    
    # define a function to store the inverse of the matrix as m
    # The <<- operator searches through this function's
    # parent environment and find m which was already created 
    # inside the "matrix" object's environment 
    setinverse <- function(inverse) m <<- inverse
    
    # define a function to return m, which is either NULL or
    # the inverse of the matrix which was stored by setinverse()
    getinverse <- function() m
    
    # return a vector composed of the functions defined above
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## 2. This function computes the inverse of the special "matrix"
## object returned by makeCacheMatrix above. If the inverse has
## already been calculated, then cacheSolve retrieves the
## inverse from the cache.

cacheSolve <- function(x, ...) {
    # retrieve the value of m stored in the environment of  the 
    # "matrix" object (accessed by calling the "getinverse" function 
    # within that object). m will be NULL if the inverse hasn't been 
    # cached yet, or it will be the inverted matrix if it has been
    # stored.
    m <- x$getinverse()
    
    # If m is not NULL, then it will be the cached matrix.  
    # Return the cached matrix.
    if(!is.null(m)) {
        
        # let the user know R found the cached data
        message("getting cached data")
        # return the inverted matrix
        return(m)
        
        # If m is NULL then we need to invert the matrix and store it 
        # in the special "matrix" object using setinverse()
    } else {
        # retrieve the original matrix from within the special
        # "matrix" object's environment using its get() function
        # and store it as 'data'
        data <- x$get()
        
        # let the user know that R will be working on solving the  matrix
        # (since it can take some time to finish)
        message("solving for inverse of matrix...")
        
        # use the solve() function from the base package to find the
        # inverse of the original matrix; store it as m
        m <- solve(data)
        
        # use the setinverse() function from the special "matrix" 
        # object to cache the inverted matrix in that special object's
        # environment.
        x$setinverse(m)
        
        # return the inverted matrix
        m
    }
}