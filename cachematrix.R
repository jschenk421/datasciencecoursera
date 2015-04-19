## These R functions are able to cache the potentially time-consuming 
## computation of inverting a matrix.
##
## They can for example be invoked as:
##
## m <- matrix(c(-1, -2, 1, 1), 2,2)
## x <- makeCacheMatrix(m)
## x$get()
##
## inv <- cacheSolve(x)
## inv
##
## inv <- cacheSolve(x)
## inv
##
## inv will contain the inverse of m
## if the inverse is already in memory then a message will be shown
## that the inverse is loaded from cache


## This function, makeCacheMatrix creates a special "matrix", which
## is really a list containing a function to
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the value of the inverse in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
