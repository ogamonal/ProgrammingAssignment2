## Put comments here that give an overall description of what your
## functions do
## The following functions obtains the inverse of a matrix 'x'

## Write a short comment describing this function
## The makeCacherMatrix assign and returns the  values of a matrix 'x' and
## its matrix inverse. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
       x <<- y
       m <<- NULL
    }
    get <- function () x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Write a short comment describing this function
## The cacheSolve function calculates and return the inverse matrix of 
## the matrix 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cache data")
            return(m)
        }
        data <- x$get
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
