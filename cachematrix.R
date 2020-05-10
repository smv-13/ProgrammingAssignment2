## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setSolve <- function(solve) m <- solve
        
        getSolve <- function() m
        
        list(set = set, get = get, setsolve = setSolve, getsolve = getSolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        
        if(!is.null(m)) {
                message("Getting chached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data)
        x$setsolve(m)
        
        m
}
