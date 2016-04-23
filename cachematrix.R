## Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly

## For example:
## > m<-makeCacheMatrix(matrix((1:4),2,2))
## > m$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## > cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5



## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    if(!is.matrix(x) && is.na(x)) {
        stop("NA value, not valid")
    }
    else if(nrow(x) != ncol(x)){
        stop("Only square matrix is invertible")
    }
    
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setsolve <- function(solve) inv <<- solve
    getsolve <- function() inv
    list(set = set,
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}



## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  
    inv <- x$getsolve()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setsolve(inv)
    inv
}


