## makeCacheMatrix is a function returning a list of matrix
## input: an invertible matrix
## output: list of functions to set the matrix, get the matrix,
##         set the inverse, get the inverse 


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
         ## function below sets the matrix 
        set <- function(y) {
    
         x <<- y
         inv <<- NULL
        }
         ## function below gets the matrix
         get = function() x
         ## function below sets the inverse
        setinv = function(inverse) inv <<- inverse
         ## function below gets the inverse
        getinv = function() inv
         list (set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve is a function that return the inverse of a matrix
## input: matrix from makeCacheMatrix
## output: inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$getinv()
        ## if we already have the inverse
        if (!is.null(inv)) {
        message("geting cached data")
          ## get it from the cache matrix
        return (inv)
         }
         ## if not, get the inverse
         data = x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
  
         return (inv)
}
