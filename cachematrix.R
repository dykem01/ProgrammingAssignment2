## makeCacheMatrix() creates a special "matrix" object that can cache its inverse

## Given an invertible square matrix, makeCacheMatrix will return a list 
## containing functions to 1. set the matrix; 2. get the matrix; 
## 3. set the inverse; and 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
              x <<- y
              inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set=set,get=get,
             setinv=setinv,
             getinv=getinv)
}


## cacheSolve() computes the inverse of the "matrix" that is returned by 
## makeCacheMatrix().  If the inverse has already been calculated and the matrix
## hasn't changed, this function will retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        ## check if inverse already calculated
        if(!is.null(inv)) {
              ## retrieve from cache and do not recalculate
              message("retrieving from cache")
              return(inv)
        }
        ## else calculate the inverse
        data <- x$get()
        inv <- solve(data, ...)
        ## place inverse in cache using setinv function
        x$setinv(inv)
        inv
}
