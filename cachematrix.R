## Functions created to compute a matrix and 
## retrieve its inverse from cache

## Steps are: Set a Matrix
## Get the value of the matrix
## Inverse the matrix
## Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list (set = set, 
              get = get, 
              setInverse = setInverse, 
              getInverse = getInverse)
        }

## Function computes the inverse of the "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## 'x': Output of makeCacheMatrix
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("Getting cache data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
