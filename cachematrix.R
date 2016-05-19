## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.
## The following two functions are used to create a special object that stores a matrix 
## and cache its inverse value.

## MakeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inversed matrix
## 4.get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## CacheSolve returns the value of inversed matrix created by the makeCacheMatrix function. 
## However, it first checks to see if the inversed matrix has computed. 
## If so, it gets the inverse value from the cache and skips the computation. 
## Otherwise, it calculates the inverse value of the data, 
## and sets the value of the inversed matrix in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
           message("getting cached data.")
           return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}

