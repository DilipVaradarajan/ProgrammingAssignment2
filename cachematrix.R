## The following functions implement a simple cache for caching the inverse of a matrix.

## This function creates a special matrix which really is a function to
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <- function(inv) {
        inverse <<- inv
    }
    getInverse <- function() {
        inverse
    }
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function calculates the inverse of a given matrix. It first checks to see if the inverse has
## already been calculated. If so, it returns the inverse from the cache. If not, it computes the inverse
## and stores the inverse in the cache

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if (!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
