##makeCacheMatrix creates a special "matrix" object that can cache its inverse.
##and cachesolve computes the inverse of the special "matrix".
##If the inverse has already been calculated ,
##then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) ｛
  inverse_x <- NULL
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_x <<- inverse
  getinverse <- function() inverse_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
}

## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if
## not, it computes, caches, and returns it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_x <- x$getinverse()
    if (!is.null(inverse_x)) {
        message("getting cached inverse matrix")
        return(inverse_x)
    } else {
        inverse_x <- solve(x$get())
        x$setinverse(inverse_x)
        return(inverse_x)
    }
}
