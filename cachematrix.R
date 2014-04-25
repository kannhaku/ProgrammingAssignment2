##makeCacheMatrix creates a special "matrix" object that can cache its inverse.
##and cachesolve computes the inverse of the special "matrix".
##If the inverse has already been calculated ,
##then the cachesolve should retrieve the inverse from the cache.
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) ｛
  # initialize the stored inverse value to NULL
  inverse_x <- NULL
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  # to get the value of the matrix
  get <- function() x
  # to set the inverse
  setinverse <- function(inverse) inverse_x <<- inverse
  # to get the inverse
  getinverse <- function() inverse_x
  # return a list of all the above functions
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
   # check if the inverse is already cached
    inverse_x <- x$getinverse()
    if (!is.null(inverse_x)) {
        message("getting cached inverse matrix")
        return(inverse_x)
    } else {
    # not cached, so we get the matrix into data
    # and compute the inverse
        inverse_x <- solve(x$get())
    # then cache the inverse
        x$setinverse(inverse_x)
    # and return it as well
        return(inverse_x)
    }
}
