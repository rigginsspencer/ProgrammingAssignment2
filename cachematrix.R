## The following functions utilize the <<- operator to store
## the value of an inverse matrix in the cache. The second
## function, in order to save computing time, will simply
## return the inverse matrix if it is stored in the cache,
## or calculate the inverse matrix manually.

## The following makeCacheMatrix function inputs a matrix and
## returns a list containing functions to set the value of the
## matrix, get the value of the matrix, set the value of the
## inverse matrix, and get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y)
      {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(x) m <<- x
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## The following cacheSolve function inputs a list returned
## from makeCacheMatrix. If the inverse of the matrix is
## already cached it is retrieved from the cache using the
## getinverse function. If it is not already cached, then
## the inverse is calculated manually.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m))
      {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data,...)
      x$setinverse(m)
      m
}
