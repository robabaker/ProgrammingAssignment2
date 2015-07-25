## This code creates an object that stores a matrix and calculates its inverse
## The code takes advantage of cached results is they are available

## makeCacheMatrix creates a list containing a function to:
##  set and get the value of a matrix
##  set the value of the inverse of the matrix and get that value

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setinv <- function(inv) m <<- inv
      getinv <- function() m
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve calculates the inverse of the matrix from makeCacheMatrix.  If the inverse is still in cache then
##   it skips the computation and uses it directly.  Otherwise it calculate the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data,...)
      x$setinv(m)
      m
}

  