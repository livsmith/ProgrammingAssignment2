## These functions are for the Coursera "R Programming" Course.
## They allow the user to cache the inverse of a matrix
## This can be used to avoid re-doing a slow calculation

## makeCacheMatrix sets up the cached matrix.
## it returns a vector of four functions which are used 
## to set and get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## if setting a new x, the inverse is now null
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve finds the inverse of the matrix x if it hasn't
## been found yet.  It stores that inverse for future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message ("getting cached solution")
        return(inv)
    }
    ## if no existing solution, we need to re-solve
    inv <- solve(x$get(), ...)
    x$setinv(inv)
    inv
}
