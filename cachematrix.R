## These two functions are to compute the inverse of a matrix
## Inverse is cached so it can be looked up instead of a recomputation

## This function can be used to save an exising matrix,
## create a new matrix, get inverse (minv) from the cacheSolve function
## or set the inverse manually

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinv <- function(solveinverse) minv <<- solveinverse
  getinv <- function() minv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the matrix 
## from the makeCacheMatrix function above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  minv <- x$getinv()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data)
  x$setinv(minv)
  minv
}
