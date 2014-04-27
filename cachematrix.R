## Matrix inverse computation - time consuming task. Using these two functions it is easy to
## get rid of inverse computation because it is computed just once.

## This one returns special list containig special functions for get-set
## of numeric matrix an inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This one returns inverse of matrix if possible and caches the result for \
## further use.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  ## Check if matrix is square and of full rank and store an error msg if not
  if(dim(data)[1]!=dim(data)[2]) {
      m<-("Not invertible")
      x$setInverse(m)
      return(m)
  }
  ## Return a matrix that is the inverse of 'x'
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}