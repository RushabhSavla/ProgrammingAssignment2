## Inverse of a matrix functions for faster cache operations

## First function. Creates a special vector object. 
makeCacheMatrix <- function( x = matrix() ) {
  i <- NULL
  set <- function( matrix ) {
    x <<- matrix
    i <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {
    i <<- inverse
  }
  getInverse <- function() {i}
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## calculate the inverse if not already calculated. Else use the cache.
cacheSolve <- function(y, ...) {
  x <- y$getInverse()
  if( !is.null(x) ) {
    message("Already calculated. Returning cached copy")
    return(x)
  }
  data <- y$get()
  x <- solve(data) %*% data
  y$setInverse(x)
  x
}
