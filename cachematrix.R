## Below are two functions that are used to create a special object 
## that stores a matrix and caches its inverse.


## makeVector creates a list consisting of four functions. The first and second
## set and get the value of the matrix respectively. The third and fourth set and
## get the value of the inverse respectively.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This calculates the inverse of the matrix. Before calculation occurs, the
## function checks to see if an inverse is cached. If so, the cached value is
## returned. Otherwise, the computation is done and the inverse of the matrix
## is set into the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}
