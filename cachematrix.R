#That functions will make the inverse of a matrix, but to save some memory and make the
#process fast, the function will verify if there is the answer in cache.

#This function create a space to cache a matrix, so you must to create a matrix and then
#cache that in makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
#This function makes the inverse of a matrix, but first check if the answer is in cache
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
