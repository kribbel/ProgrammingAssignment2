## Computing Inverse of a matrix
## for optimisation cache the result

## function to create a cached matrix

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


## function to inverse (solve) a matrix and cache it or if available return the cached inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    #print("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  message("solving data")
  #print("solving data")
  x$setsolve(m)
  m
}
