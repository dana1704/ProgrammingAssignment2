makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

makeCacheMatrix <- function(a = matrix()) {
  j <- NULL
  set <- function(b) {
    a <<- b
    j <<- NULL
  }
  get <- function() a
  setinverse <- function(inverse) j <<- inverse
  getinverse <- function() j
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function(a, ...) {
  j <- a$getinverse()
  if (!is.null(j)) {
    message("getting cached data")
    return(j)
  }
  data <- a$get()
  j <- solve(data, ...)
  a$setinverse(j)
  j
}
