

## makeCacheMatrix is a function that gets a matrix and turns it to a list which can
## contain the inverse of the matrix as a cached value. In other words it transforms a 
## normal matrix to a 'special' matrix.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve gets a list which may contain a matrix and its inverse. If the inverse 
## matrix is calculated previously, it will be returned. Otherwise, the inverse is
## calculated and is set into the list (special matrix).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinvers()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

