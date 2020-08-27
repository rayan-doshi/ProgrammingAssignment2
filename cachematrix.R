makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                                            
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# we have set the makeCacheMatrix function and 
# initialized the reuqired values.
# we will call this function while working, and pass our original matrix as input

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

# the cacheSolve function, by usingthe solve() function,
# will return and cache the value of the inverse matrix
