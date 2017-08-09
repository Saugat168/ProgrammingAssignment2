## The set function initializes the matrix and its corresponding inverse matrix
## The inverse matrix is initialized with NA(s)
## The set and get inverse functions saves and fetches the inverse of the matrix respectively

makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix(NA,nrow(x),ncol(x))
  set <- function(y) {
    x <<- y
    inv <- matrix(NA,nrow(x),ncol(x))
  }
  get <- function() x
  setInverse <- function(Inverse) inv <<- Inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve methods checks if the inverse is already cached
## It does that by checking for any NAs in the matrix as that is how it was initialized when the inverse wasn't calculated
## When it sees that it is not cached, it calculates it and saves it for later use

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(any(!is.na(inv))) {
    message("getting cached inversed matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
