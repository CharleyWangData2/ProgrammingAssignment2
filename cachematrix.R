## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly 
## two functions here, save inversion values 
##                     and search or calculte inversion values

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  val <- NULL
  set <- function(y) {
    x <<- y
    val <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) val <<- inverse
  getInverse <- function() val
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  val <- x$getInverse()
  if(!is.null(val)) {
    message("getting cached data")
    return(val)
  }
  data <- x$get()
  val <- solve(data)
  x$setInverse(val)
  val
}

## Testing commends

## x1 <- matrix(rnorm(9), 3, 3)
## x2 <- matrix(rnorm(16), 4, 4)
## x3 <- matrix(rnorm(25), 5, 5)

## m1 <- makeCacheMatrix(x1)
## m2 <- makeCacheMatrix(x2)
## m3 <- makeCacheMatrix(x3)

## cacheSolve(m1)
## cacheSolve(m2)
## cacheSolve(m3)

## cacheSolve(m1)
## cacheSolve(m2)
## cacheSolve(m3)
