## The first function, makeCacheMatrix creates a inverse of the matrix. There are functions to get a matrix,
## set a matrix, get inverse of matrix using solve() and set inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function takes the inverse of the matrix and checks if the inverse is already cached,
## if it is already calculated and cached, then it returns the cached copy, else calculates the 
## inverse of the matrix and sets the value of the inverse in the cache via the setinv function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

