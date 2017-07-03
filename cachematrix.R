## cached/memoized matrix operations
##
## A cachematrix created by `makeCacheMatrix` can save results of
## expensive matrix operations to avoid recomputation
##
## for R Programming course in JHU Data Science Coursera track
##
##
## Usage:
##
##   > m <- makeCacheMatrix(matrix(c(4, 2, 7, 6), 2, 2))
##   > cacheSolve(m)


## create a cachematrix that can store its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set = function(x_) {
    x <<- x_
    inv <<- NULL
  }
  list(set = set,
       get = function() x,
       setinverse = function(inv_) inv <<- inv_,
       getinverse = function() inv)
}

## return matrix inverse of x, using cached result if available
##
## x must be a cachematrix resulting from a call to `makeCacheMatrix`
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv))
    return(inv)
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
