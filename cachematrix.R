# makeChacheMatrix returns a list of functions to: set matrix, get matrix, set calculated inverse, get calculated inverse.
# cacheSolve returns the cached inverse if it exists. It checks that the original matrix has not changed.

##  Calculate inverse of matrix x and cache it. returns a list object. call "getInverse" to retrieve.
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL # set cachedInverse to NULL
  set <- function(y) { # define a function to set the matrix, x, to a new matrix, y. Reset cachedInverse to NULL
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) cachedInverse <<- solve # sets cachedInverse, to a new matrix, solve.
  getInverse <- function() cachedInverse # returns the inverse, cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##  Return the cached inverse of a matrix if it exists.
cacheSolve <- function(x=matrix(), ...) {
  cachedInverse<- x$getInverse()
  if(!is.null(cachedInverse)){    # check if inverse of matrix exists. if it does, return it.
    message("getting cached data")
    return(cachedInverse)
  }
  matrix<-x$get()                  # else, calculate inverse of the matrix.
  cachedInverse<-solve(matrix, ...)
  x$setInverse(cachedInverse)
  cachedInverse
}