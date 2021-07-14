## This create special matrix
library(MASS) ##this library is used to calculate inverse of non-sq. matrix
makeCacheMatrix <- function(x = matrix()) {
  
    Inver <- NULL
    set <- function(y) {
      x <<- y
      Inver <<- NULL
    }
    get <- function() x
    setinver <- function(solve) Inver <<- solve
    getinver <- function() Inver
    list(set = set, get = get,
         setinver = setinver,
         getinver = getinver)
  
  
}


## here we get cahce data of matrix Inverse or Inverse data

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    Inver <- x$getinver()
    if(!is.null(Inver)) {
      message("getting cached data")
      return(Inver)
    }
    data <- x$get()
    Inver <- ginv(data, ...)
    x$setinver(Inver)
    Inver
  
}