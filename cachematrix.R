# This code contains two functions that 1) create special matrices,
#  and 2) creates  
#
#
#The first function, `makeCasheMarix` creates a special "vector", which is
#really a list containing a function to

#1.  set the value of the vector
#2.  get the value of the vector
#3.  set the value of the mean
#4.  get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setINV <- function(inv) m <<- inv
    getINV <- function() m
    list(set = set, get = get,
         setINV = setINV,
         getINV = getINV)

  
}

# The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getINV()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setINV(m)
    m
  }
