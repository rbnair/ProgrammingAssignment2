# RBN code start - Dec 19, 2014
# Takes a matrix and calculates its inverse
# If inverse has already been calculated and stored before, then retrieves the cached value
# Otherwise, calculates afresh 


# Part 1 - makeCacheMatrix - calling wrapper function

makeCacheMatrix <- function(x = matrix()) {   # function defined
  m <- NULL                                   # initial assignment of NULL to m
  
  set <- function(y) {                        # structure similar to the getmean example 
    x <<- y                                   # 
    m <<- NULL}                               # 
  
  get <- function() x                         # get, set, getinverse & setinverse
  setinverse <- function(inv) m <<- inv       # 
  getinverse <- function() m                  # 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# Part 2 - cacheSolve - calculates / rreturns the inverse

cacheSolve <- function(x,...) {                   # 
  m <- x$getinverse()                             # 
  if(!is.null(m)) {                               # if cached value exists ...
    message("getting cached data")                # then print message ...
    return(m)                                     # and retrieve the value of matrix inverse
  }
  data <- x$get()                                 # otherwise, ...
  m <- solve(data, ...)                           # use solve function to get the inverse of the square matrix
  x$setinverse(m)                                 # and pass that value to setinverse
  m                                               # 
}

