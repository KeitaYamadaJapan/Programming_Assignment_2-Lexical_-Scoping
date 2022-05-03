#
#set the value of the vector
#get the value of the vector
#set the value of the solve
#get the value of the solve#
#
makeCacheMatrix <- function(x = numeric()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function(){return(s)}
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
# 
#
#Function computes the inverse of the special "matrix" returned by makeCacheMatrix above
#
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  return(s)
}
#

