## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(y)
  {
    x<<-y
    m<<-NULL    
  }
  
  get <- function() x
  setmatrix <- function(mysolve) m <<- mysolve
  getmatrix <- function() m
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # return a matrix that is the inverse of matrix 'x'
  m <- x$getmatrix()
  if (!is.null(m))
  {
    message("found cached matrix inverse..")
    return (m)
  }
  else
  {
    # found cached matrix 
    # get the matrix from cache
    data <- x$get()
    # Inverse it
    m <- solve(data,...)
    x$setmatrix(m)
    m
  }
}
