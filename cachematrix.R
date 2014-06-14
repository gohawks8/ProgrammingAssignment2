## this function will cache the given matrix if it is not already cached
## and then return the cached matrix
## if it is found, then directly return it 
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


## this function calls getmatrix (recursed) in makeCacheMatrix to check if the matrix has been processed before
## if yes (return is the inversed matrix), return the result returned by getmatrix
## if no, store the inversed matrix and return it

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
