## Put comments here that give an overall description of what your
## functions do


##   ----------------------------------
## Example Make Vector Code for reference
## makeVector <- function(x = numeric()) {
##  m <- NULL
##  set <- function(y) {
##    x <<- y
##    m <<- NULL
##  }
##  get <- function() x
##  setmean <- function(mean) m <<- mean
##  getmean <- function() m
##  list(set = set, get = get,
##       setmean = setmean,
##       getmean = getmean)
##}

## cachemean <- function(x, ...) {
##  m <- x$getmean()
##  if(!is.null(m)) {
##    message("getting cached data")
##    return(m)
##  }
##  data <- x$get()
##  m <- mean(data, ...)
##  x$setmean(m)
##  m
##}
##   ----------------------------------


## Write a short comment describing this function
##This function takes a matrix and creates and inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  iMatrix <- NULL  ## create empty matrix 
  set <- function(y){
    x <<- y  ## Superassignement of x to the value of y
    iMatrix <<- NULL ## superassignment of iMatrix to NULL
  }
  get <- function() x
  setiMatrix = function(inverse) iMatrix <<- inverse
  getiMatrix = function() iMatrix
  list(set = set, get = get, setiMatrix = setiMatrix, getiMatrix = getiMatrix) ## create inverse of the passed in matrix
}


## Write a short comment describing this function
## This function checks to see if the inversed matrix has been cached, if it has the cached matrix will be returned if it has not then the matrix will be inversed and returned

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  iMatrix = x$getiMatrix()
  if(!is.null(iMatrix)){   ## Check to see in the inverted matrix has been cached
    message("getting cached data")
    return(iMatrix)  ## Return the cached results
  }
  data = x$get()
  iMatrix = solve(data, ...) ##Invert the matrix
  x$setiMatrix(iMatrix)
  return(iMatrix) ## return the results
}

