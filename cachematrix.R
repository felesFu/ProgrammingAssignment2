##  ~~~~~~ result~~~~~~~~~~~~~~~~~~~~~
## > m1 <- matrix(2:5, 2, 2)
## > cachedm1 <- makeCacheMatrix(m1)
## > cacheSolve(cachedm1)
## [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1
## > cacheSolve(cachedm1)
## geting cached data
## [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1
## ~~~~~~~~~~~~~ END ~~~~~~~~~~~~~~~~~


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function()x
  
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}


## inverse of SQUARE matrix
## (the function is only for square matrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmean()
  if(!is.null(m)){
    message("geting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setmean(m)
  m 
}
