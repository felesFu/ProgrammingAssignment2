##  ~~~~~~  result ~~~~~~~~~~~~Please look at it~~~~~~~~~
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


## the function is used to cache the processed result
## x is the matrix to be cached

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
## the function is used to check the matrix whether it is be cached or not
## and calculate the inverse of square matrix

cacheSolve <- function(x, ...) {
       
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
