## makematrix function creates a matrix and store the value in a list
## functions stores the input matrix ,matrix inverse value assigned from second function

## use this function to store the output list in a variable and use the variable in the second function
makeCacheMatrix <- function(x = numeric()) {
  mi <- NULL
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  get <- function() x
  setmtxinv <- function(mtx_inv) mi <<- mtx_inv
  getmtxinv <- function() mi
  list(set = set, get = get,
       setmtxinv = setmtxinv,
       getmtxinv = getmtxinv)
}


## functions tries to get the stored matrix inverse value ,if it doesn't get it from envirnment variable then
# it calculates matrix incerse and  assigns the value to environment variable


cacheSolve <- function(x, ...) {
  mi <- x$getmtxinv()
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  data <- x$get()
  mi <- solve(data)
  x$setmtxinv(mi)
  mi
}