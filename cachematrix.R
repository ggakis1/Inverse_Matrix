makeCacheMatrix <- function(x=matrix()) {
  matrix_inv <- NULL
  
  set <- function(y) {
    x <<- y
    matrix_inv <<- NULL
  }
  
  get <- function() x
  
  setcacheSolve <- function(solve) matrix_inv <<- solve
  getcacheSolve <- function() matrix_inv
  
  list(set=set, get=get, 
       setcacheSolve=setcacheSolve, 
       getcacheSolve=getcacheSolve)
}

cacheSolve <- function(x, ...) {
  matrix_inv <- x$getcacheSolve()  
  
  if (!is.null(matrix_inv)) {  
    message("getting cached data")
    return(matrix_inv)
  }
  
  data <- x$get()  
  matrix_inv <- solve(data, ...)  
  x$setcacheSolve(matrix_inv)  
  
  matrix_inv
}

