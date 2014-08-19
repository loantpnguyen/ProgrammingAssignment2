## makeCacheMatrix creates a special "matrix",which is really a list containing a function to



makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  ## 1.set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## 2.get the value of the matrix
  get <- function() x
  ## 3.set the value of the inverse matrix
  setinverse <- function(inverse) inverseMatrix <<- inverse
  ## 4.get the value of the inverse matrix
  
  getinverse <- function() inverseMatrix
  list(set=set,get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function calculates the inverse of the special "matrix" created with the above function

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getinverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- data %*% solve(data) 
  x$setinverse(inverseMatrix)
  inverseMatrix
        ## Return a matrix that is the inverse of 'x'
}
