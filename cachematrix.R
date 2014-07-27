makeCacheMatrix <- function(x = matrix()) {
  
  ## "i" is set to NULL as the "makeCacheMatrix" function is called and run
  i <- NULL
  
  ## When "set" function is run, the matrix is assigned to "x", then "i" is set to NULL
  ## But the use of "<<-" operator means that both objects are assigned outside this environment
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## "get" function simply returns the matrix "x"
  get <- function() x
  
  ## "setinverse" function takes the inversed matrix input and saves(assign) it to "i" 
  setinverse <- function(inverse) i <<- inverse
  
  ## "getinverse" function simply returns the inverse matrix "i"
  getinverse <- function() i
  
  ## This is the backbone of the "makeCacheMatrix" function, creating a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

cacheSolve <- function(x, ...) {
  
  ## "x" variable here is the "makeCacheMatrix" function as shown above
  ## From the list of functions, the following line calls "getinverse" function and 
  ## assigns it to "m"
  i <- x$getinverse()
  
  ## This 'if' clause is run if the "makeCacheMatrix" is run for the first time with 
  ## a given matrix
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  ## Matrix "x" from the "makeCacheMatrix" is acquired and assigned to "matrix"
  matrix <- x$get()
  
  ## "matrix" is solved for its inverse matrix and assigned to "i"
  i <- solve(matrix,...)
  
  ## Before returning the result, the inverse matrix is saved to "i" 
  x$setinverse(i)
  
  ## Returning the inverse matrix
  i
  
}
