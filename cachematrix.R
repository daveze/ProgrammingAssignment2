#caching the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  m  <- NULL
  
  set  <- function(y){
    x <<- y
    m <<- NULL 
  }
  get  <- function() x
  setinverse  <- function(inverse) m  <<- inverse
  getinverse  <- function() m
  list(set= set, get = get,        
       setinverse = setinverse,       
       getinverse = getinverse)
  
}



cacheSolve <- function(x, ...) {
  
  ## Return  inverse of matrix X
  m  <- x$getinverse()  
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data  <- x$get()
  m  <- solve(data, ...)
  x$setinverse(m)
  m
}

#test functionality
#a<-matrix(c(4,3,3,2),nrow=2,ncol=2,byrow=T)
#a
#b<-makeCacheMatrix(a)
#cacheSolve(b)
