## The following functions allows you to catch the inverse of a matrix
## if it was calculated before instead of doing the calculation everytime

## Creates an object with setters and getters for a matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  s<-NULL
  set <-function(y){
    x<<-y
    s<<-NULL
  }
  get<-function() x
  setInverse<-function(xinverse) s <<- xinverse
  getInverse<-function() s
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns the inverse of a matrix, if teh inverse was calculated before then
## it returns the cached date instead of computing the calculation again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getInverse()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  matrixdata <- x$get()
  s <- solve(matrixdata,...)
  x$setInverse(s)
  s
}
