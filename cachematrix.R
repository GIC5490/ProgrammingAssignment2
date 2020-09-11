## creating two functions makeCacheMatrix and cachesolve which consist of "set" and "get" 

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}
  setInverse<-function(inverse) {inv<<-inverse}
  getInverse<-function(){inv}
  list(set=set, get=get, setInverse=setInverse)
}


# This function calculates the inverse of the "matrix" created by the first function. It will check to see if the inverse of the matrix has already been calculated.
# If so, it will retrieve the inverse from the cache and skip the computation. Otherwise, it will calculate the inverse and produce it by itself via the setinverse function

cacheSolve <- function(x, ...) {
        inv<-x$getInverse()
        if(!is.null(inv)){
          message("from cached data")
          return
        }
        mat<-x$get()
        inv<-solve(mat,...)
        x$setInverse(inv)
        inv
    }
