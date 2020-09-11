

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
