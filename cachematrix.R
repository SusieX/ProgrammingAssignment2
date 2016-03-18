## Cache the inverse of a matrix

## Create an object that stores the matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inver<-NULL
      set<-function(y){
            x<<-y
            inver<<-NULL
      }
      get <-function() x
      setinver<-function(solve) inver<<-solve
      getinver<-function() inver
      
      list(set=set,get=get,setinver=setinver,getinver=getinver)
}



## Computes the inverse of the special "matrix" returned by makeCacheMatrix; 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inver <- x$getinver()
    if(!is.null(inver)){
          message("getting inverse data")
          return(inver)
    }
    data <- x$get()
    inver <-solve(data,...)
    x$setinver(inver)
    inver
}
