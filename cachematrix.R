## Together, these functions allow the creation of a matrix object that saves
## in cache its own inverse, for quick retrieval without recalculation.

## makeCacheMatrix saves the matrix alongside its inverse in a list of length 4.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y){
      inv<<-NULL
      x<<-y
  }
  get <- function () x
  setinv <- function (y) inv <<- y
  getinv <- function () inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## If its x argument's inverse is not set, cacheSolve function calculates it,
## otherwise it returns it taking it from the cache.
## Includes the option to demonstrate time savings.

cacheSolve <- function(x, time=FALSE, ...) {
        ## Return a matrix that is the inverse of 'x'
  t0 <- proc.time()
  inv <- NULL
  cacheinv <- x$getinv()
  if (!is.null(cacheinv)){
    message('getting cached inverse...')
    if(time==TRUE) print(proc.time()-t0)
    return(cacheinv)
  }
  inv <- solve(x$get())
  x$setinv(inv)
  if(time==TRUE) print(proc.time()-t0)
  inv
}
