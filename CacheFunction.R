## This overall function computed the inverse of a matrix
## If the inverse has already been calculated, it gets the inverse
## From the cache

## Here we are writing a function that will calculate the inverse of the function

makeCacheMatrix <- function (x = matrix ()) {
  inv <- NULL
  set <- function(y){
    x <<-y
    inv <<-NULL
  }
  get <- function()x
  setinv <- function(solve) inv <<-solve
  getinv <- function()m
  list (set=set, get=get, sitinv=setinv, getinv=getinv)
}

## If the inverse is null, means the inverse has already been calculated in the cache
## So the first part of the function retrieves this
cacheMatrix <- function (x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
## If the inverse has not already been calculated, solve the inverse from the data
  data <- x$get()
  inv <- solve(data,...)
  x$setmean(inv)
  inv
}

## Returns a matrix that is the inverse of X