## These functions create a special matrix that can cache its 
## inverse and retrieve the same

## The first function, makeCacheMatrix creates a special matrix
## object that can cache the input matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <- y
    inverse <- NULL
  }
  get <- function() x
  setinverse <- function(matinv) inverse <<- matinv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The second function cacheSolve calls functions stored 
## in the matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated with matrix not changing
## cacheSolve retrieves the inverse from the cache. If the input is new,
## it calculates the inverse of the data and sets the inverse in the
## cache via the setinverse function.




cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return (inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
