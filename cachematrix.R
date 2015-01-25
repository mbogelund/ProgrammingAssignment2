## Put comments here that give an overall description of what your
## functions do

## Function for creating a special matrix object with the ability to cache its inverse,
## and thus save recalculation time, once the inverse is computed.
## Usage
## Define matrix: my_cache_matrix <- makeCacheMatrix(some_matrix)
## Get matrix: my_matrix$get()
## Set inverse: my_matrix$setinverse(inverse_matrix)
## Get inverse: my_matrix$getinverse()


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function for solving and caching the inverse of a special matrix object
## Usage
## Solve for inverse of matrix: cacheSolve(my_matrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

## Example
#the_matrix <- c(4, 3, 3, 2)
#dim(the_matrix) <- c(2, 2)
#the_matrix
#my_matrix <- makeCacheMatrix(the_matrix)
#my_matrix$get()
#cacheSolve(my_matrix)
#my_matrix$getinverse()
