## The outcome of the assignment will be a result of Lexical
##scoping using a pair of function namely "makeCacheMatrix" and "cacheSolve"that will be
##able to cache the inverse of a matrix.

## The  makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

##               **Checking The Function**
## source("ProgrammingAssignment2/cachematrix.R")
## my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## my_matrix$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## my_matrix$getInverse()
## NULL

## cacheSolve(my_matrix)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## cacheSolve(my_matrix)

## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
