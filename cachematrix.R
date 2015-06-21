##This functions caches the inversion of matrix
## First make the special matrix with the help of makeCacheMatrix function
## then use the cacheSolveFunction to get the inversion of your matrix
## on the way you want to change yor matrix, 
##use the $set function to the object,created by makeCacheMatrix function

## makeCacheMatrix function 
## gets x - the square matrix
## returns the special "matrix"
## where
## $get - function, return the cached matrix
## $set - function, set the square matrix in cache 
## $getSolve - function, return the inverse of the cached matrix if it already was calculated
## $setSolve - function, set the inverse of the matrix in cache

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve function return the inversion of a matrix, created with function makeCacheMatrix
## if it was already calculated, it returns the cached data
## otherwise it calculate it, put it in cache and then return it

cacheSolve <- function(x, ...) 
{
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m      
  
}
