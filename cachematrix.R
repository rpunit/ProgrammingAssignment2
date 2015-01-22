## Pair of functions that caches a inverse of matrix
##


## makeCacheMatrix  will take a matrix as input and create an object
## that can cache its inverse.  
## 


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

# testing with data

x = makeCacheMatrix(matrix(rnorm(25),5,5))
y = cacheSolve(x)
print (x$get())
print (y)

x1 = makeCacheMatrix(matrix(rnorm(4),2,2))
y1 = cacheSolve(x2)
print (x1$get())
print (y1)


x2 = makeCacheMatrix(matrix(rnorm(9),3,3))
y2 = cacheSolve(x2)
print (x2$get())
print (y2)

