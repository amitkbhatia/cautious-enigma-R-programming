# The first function, makeCacheMatrix  creates a special "Matrix" that can cache its inverse. This function contains the following function
# setmatrix to set the value of the Matrix # getmatrix to get the value of the Matrix # setinverse to inverse the value of the Matrix # getinverse to get inverse value of the Matrix
 
makeCacheMatrix <- function(x = matrix())
 {
# initialize the value of the matrix inverse to NULL
cache <- NULL
# delcare another function setmatrix where the value will be cached in 1. Matrix is created for the first time. 2. changes made to cached matrix
setmatrix <- function(y) 
 {
 x <<- y
# change the value of cache in case the matrix was changed. 
 cache <<- NULL
 }
# gets the value of the inverse
 getmatrix <- function () x
#calculates the inverse of non-singular matrix via the solve function
 setinverse <- function(solve) 
 cache <<- solve
# gets the inverse 
 getinverse <- function() 
 cache 
# passes the value of the function makeCacheMatrix 
 list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
 } 
# used to get the cache of the matrix. This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
 cacheSolve <- function(y, ...) 
 {
 inverse <- y$getinverse()
#if the inverse exists, it gets it.
 if(!is.null(inverse)) {
 message("getting cached data")
 return(inverse)
 }
#if the inverse if not there, first it is calculated and then retrieved.
 data <- y$getmatrix()
 inverse <- solve(data)
 y$setinverse(inverse)
 }
 
# Code Testing -create a *square* matrix (because `solve` only handles square matrices)
 n <- matrix(1:4,2,2)
 h <- makeCacheMatrix(n)
 a <- cacheSolve(h)   
  a
# Expected Result

#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

