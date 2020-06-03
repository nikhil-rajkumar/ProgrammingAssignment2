## Put comments here that give an overall description of what your
## functions do
## The two functions written below, use the <<- operator 
## this is used to assign a value to an object in an environment 
## different from its current environment. 
## We create a special matrix and cache its inverse. 


## Write a short comment describing this function
## The makeCacheMatrix creates a special "matrix", which 
## sets the value of the matrix, gets the value, 
## sets the value of the inverse, and gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Write a short comment describing this function
## The cacheSolve function computes the inverse of the special "matrix"
## If first checks to see if the inverse has already been computed
## if so, it gets the inverse from the cahce and skips the computation
## else, it proceeds with the computation and sets the value in the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i 
}

