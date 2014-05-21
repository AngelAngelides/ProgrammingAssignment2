## I have written two functions that cache the inverse of a matrix, first by creating a special matrix object
## and caching it's inverse and then either calculatinng the inverse of the matrix or retrieving it if it's already
## calculated and stored in the cache

## The first function, makeCacheMatrix creates a special matrix, which is a list containing a function to
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse of the matrix
##    get the value of the inverse of the matrix


makeCacheMatrix<- function(x=matrix()){
 m <- NULL
 set <- function(y){
    x <<- y
 m <<- NULL
 }
 get <- function() x
 setmatrix <- function(solve) m <<- solve
 getmatrix <- function() m
 list(set = set, get = get,
      setmatrix = setmatrix,
      getmatrix = getmatrix)
}



## The following function calculates the inverse of the special matrix created with the above function. 
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via 
## the setmean function.


cachesolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
