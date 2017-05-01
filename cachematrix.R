## The makeCacheMatrix creates a "matrix" object that can cache its inverse.
## The cacheSolve computes the inverse matrix created with makeCacheMatrix.
## You can test using the following commands (after setwd to the suitable dir):
##      > source("cachematrix.R")
##      > a<-matrix(c(3,2,0,0,0,1,2,-2,1),3,3)
##      > A<-makeCacheMatrix(a)
##      > cacheSolve(A)
##      > cacheSolve(A)   #Should indicate "getting cached data"

## This function creates a special "matrix" object that can cache its inverse.
#' Make Cache Matrix
#'
#' @param x : The matrix to be stored in cache
#'
#' @return : The instantiated object of cached matrix
#' @export
#'
#' @examples : > h<-makeCacheMatrix(matrix(c(3,2,0,0,0,1,2,-2,1),3,3))
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function()
        x
    setinverse <- function(inverse)
        m <<- inverse
    getinverse <- function()
        m
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


#This function computes the inverse of the special "matrix" returned by
#makeCacheMatrix above. If the inverse has already been calculated
#(and the matrix has not changed), then the cachesolve should retrieve
#the inverse from the cache.
#' Solve cached matrix into the inverse of the CacheMatrix
#'
#' @param x : The CacheMatrix object
#' @param ... 
#'
#' @return : The inverse matrix or the cached inverse matrix if already solved
#' @export
#'
#' @examples : > cacheSolve(h)
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    message("matrix inversion operation")
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
