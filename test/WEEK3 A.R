#Week3 Assignment Example: Caching the Mean of a Vector
#makeVector <- function(x = numeric()){
#     m <- NULL
#     set <- function(y){
#         x <<- y
#         m <<- NULL
#     }
#     get <- function() x
#     setmean <- function(mean) m <<- mean
#     getmean <- function() m
#     list(set = set, get = get,
#          setmean = setmean,
#          getmean = getmean)
# }
# 
# cachemean <- function(x, ...){
#     m <- x$getmean()
#     if(!is.null(m)){
#         message("getting cached data")
#         return(m)
#     }
#     data <- x$get()
#     m <- mean(data, ...)
#     x$setmean(m)
#     m
# }

##Assignment 
## Put comments here that give an overall description of what your functions do:
## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL                                                  ##The mi will cache matrix inverse value, initially null
    set <- function(y){
        x <<- y 
        mi <<- NULL                                             ##After we set the y(the input matrix), we will get x=y and mi=null
    }
    get <- function() x                                         ##Give the martrix argument x to the get function
    setmi <- function(inverse) mi <<- inverse                   ##Calculate the inverse value and put it into mi
    getmi <- function() mi                                      ##Get mi value
    list(set = set, get = get, setmi = setmi, getmi = getmi)    ##Set the list for the next step
    }


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mi <- x$getmi()
    if(!is.null(mi)){
        message("getting cached data")
        return(mi)
    }
    data <- x$get()
    mi <- slove(data, ...)
    x$setmi(mi)
    mi
}

