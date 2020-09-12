# This code helps to make a matrix of whatever sorts the person wants
makecacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set, get= get, setInverse = setInverse, getInverse = getInverse)
        
}
#this helps to find the inverse of a matrix
cachesolve <- function(x, ...){
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting Cached Data")
                return(inv)
        }
        #if statement helps to tell whether the inverse of such a matrix was cached or not 
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}