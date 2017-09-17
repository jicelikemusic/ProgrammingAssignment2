#Matrix inversion is usually a costly computation and there may be some benefit
#to caching the inverse of a matrix rather than computing it repeatedly.
#Here I am going to write a pair of functions that cache the inverse of a matrix.

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <- function(inv){
        inverse <<- inv
    }
    getInverse <- function(){
        inverse
    }
    return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if (!is.null(inverse)){
        message("This is a cached matrix")
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    return(inverse)
}
