# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    #set the value of the matrix
    set <- function(y){ 
        x <<- y
        inv <<- NULL
    }
    #get the value of the matrix
    get <- function() {x}
    #set the value 
    setInverse <- function(inverse) {inv <<- inverse}
    # get the value 
    getInverse <- function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...){
    inv <- x$getInverse()
    #cheking if the valye is null
    if(!is.null(inv)){
        #writing the message
        message("getting cached data")
        return(inv)
    }
    #retrieve the inverse from the cache
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
