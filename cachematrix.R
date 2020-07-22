## The first function is to create a "special matrix", which is a list with four important functions,
## and the second function is to return the inverse matrix, but it will first check if the inverse of the matrix
## has already been calculated so in that way it saves computational work.

## This function will return a list with 4 functions, one to set the matrix, other for getting it, 
## other to set the inverse matrix, and a last one to get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse matrix of the "special matrix" created with the function above.
## It will check if the "special matrix" already has its inverse calculated by calling the fourth
## element of the "special matrix" (a function to get the value of the inverse of the matrix). If it isnt
## NULL then it will just return the value that the fourth function of the list returns. If it is NULL,
## then this function will calculate it and set it in the environment of the "special matrix", using its
## third element (a function to set the value of the inverse f the matrix), and also return it.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m    ## Return a matrix that is the inverse of 'x'
}
