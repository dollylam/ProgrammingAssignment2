## Put comments here that give an overall description of what your
## functions do
## Use a pair of functions to create a special matrix using "makeCacheMatrix" 
## and cache the inverse of that special matrix using "cacheSolve"

## Write a short comment describing this function
## Function "makeCacheMatrix" creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ## Defining i
        set <- function(y) {
                x <<- y ## Creates x
                i <<- NULL ## Clears cache
}
        get <- function() x ## Defines function to compute value of special matrix
        setinverse <- function(inverse) i <<- inverse ## Set inverse of function
        getinverse <- function() i ## Computing inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## Return function
}

## Write a short comment describing this function
## Function "cacheSolve" finds the inverse of the special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse() ## Grabs inverse value that was cached
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
}
        data <- x$get() ## Grabs value of matrix
        i <- solve(data) ## Calculating inverse of matrix
        x$setinverse(i) ## Set inverse of i using cached result
        i ## Return inverse
}
