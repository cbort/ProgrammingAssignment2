## Calculating the inverse of a squared matrix can be a time consuming process.
## When a given matrix inverse is needed repeatedly it is very inefficient to 
## have to calculate the inverse each time it is needed.
## The functions makeCacheMatrix and cacheSolve are used together to create a 
## matrix, which once the inverse has been calculated it is simply stored away
## for rapid retrieval in the future.


## The makeCacheMatrix function serves as a wrapper to the matrix class allowing
## a cached value of the Inverse to be maintained. makeCacheMatrix accepts a
## single argument containing the desired matrix. 
makeCacheMatrix <- function(x = matrix()){
    # creates a variable to store the matrix inverse in
    inv <- NULL
    
    # this function is used to (re)set the value of the matrix if needed
    # additionally it resets the cached inverse anytime the matrix changes
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    # this function returns the matrix described by the instance of 
    # makeCacheMatrix
    get <- function() x
    
    # this function takes in a single argument which is used to set the value of
    # the cached inverse
    setInverse <- function(inverse) inv <<- inverse
    
    # this function returns the value of the cached inverse
    getInverse <- function() inv
    
    # this list is used to associate all functions with the makeCacheMatrix
    # function
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function accepts one makeCacheMatrix as an argument.
## It makes use of the makeCacheMatrix and its functions to
## store and calculate the matrix inverse as needed.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    # Verifies whether the inverse already exists and returns it if it does. 
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    # If the inverse was not previously set the matrix is obtained from within
    # makeCacheMatrix
    data <- x$get()
    # the inverse is calculated
    inv <- solve(data, ...)
    # and the calculated inverse is then cached for future use.
    x$setInverse(inv)
    # lastly the newly calculated inverse matrix is returned.
    inv
}
