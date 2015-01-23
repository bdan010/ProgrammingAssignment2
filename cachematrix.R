## Function makeCacheMatrix creates an environment that will hold a matrix
## and metadata about the matrix to enable caching functions.
##
## Arguments:
##  x = matrix object that you want to be wrapped
##
## Returns:
##  list of hidden functtions/pointers to the data
##
makeCacheMatrix <- function(x = matrix()) {
    sol <- NULL
    set <- function(y) {
        x <<- y
        sol <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) sol <<- solve
    getsolve <- function() sol
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve) #return object
}


## Function cacheSolve will evaulate the inverse, or solve() function for a
## object defined in the makeCacheMatrix function that is a matrix.
## This function will automatically use the previously cached evaulated
## solve() result when available (meaning this function was run 
## previouisly on the same object).
##
## Arguments:
##  x = special matrix defined by makeCacheMatrix that you want to solve()
##
## Prints "getting cached data" when accessing previously cached data.
##
## Returns:
##  sol - standard matrix() object which is inverse of input
##
cacheSolve <- function(x, ...) {
    sol <- x$getsolve()
    if(!is.null(sol)) {
        message("getting cached data")
        return(sol)
    }
    data <- x$get()
    sol <- solve(data, ...)
    x$setsolve(sol)
    sol ## Return a matrix that is the inverse of 'x'
}
