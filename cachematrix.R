## Cached matrix inverse (solution)
## 
## In order to store a matrix and lazily cache its solution 
## and save subsequent recalculations
##
## `makeCacheMatrix` will create a _special matrix_
## `cacheSolve` will be used to get the solution from 
## the special matrix created by `makeCacheMatrix`


##
## This function takes a matrix and returns a list of functions to:
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the inverse (i.e: solution) of the matrix
##  4. get the inverse (i.e: solution) of the matrix
##
##  It is assumed that the matrix always has a solution
##

makeCacheMatrix <- function(x = matrix()) {
    solution <- NULL
    set <- function(y) {
        x <<- y
        solution <<- NULL
    }
    get <- function() x
    set_solution <- function(newSolution) solution <<- newSolution
    get_solution <- function() solution
    list(set = set, get = get,
         set_solution = set_solution,
         get_solution = get_solution)
}


##
## This function takes a list as returned from makeCacheMatrix
## and retrieve the cached solution of the corresponding matrix.
## In case the solution is not yet cached, it calculates the solution
## stores it as cached solution and returns it
##

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cachedSolution <- x$get_solution()
    if(!is.null(cachedSolution)) {
        message("getting cached solution")
        return(cachedSolution)
    }
    
    data <- x$get()
    computedSolution <- solve(data, ...)
    x$set_solution(computedSolution)
    computedSolution
}