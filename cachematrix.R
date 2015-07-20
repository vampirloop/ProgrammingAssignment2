## Below you can find two functions for solving the inverse of a matrix by caching 
## results to improve the subsequent executions.

## First function defines a function's list in order to evaluate and setting variables.
## 'set' it's setting initial values in environment with <<-
## 'get' takes user's input.
## 'setinv' defines the functionality and assign in environment.
## 'getinv' get the value of inverse of matrix.
makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
        get <- function() x
        setinv <- function(solve) mat <<- solve ## Please note condition det(mat) != 0 
        getinv <- function() mat                ## is required to run (square matrix assumed).
        list(set = set , get = get , setinv =setinv, getinv = getinv)
}

## Second function check for stored inverted matrix or evaluates and stores it, if not stored
## previously and returns the value of the inverted matrix.
cacheSolve <- function(x, ...) {
        mat <- x$getinv()
        if(!is.null(mat)) {
                message("getting cached data")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data, ...)## Please note condition det(mat) != 0 is required to run
        x$setinv(mat)          ## (square matrix assumed) not implemented in this script.
        mat
}

## USAGE: 
## 1. source('~/yourwd/cachematrix.R') load R script: Please note your working directory.
## 2. Define your matrix and / or assign to a variable (ie. mtrx <- matrix(1:9, 3, 3)).
## 3. Chech if your matrix is valid (ie. det(mtrx) != 0 gives [1] TRUE).
## 4. yourlist <- makeCacheMatrix(mtrx) create your functions' list and assign to a variable.
## 5. mtrx$set() create your matrix in the function environment.
## 6. cacheSolve(mtrx) 1st returns inverted matrix and stores in working environment.
## 7. cacheSolve(mtrx) 2nd returns stored inverted matrix.
