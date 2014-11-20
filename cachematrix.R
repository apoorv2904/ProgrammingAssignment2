## Put comments here that give an overall description of what your
## functions do

## The first function  makeCacheMatrix takes as input a matrix and converts it to a special "matrix" with useful
## functions. The special "matrix" and its inverse can be stored in cache for faster access.

## The second function  cache takes as input the special "matrix" and if the inverse of the matrix was already computed
## then its retrieved from the cache else it is solved for and is saved in cache for future acccess.

## Write a short comment describing this function

##This first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
        inv_m <- NULL
        set <- function(y) {
                x <<- y
                inv_m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_m <<- inverse
        getinverse <- function() inv_m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
# function name is cacheSolve
# input is special matrix
# output is inverse of special matrix

# The following function finds the inverse of the special "matrix" created with the above function.
# However, it first checks to see if the inverse has already been found. If so, it gets the inverse from the cache
# and skips the computation. Otherwise, it solves for inverse of the matrix data and sets the value of the inverse in 
# the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_m <- x$getinverse()
        if(!is.null(inv_m)) {
                message("getting cached data")
                return(inv_m)
        }
        data <- x$get()
        inv_m <- solve(data)
        x$setinverse(inv_m)
        inv_m
}

