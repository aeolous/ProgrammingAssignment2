## Put comments here that give an overall description of what your
## functions do

#The first function, makeCacheMatrix creates a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the solve of the matrix
#get the solve of the matrix'''
## Write a short comment describing this function 

makeCacheMatrix <- function(x = matrix()) {
              s <- NULL
              set <- function(y) {
                  x <<- y
                  s <<- NULL
              }
              get <- function() x
              setsolve <- function(solve) s <<- solve
              getsolve <- function() s
              list(set = set, get = get,
                   setsolve = setsolve,
                   getsolve = getsolve)
}


## Write a short comment describing this function
#The following function calculates the solve of the special matrix created with the above function.
#However, it first checks to see if the solve has already been calculated. If so, it gets the solve from 
#the cache and skips the computation. Otherwise, it calculates the solve of the matrix and sets the value 
#of the solve in the cache via the setsolve function.'''

cacheSolve <- function(x, ...) {
       s <- x$getsolve() ## Return a matrix that is the inverse of 'x'
       if (!is.null(s)) {
         message('getting cached data')
         return(s)
       }
       matrix <- x$get()
       s <- solve(matrix,...)
       x$setsolve(s)
       s
}
