## Put comments here that give an overall description of what your
## functions do
## overall creating and storing a matrix and the second functions
## takes the stored matrix then inverse it.

## Write a short comment describing this function
## makeCacheMatrix stores matrix
makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL ##matrix
  set <- function(y){   ##set the value of matrix
    x <<-y  ##assign operator
    mat <<- NULL
  }
  get <- function(){    ##get the value of matrix
    x
  }
  setInverse <- function(inverse){  ##set the value of inverse
    mat <<- inverse
  }
  getInverse <- function(){   #get the value of inverse
    mat
  }
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
## cacheSolve takes the stored matrix and returns the inverse version of the
## matrix. returns the same matrix if already inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getInverse()
        if(!is.null(mat)){  #check if already inverse
          return(mat)
        }
        newMat <- x$get()
        mat <- solve(newMat, ...)
        x$setInverse(mat)
        mat
}
