## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function that will create de matrix object that will cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse<- NULL
        
        setMatrix<-function(m){
                x <<- m  
                inverse <<- NULL
        }
        
        getMatrix<-function() x
        
        setInverse<-function(minver){
                inverse<<-minver
        }  
        
        getInverse <- function() inverse
        
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function will return the matrix inverse, if already calculated, or will calculate and cache it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        y<-x$getInverse()
        if(!is.null(y)){
                message("getting cached data")
                return(y)
        }
        
        y<-x$getMatrix()
        i <- solve(y)
        x$setInverse(i)
        i
}
