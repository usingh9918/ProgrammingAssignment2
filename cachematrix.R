##  Hi! Welcome to my submission of Assignment 2: Lexical Scoping 

## makeCacheMatrix function calculates the inverse of a valid square matrix,

## and store the result in the Cache memory.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      
      set <- function(y){
            
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      
      list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function retrieves the stored value of the inverse of a the matrix. If the value

## does not exist, the function calculate the inverse and stores the data in the Cache memory.

cacheSolve <- function(x, ...) {
      
      m <- x$getinverse()
      
      if(!is.null(m)){
            message("getting cached data")
            return(m)
            
            
      }
      data <- x$get()
      
      m<- solve(data,...)
      x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
}
