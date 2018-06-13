## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
         
      ## Initialize the inverse property
      i <- NULL
      
      
      ## Method to set the matrix
      
      set <- function( matrix ){
            m <<- matrix
            i <<- NULL
      }
      
      ## Method the get the matrix
      get <- function() m
      
      setInverse <- function(inverse){
            i<<-inverse
      }
      
      getInverse <- function() i
      
      
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse
      )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
              ## Return a matrix that is the inverse of 'x'
      m<- x$getInverse()
      
      ## Just return the inverse if its already set
      if( !is.null(m) ){
            message("getting cached data")
            return(m)
      }
      
      ## Get the matrix from our object
      data <- x$get()
      
      ## Calculate the inverse using matrix multiplication
      m <- solve(data) %*% data
      
      ## Set the inverse to the object
      x$setInverse(m)
      
      ## Return the inversed matrix
      m
}
