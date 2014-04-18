makeCacheMatrix <- function(x = matrix()) {
     m <- NULL  # initialize a container or store for the inverse of particular matrix
     set <- function(y) {   # function to register the matrix in the environment
          x <<- y
          m <<- NULL
     }
     get <- function() x   # function to retrieve a particular matrix stored in environment
     setinverse <- function(inverse) m <<- inverse  # function to register the inverse of the matrix in the environment
     getinverse <- function() m  # function to call the inverse of a matrix from environment registry
     list(set = set, get = get, # output functions
          setinverse = setinverse,
          getinverse = getinverse)
}


cacheSolve <- function(x, ...) {

     m <- x$getinverse() # calling out inverse in the container or store for a cached matrix
     if(!is.null(m)) {     # boolean to check if container is not empty and if true, report "message" and output the value stored
          message("getting cached matrix")
          return(m)
     }
     mymatrix <- x$get()  # if false, retrieve or get the matrix
     m <- solve(mymatrix, ...)  # solve the matrix
     x$setinverse(m)  # register the inverse in the store or container in environment
     m  # output result
}
