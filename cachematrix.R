## These functions will find the inverse of invertible matrices and store their
## inverses to reduce calculation time in the future.

## makeCacheMatrix returns functions to allow the cacheSolve function to
## access cached inverses of matrices

makeCacheMatrix <- function(x = matrix()) {
  
  #create a variable to store the inverse and initialize it with a NULL
  m <- NULL
  
  #Create a function to store the matrix being inverted
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Create a function to recall the matrix
  get <- function() x
  
  #Create a function that creates the invertible matrix
  setsolve <- function(solve) m <<- solve
  
  #Create a function the calls the inverse matrix
  getsolve <- function() m
  
  #List the functions that can be called for other functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function will either produce the previously calculated inverse of a matrix
## or if it has not yet been calculated, it will find the inverse and produce the inverse

cacheSolve <- function(x, ...) {
  
  #Recall any previous inverse solutions and store them as m
  m <- x$getsolve()
  
  #Return the previously calculated matrix from the cache if its available
  if(!is.null(m)) {
    print("getting cached data")
    return(m)
  }
  
  #Recall the matrix and label it data
  data <- x$get()
  
  #Calculate the inverse of the matrix and store as m
  m <- solve(data, ...)
  
  #Store the inverse of data for future use in the cache
  x$setsolve(m)
  
  #Display the inverse
  m
}
