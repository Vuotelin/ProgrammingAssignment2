# This function takes in a matrix and performs the following functions:
# 1. sets the matrix
# 2. gets the matrix
# 3. sets the inverse
# 4. gets the inverse
# It is extremely similar to the makeVector function provided in the 
# example, except that this is done to a matrix instead of a vector.

makeCacheMatrix <- function(x = matrix()) {
  #This if statement eliminates all non-matrix objects from consideration,
  #as well as any non-square matrices. We assume that the matrices that
  #passed the filter are invertible for this assignment.
  if(class(x) != "matrix") {
    print("Not a matrix")
  } else if (nrow(x) != ncol(x)) {
    print("Matrix not invertible")
  } else {
    solution <- NULL;
    
    #Caches the original matrix
    set <- function (mat) {
      x <<- mat
      solution <<- NULL
    }
    
    #Gets the cached matrix 
    get <- function() x
    
    #The last two functions make it possible to set and get the inverses
    #to the original matrix.
    setinv <- function(inverse) solution <<- inverse
    getinv <- function() solution
    list(set = set, get = get, setinv = setinv, getinv = getinv)
  }
}

# The point of this function is to take a matrix and invert it,
# returning the result. It is like cacheMean in the example, except 
# we're doing it to a matrix instead of a vector.

cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  
  #If we've already solved the problem, then we should look inside the
  #cache for stored answers, instead of calculating all over again.
  if(!is.null(inverse)){
    message("getting cache")
    inverse
    
    #If we don't have the answer yet, we'll calculate the inverse first.
  } else {
    data <- x$get()
    inverse <- solve(data,...)
    x$setinv(inverse)
    inverse
  }
}
