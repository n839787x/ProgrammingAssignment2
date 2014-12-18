# This function does the following
# 1. sets the 's' variable to null
# 2. has a 'get' function to get the value of 'x'
# 3. has a 'setinv' function to store the inverse (i.e. solve()) of the matrix
# 4. has a 'getinv' function to returned the cached inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  
  s <- NULL

  get <- function() { x }  # This returns the value of X i.e. what you 
                           # enter into makeCacheMatrix as the matrix
  
  
  setinv <- function(inv) { s <<- inv } # this is to store the inverse 
                                        # of the matrix in x
  
  getinv <- function() { s } # This returns the cached inverse of the
                             # matrix
  
  list (get = get, setinv = setinv, getinv = getinv)
  
}


# This function does the following:
# 1. looks to see if a value is stored in cache
# 2. if a value is found in cache then it is returned to the console
# 3. if the value is not in cache then the solve() function is run
#     and the value is stored in cache
# 4. the inverse matrix value is then displayed or returned

cacheSolve <- function(x, ...) {
  
  s <- x$getinv()   # accesses the object 's' and gets the value of the
                    # stored value of the inverse matrix
  
  if (!is.null(s)) {                    # Check to see if there is a value in s
    message ("getting cached data")    # Print a message if there is a value in s
    return(s)                          # Return the value of s and get out 
  }
  
  data <- x$get()      # if the data is null then go get the data from
                       # makeCacheMatrix
  
  s <- solve(data)     # runs the solver function with the data returned 
                       # from the get function - this is cached data
  
  x$setinv(s)          # store the inverse of the matrix in x
  
  s                    # Return the matric that is the inverse of 'x'
        
}
