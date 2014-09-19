
##makeCacheMatrix pulls a cached matrix result back and forth. 
##cacheSolve uses the cached result if it exists, otherwise calcs the inverse with Solve. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
 
  getcachedx <- function()  x    # this function returns the value of the original vector
  setcachedxinv <- function(invertedresult){ # called by f below, pushes the inverse up to minv 
      minv<<- invertedresult
      }
  getcachedxinv <- function() minv # pulls the inverse down 

  list(getcachedx = getcachedx,  setcachedxinv = setcachedxinv, getcachedxinv=getcachedxinv)
}


# This function uses Solve to get the matrix inversion, if it needs to.

cacheSolve <- function(x, ...) {
        ##  if a stored (previous) value of x exists, uses its stored inverse, , otherwise
        ##  Returns a matrix that is the inverse of 'x'
  
  m<- x$getcachedx() #access the object x and gets the stored matrix
  if(!is.null(m)) {
        
      message("getting cached inverse")
      return(x$getcachedxinv())
    } 
        
  data<- x$getcachedx()
  minv <- solve(data)
  x$setcachedxinv(minv)
    
  minv
}
