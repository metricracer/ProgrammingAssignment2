
#################################################
# This function basically saves a special matrix
# object of the matrix after is is inverted.
#################################################
makeCacheMatrix <- function(x = matrix())
{
  # Initialize to null - nothing cached
  #################################################
  inv_matrix_cache <- NULL
  
  # Classic setter - Sets the matrix
  #################################################
  set <- function(x_matrix)
  {
    # message("Setting matrix to cache")
    x <<- x_matrix
    inv_matrix_cache <<- NULL
  }
  
  # Classic getter - Fetches the matrix
  #################################################
  get <- function()
  {
    # message("Fetching original matrix")
    return(x)
  }
  
  #################################################
  set_cache_inv <- function(inv_matrix)
  {
    # message("Caching the inverted matrix")
    inv_matrix_cache <<- inv_matrix
  }
  
  #################################################
  get_cache_inv <- function()
  {
    # message("Fetching cached inverted matrix")
    return(inv_matrix_cache)
  }
  
  # Each argument is a function
  #################################################
  list(set = set,
       get = get,
       set_cache_inv = set_cache_inv,
       get_cache_inv = get_cache_inv)
  
}


#################################################
# This function inverts the given matrix object
# or returns a previously cached version if exists
#################################################
cacheSolve <- function(x, ...)
{
  # Return a matrix that is the inverse of 'x'
  #################################################
  inv_matrix <- x$get_cache_inv()
  if (!is.null(inv_matrix))
  {
    message("Returning the cached solved matrix")
    return(inv_matrix)
  }
  # message("Nothing cached: solving")
  
  # Get the matrix to inverse
  #################################################
  org_matrix <- x$get()
  
  # Inverse the matrix
  #################################################
  # message("Solving matrix")
  inv_matrix <- solve(org_matrix, ...)
  
  # Cache inversed matrix
  #################################################
  # message("Caching solved matrix")
  x$set_cache_inv(inv_matrix)
  
  # Return the inverse of the given matrix x 
  #################################################
  message("Returning solved matrix")
  return(inv_matrix) 
}
