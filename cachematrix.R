## Assigment 2: Lexical Scoping
## Using the <<- operator to assign a value to an object
## matrix must be fed as an object for function to work!!

## 1.  `makeCacheMatrix`: This function creates a special "matrix" object
#that can cache its inverse.

makeCacheMatrix <- function (x = matrix()) {
  #set value of the matrix 
  inverse <- NULL 
  # set function
  set <- function(mat){
    
    #get value of the vector
    x   <<- mat   #set value of matrix & set value to cache
    inverse <<- NULL #assume matrix provided are allways square & invertible 
  }
  #set & get value of the matrix
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

#2.  `cacheSolve`: This function computes the inverse of the special
#"matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated (and the matrix has not changed), then
#`cacheSolve` should retrieve the inverse from the cache.
## The following function calculates the inverse of a matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
