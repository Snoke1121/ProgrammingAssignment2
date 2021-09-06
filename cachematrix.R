## These functions cache the inverse of the matrix


## makeCacheMatrix creates special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## the inverse property is initialized with inverse
        I <- NULL
        ##function is used to set the matrix
        set <- function(y){
                xx <<- y
                I <<- NULL
                }
        ##function to get the matrix
        get <- function(){
                x
                }
        ## function to set the inverse of the matrix
        Inverse_set <- function(inverse) {
                I <<- inverse
                }
        Inverse_get <-function(){
                I
                }
        ##list to store the functions
        list(
                set = set, get = get,
                Inverse_set = Inverse_set,
                Inverse_get = Inverse_get
                )

}


## Calculates the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## return the inverse of matrix x
        I <- x$Inverse_get()
        ## check if its already computed, if yes then return the inverse
  if (!is.null(I)) {
          message("getting cached data")
          return(I)
  }
        
  data <- x$get()
  ##calculate the inverse
  I <- solve(data, ...)
  x$Inverse_set(I)
  ## return inverse
  I
}
