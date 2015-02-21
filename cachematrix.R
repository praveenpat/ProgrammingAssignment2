## The source defines two functions that help you cache the results of solve function used for 
## calculating the inverse of matrix.  It may take too long to compute the inverse of matrix that is big, especially if it has to be computed repeatedly. 
## If the contents of a matrix are not changing, it may make sense to cache the value of the inverse so that when we need it again, it can be looked up in the cache rather than recomputed.

## The first function,  makeCacheMatrix  creates a special "vector", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {

          ## message("entered makeCacheMatrix")

		 inv<- NULL
				 set <- function(y) {
						 x <<- y
						 inv <<- NULL
				 }
         get <- function() x
         setinverse <- function(inverseMatrix) inv <<- inverseMatrix
         getinverse <- function() inv
         list(set = set, get = get,
              setinverse = setinverse ,
              getinverse = getinverse )

}


## The following function calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse has already been calculated.
## If so, it  gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the  setinverse  function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		##message("entered cacheSolve")
		
		 inv <- x$getinverse()

				if(!is.null(inv)){

				   message("getting the cached inverse matrix")

				   return(inv)
				   
				 } 


      data <- x$get()

      inv <- solve(data,...)

      x$setinverse(inv)

      inv

}
