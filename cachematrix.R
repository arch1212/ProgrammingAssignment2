##Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

##The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to

#set the value of the matrix
#get the value of the matrix
#set the value of the matrix inverse
#get the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL             #setting the inverset to null as a placeholder for a future value
  set <- function(y){   #define a function tp set the value of x, matrix to a new matrix y
    x <<- y             #and reset the inverse i to NULL
    i <<- NULL      
  }
  
  get <- function() x   #returns the matrix x
  
  setinverse <- function(solve){  #sets the inverse i to solve
    i <<- solve
  }
  
  getinverse <- function() i    #returns the inverse i 
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  #return a special matrix that consists of all functions just defined
} 


## The following function calculates the inverse of the special "matrix" created with the above function. 
##However, it first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()                   #to check if the inverse has already been calculated
  if(!is.null(i)){                      
    message("getting the cached data")
    return(i)                           #if so, then we return the value from the cache 
  }
  matrix <- x$get()                     #get the value of matrix x, and assign it to variable, matrix
  i <- solve(matrix, ...)               #solve for its inverse, set the inverse using the setinverse function, and return it
  x$setinverse(i)
  i
}

#This is what you get when we use it:
#xcv
#[,1] [,2] [,3]
#[1,]    1    2    3
#[2,]    0    1    4
#[3,]    5    6    0
#> mymatrix <- makeCacheMatrix(xcv)
#> cacheSolve(mymatrix)
#[,1] [,2] [,3]
#[1,]  -24   18    5
#[2,]   20  -15   -4
#[3,]   -5    4    1
#> cacheSolve(mymatrix)
#getting the cached data
#[,1] [,2] [,3]
#[1,]  -24   18    5
#[2,]   20  -15   -4
#[3,]   -5    4    1