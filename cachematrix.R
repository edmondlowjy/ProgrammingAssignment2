## In this assignment, I realized that the nested function 'set' is not invoked and hence
# does not serve any purpose in the code. Thus, I have removed it entirely.



## makeCacheMatrix creates a list that stores the input matrix and corresponding inverse
# if the inverse has been solved once before

makeCacheMatrix <- function(x = matrix()) {
  m.inv <- NULL
  get <- function() x
  setinv <- function(inv) m.inv <<- inv
  getinv <- function() m.inv
  list(get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve modifies the list created by makeCacheMatrix by calculating the inverse of the
# matrix contained within the list and storing the inverse within this list

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m.inv <- x$getinv()
  if(!is.null(m.inv)){
    message("getting cached data")
    return(m.inv)
  }
  mat.data <- x$get()
  m.inv <- solve(mat.data)
  x$setinv(m.inv)
  m.inv
}

## Test below
# First run of cacheSolve(test) should store inverse. Second run should display cache message

mat.test=matrix(c(-1,1,1.5,-1),2,2)
test=makeCacheMatrix(mat.test)
cacheSolve(test)

