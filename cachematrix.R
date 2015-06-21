## Put comments here that give an overall description of what your
## functions do

# execution example
# a <- matrix(rnorm(9), 3)
# b <- makeCacheMatrix(a)
# cacheSolve(b)

## Write a short comment describing this function
# The function 'makeCacheMatrix' 
# 1. initialzie/set variables (input, output).
# 2. define functions in the list (set, get, setInvMatrix, getInvMatrix )
#  - Using set function, 
#    a new matrix can be set in the same environment. 
#     ex: c <- matrix(rnorm(9), 3) 
#         b$set(c)
#  - get, getInvMatrix : call current stored matrix 

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # define functions 
    get <- function() x
    # im : inverse matrix, 
    # if im exist, set inverse matrix with im with re-calculating
    setInvMatrix <- function(im) m <<- im
    getInvMatrix <- function() m
    list(set = set, get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}


## Write a short comment describing this function
# The function 'cacheSolve'  
# 1. tests if 'inverse matrix (IM)' is stored in cache memory. 
# 2. If 'inverse matrix' is stored, it calls the result from the memory.
# 3. If not, call 'solve' function to calculate inverse matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInvMatrix()
    
    # If IM is stored in the memory 'm'
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    
    # If IM is not stored
    # 1. tests if it is invertible by checking determinant of the matrix.
    # 2. if it is invertible, calculate IM with 'solve' function.
    if(det(data) != 0) {
        m <- solve(data, ...)
    } else {
        message("This matrix is not invertible.")
    }
    
    # set m as an interse matrix 
    x$setInvMatrix(m)
    m
}
