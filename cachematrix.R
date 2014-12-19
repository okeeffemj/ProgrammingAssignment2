## These functions 
##    1) store a matrix and caches it's inverse once calculated
##    2) return the inverse
## NB We assume the matrix is always square invertable

## For a matrix passed as a parameter this function creates a list containing :
##      a setter and getter for the orginal values of the matrix 
##      a setter and getter for the inverse of that matrix

makeCacheMatrix <- function(orginalMatrix = matrix()) {
    # create list of setters and getters for matrix and inverse
    invertedMatrix <- NULL
    set <- function(y) {
        orginalMatrix <<- y  # new matrix - overwrite the old one
        invertedMatrix <<- NULL # new matrix - clear inverse
    }
    get <- function() {
        orginalMatrix
    }
    setInverse <- function(inverted) {
        invertedMatrix <<- inverted # this is called to set inverse matrix
    }
    getInverse <- function() {
        invertedMatrix
    }
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## taking as input the output of a makeCacheMatrix this fuction will
## return the matrix representing it's inverse.
## if the inverse has previously been calculated it will use this calculation
## if there is no value cached it will calculate the value and update the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) { # there is a value cached so use it
        message("getting cached inverse data")
        return(inverse)
    }
    originalMatrix <- x$get()
    inverse <- solve(originalMatrix) # solve find the inverse of a matrix
    x$setInverse(inverse)
    inverse
    
}
# test code 
a<- matrix(c(2,2,3,2), nrow=2, ncol =2)
a
cacheMatrix <- makeCacheMatrix(a)
cacheSolve(cacheMatrix)
cacheSolve(cacheMatrix)


b<- matrix(c(1,0,5,2,1,6,3,4,0), nrow=3, ncol =3)
b
cacheMatrix2 <- makeCacheMatrix(b)
cacheSolve(cacheMatrix2)
cacheSolve(cacheMatrix2)

