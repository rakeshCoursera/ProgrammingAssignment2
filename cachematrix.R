#Assignment : Caching the Inverse of a Matrix

#An an overall description of functions --

#The first function "makeCacheMatrix" create the a matrix 
#which is really a list containing the a function to
# 1. Set the value of squre invertible matrix
# 2. Get the value of squre invertible matrix
# 3. Set the inverse of a squre invertible matrix
# 4. get the inverse of a squre invertible matrix
#This is defination of makeCacheMatrix function 
#which takes input as a squre invertible matrix.

# The second function "cacheSolve"calculate the inverse of square invertible matrix.
# It first checks to see if the inverse has already been calculated or not.
# if calculated then it gets the inverse of thet matrix from the cache and skip the computation
# Otherwise it calculates the inverse of given matrix and sets the inverse in the cache via the setmean function 

makeCacheMatrix <- function(x = matrix()) {
    #The value of m is assigned as null each time when this function run
    m <- NULL  
  
    #This function set the value to an object x by using y in an environment 
    #that is different from the current environment. 
    set <- function(y) {
        x <<- y          
        m <<- NULL
    }
  
    #function to get value of x
    get <- function() x
  
    #this function actualy work after one run of both "makeChacheMatrix" and "cachesolve"
    # set the value of m in "Cachesolve" fuction when it called
    setinverse <- function(inverse) m <<- inverse
  
    #this fuction get the value of m when it called.
    getinverse <- function() m
  
    #this will create the list of mention function so that it can be called in next
    # "cachesolve" function. Without it indexing of those function is not possible.
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
  
}


#definning the function for value pf x(which is a list type object) with ... (three dot) 
#to allow additional arguments or allows variability in the arguments 
cacheSolve <- function(x, ...) {
    # Call the fuction 'getinverse' from "makeCacheMatrix" function and set the value of m
    m <- x$getinverse()
  
    #check that m is null or not, if not the condition execute
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #Called get() function form the "makeCacheMatrix" function and get the value of x and set into 'data' object
    data <- x$get()
  
    #This calculate the inverse matrix of data or we can say the inverses of input matrix x
    # and assiged into the object m
    m <- solve(data, ...)
  
    #this called function takes the inverse matrix 'm' as input and do assiged in "MakeCacheMatrix"
    x$setinverse(m)
  
    #return the value of m i.e. inverse matrix
    m
}
