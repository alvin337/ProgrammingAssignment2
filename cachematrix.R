## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
        m<-NULL
        
        ## set the matrix
        set<-function(y)
        {
                x<<-y
                m<<-NULL
        }

        ## get the matrix
        get<-function() x
        
        ## inverse the matrix
        setMatrix<-function(inverse) m<<- inverse
        
        ## get the inversed matrix
        getMatrix<-function() m
        
        ## return the results
        list(set=set, get=get, setMatrix=setMatrix, getMatrix=getMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMatrix()
        
        ## return the results if its set-ed
        if(!is.null(m)) 
        {
                message("getting cached data")
                return(m)
        }
        
        ## get the matrix
        matrix <- x$get()
        
        ##
        m <- solve(matrix, ...)
        
        ## set matrix invserse
        x$setMatrix(m)
        
        ## return the results
        m
}
