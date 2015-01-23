## These functions caches processed matrixes and stores them.  Then when 
## an identical matrix is given to these functions the answers are retrieved 
## from cache rather than being recalculated. 

## makeCacheMatrix() a "matrix" object is created that caches the 
##  input matrix and the inverse of the input matrix for use in cacheSolve

makeCacheMatrix <- function(x = mx()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

## cachesolve() determines if the matrix has been previously cached 
## and if so returns the cached solution if not calculates the inverse


cacheSolve <- function(x=mx(), ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        mx<-x$get()
        m<-solve(mx, ...)
        x$setmatrix(m)
        m
}
