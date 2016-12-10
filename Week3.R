makeCacheMatrix <- function(x=matrix()){
      in_verse <- NULL
      set <- function(y){
            x <<- y
            in_verse <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) in_verse <<- inverse
      getinverse <-  function() in_verse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse= getinverse)
}


cacheSolve <- function(x, ...) {
      in_verse <- x$getinverse()
      if(!is.null( in_verse)) {
            message("getting cached inverse matrix")
            return( in_verse)
      } 
      else { 
            in_verse <- solve(x$get())
            x$setinverse(in_verse)
            return( in_verse)
      }
}

