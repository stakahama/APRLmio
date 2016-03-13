
################################################################################
##
## file: R_average.R
## author: S. Takahama (satoshi.takahama@epfl.ch)
## date: May 2015
## prepared for Radon project
##
################################################################################

###_* function definitions

Weighted.average <- function(start1, end1, start2, end2, X) {
  ## INPUTS:
  ##   start1 = numeric vector (m x 1)
  ##   end1 = numeric vector (m x 1)
  ##   start2 = numeric vector (n x 1)
  ##   end2 = numeric vector (n x 1)
  ##   X = matrix-like object (2-D, n x k)
  ## OUTPUT:
  ##   matrix (m x k)
  
  ##
  ## This function averages measurements X collected at time resolution
  ##    (start2, end2) to (start1, end1)

  ## convert to matrix
  if(!is.matrix(X))
    X <- data.matrix(X)

  ## create emtpy matrix and vector
  out <- matrix(NA, nrow=length(start1), ncol=ncol(X),
                dimnames=list(NULL, colnames(X)))
  frac <- vector("numeric",length(start1))

  ## loop
  for(i in seq_along(start1)) {
    
    ## find matches
    j <- (end2 > start1[i] & start2 < end1[i])
    
    if( any(j) ) {

      ## find durations according to three types
      dur <- apply(cbind(end2[j] - start1[i],
                         end2[j] - start2[j],
                         end1[i] - start2[j]),
                   1,min,na.rm=TRUE)

      ## find total duration of overlap
      tot <- sum(dur,na.rm=TRUE)
      
      ## weighted sum
      out[i,] <- colSums(X[j,,drop=FALSE]*dur,na.rm=TRUE)/tot
      frac[i] <- tot/(end1[i]-start1[i])
      
    }

  }

  ## return 
  attr(out,"frac") <- frac
  out
}

Reader <- function(txt,seprow=SEPROW,sepcol=SEPCOL) {
  ## tc <- textConnection(do.call(paste,c(strsplit(txt,seprow),list(collapse="\n"))))
  tc <- textConnection(gsub(SEPROW,"\n",txt,fixed=TRUE))
  out <- as.matrix(read.table(tc,sep=sepcol,colClasses="numeric"))
  close(tc)
  out
}

###_* Parse arguments

SEPROW <- "x"
SEPCOL <- ","

argv <- commandArgs(TRUE)

start1 <- Reader(argv[1])
end1 <- Reader(argv[2])
start2 <- Reader(argv[3])
end2 <- Reader(argv[4])
datamatrix <- Reader(argv[5])

###_* Apply functions

averagedmatrix <- Weighted.average(start1, end1, start2, end2, datamatrix)

###_* Export results

write.table(averagedmatrix, file="",na="NaN",
            sep=SEPCOL, row.names=FALSE, col.names=FALSE)
