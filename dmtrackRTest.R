######
# problem 1

GenSymMatrix <- function(N)
{
  (m <- matrix(runif(N^2), N)) + t(m)
}

CheckSymMatrix <- function(mat)
{
  all(mat - t(mat) < .Machine$double.eps^.5)
}

CheckAntiSymMatrix <- function(mat)
{
  checkSymMatrix(mat[rev(seq_len(nrow(mat))), ])
}

N <- 100
mat1 <- GenSymMatrix(N)
CheckSymMatrix(mat1)
CheckAntiSymMatrix(mat1)
mat2 <- mat1[rev(seq_len(nrow(mat1))), ]
CheckSymMatrix(mat2)
CheckAntiSymMatrix(mat2)

######
# problem 2

MassCenterNum <- function(v)
{
  which.min(sapply(seq_along(v), function(i) abs(sum(v[1:(i-1)]) - sum(v[(i+1):length(v)]))))
}
  
MassCenterStr <- function(s)
{
  massCenterNum(match(unlist(strsplit(tolower(s), "")), letters))
}

v <-  c(5, 2, 4, 8, 3, 1, 1, 6, 2)
MassCenterNum(v)
s <- "HelloWorld"
MassCenterStr(s)