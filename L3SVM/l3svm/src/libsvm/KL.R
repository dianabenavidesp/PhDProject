KL <- function(matrix, matrixProblem)
{
  options(warn=-1)
  
  max<-min(length(matrix[,1]),length(matrixProblem[,1]))
  
  freqs1 <- t(apply(matrix[1:max,], 1, function(x) { x/sum(x)}))
  freqs2 <- t(apply(matrixProblem[1:max,], 1, function(x) { x/sum(x)}))
  KL <- KL.plugin(freqs1, freqs2)
  KL
}
