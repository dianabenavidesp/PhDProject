KL <- function(matrix, matrixProblem)
{
  options(warn=-1)
  
  max<-min(length(matrix[,1]),length(matrixProblem[,1]))
  
  #computes frequency (probability) of every word on a document (documents sum-up to 1)
  freqs1 <- t(apply(matrix[1:max,], 1, function(x) { x/sum(x)}))
  freqs2 <- t(apply(matrixProblem[1:max,], 1, function(x) { x/sum(x)}))
  
  #the log of freqs1 and freqs2
  LR <- ifelse(freqs1 > 0, log(freqs1/freqs2), 0)
  LR[is.na(LR)] <- 0
  LR[is.infinite(LR)] <- 0
  
  #the log times the probability (numerator of the log), for each probability
  KLDiv <- sum(freqs1*LR)
  
  #for all documents
  KL <- abs(KLDiv / min(length(matrix[,1]),length(matrixProblem[,1])))
  KL
}

KL2<-function(matrix, matrixProblem)
{
  options(warn=-1)
  
  max<-min(length(matrix[,1]),length(matrixProblem[,1]))
  
  freqs1 <- t(apply(matrix[1:max,], 1, function(x) { x/sum(x)}))
  freqs2 <- t(apply(matrixProblem[1:max,], 1, function(x) { x/sum(x)}))
  KL <- KL.plugin(freqs1, freqs2)
  KL
}