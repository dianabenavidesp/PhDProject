fnn <- function(matrix, matrixTransfer, knn, algorithm)
{
	library("FNN")
	get.knnx(matrix, matrixTransfer, k = knn, algorithm = algorithm)
}