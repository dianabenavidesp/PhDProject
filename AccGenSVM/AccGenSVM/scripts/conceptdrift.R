#concept drift


#Drift in prior probabilities
#The distribution of P(Y) changes (ex. If, due to a crisis, more articles on dwelling houses come out and fewer articles on holiday homes do but the editor, the writing style, and the interests of the user remain the same, this situation corresponds to drift in prior probabilities of the classes.)
#In this case, change in the original distribution

#Drift in posterior probabilities
#The distribution of P(X given Y) changes
#If, on the other hand, the user has bought a house and starts looking for a holiday destination, dwelling houses become not relevant and holiday homes become relevant. This scenario corresponds to the real concept drift. In this case, the writing style and the prior probabilities stay the same. It may happen that all types of drifts may take place at the same time. 

#Concept drift hyperplane (base)
converthyperplanebase <- function(index, rep)
{
  setwd(paste("C:/Users/dben652/Desktop/Hyperplane/10features/10hyp/data/rep",rep,sep=""))
  data<-read.table(paste("hyperplanebase",index,".arff",sep=""), stringsAsFactors = FALSE)
  data<-lapply(data, FUN = strsplit, ",")
  
  numberOfRows<-1000
  numberOfFeatures<-10
  rows<-NULL
  
  for(i in 1:numberOfRows)
  {
    row <- unlist(data[[1]][i][[1]])
    class <- row[numberOfFeatures+1]
    if(class == "class1") {class <- 1} else{ class <- -1 }
    row <- row[1:numberOfFeatures]
    
    features <- NULL
    
    for(j in 1:numberOfFeatures)
    {
      featureLabel <- paste(j, ":", sep="")
      feature <- paste(featureLabel,row[j],sep="")
      
      features <- as.data.frame(cbind(features, feature))
    }
    features <- cbind(class, features)
    
    rows <- rbind(rows, features)
  }
  
  write.table(rows, file = paste("hyperplanebase",index,".csv", sep="") , sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
}

#Concept drift hyperplane (transfer and test)
converthyperplanetransfer <- function(rep)
{
  setwd(paste("C:/Users/dben652/Desktop/Hyperplane/10features/10hyp/data/rep", rep, sep = ""))
  data<-read.table("hyperplanetransfer.arff", stringsAsFactors = FALSE)
  data<-lapply(data, FUN = strsplit, ",")
  
  numberOfRows<-100
  numberOfFeatures<-10
  rows<-NULL
  
  for(i in 1:numberOfRows)
  {
    row <- unlist(data[[1]][i][[1]])
    class <- row[numberOfFeatures+1]
    if(class == "class1") {class <- 1} else{ class <- -1 }
    row <- row[1:numberOfFeatures]
    
    features <- NULL
    
    for(j in 1:numberOfFeatures)
    {
      featureLabel <- paste(j, ":", sep="")
      feature <- paste(featureLabel,row[j],sep="")
      
      features <- as.data.frame(cbind(features, feature))
    }
    features <- cbind(class, features)
    
    rows <- rbind(rows, features)
  }
  
  write.table(rows, file = paste("hyperplanetransfer",".csv", sep="") , sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
}

#split transfer and test

splittransfertest <- function(rep)
{
  setwd(paste("C:/Users/dben652/Desktop/Hyperplane/10features/10hyp/data/rep",rep, sep=""))
  data<-read.table("hyperplanetransfer.csv")

  test <- sample(1:nrow(data), (nrow(data)*0.3))
  
  training <- data[-test,]
  test <- data[test,]
  
  write.table(test, file = paste("hyperplanetest",".csv", sep="") , sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
  write.table(training, file = paste("hyperplanetransfer",".csv", sep="") , sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
}

#convert hyperplane all, for the meta-svm
converthyperplaneall <- function(rep)
{
  setwd(paste("C:/Users/dben652/Desktop/Hyperplane/10features/10hyp/data/rep",rep,sep=""))
  data<-read.table(paste("hyperplaneall.arff",sep=""), stringsAsFactors = FALSE)
  data<-lapply(data, FUN = strsplit, ",")
  
  numberOfRows<-1000
  numberOfFeatures<-10
  rows<-NULL
  
  for(i in 1:numberOfRows)
  {
    row <- unlist(data[[1]][i][[1]])
    class <- row[numberOfFeatures+1]
    if(class == "class1") {class <- 1} else{ class <- -1 }
    row <- row[1:numberOfFeatures]
    
    features <- NULL
    
    for(j in 1:numberOfFeatures)
    {
      featureLabel <- paste(j, ":", sep="")
      feature <- paste(featureLabel,row[j],sep="")
      
      features <- as.data.frame(cbind(features, feature))
    }
    features <- cbind(class, features)
    
    rows <- rbind(rows, features)
  }
  
  write.table(rows, file = paste("hyperplaneall.csv", sep="") , sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
}




#HYPERSPHERE
converthyperspherebase <- function(index)
{
  setwd("C:/Users/dben652/Desktop/Hypersphere/10features/10hyp/data/rep1/")
  data<-read.table(paste("hyperspherebase",index,".arff",sep=""), stringsAsFactors = FALSE)
  data<-lapply(data, FUN = strsplit, ",")
  
  numberOfRows<-1000
  numberOfFeatures<-10
  rows<-NULL
  
  for(i in 1:numberOfRows)
  {
    row <- unlist(data[[1]][i][[1]])
    class <- row[numberOfFeatures+1]
    if(class == "class1") {class <- 1} else{ class <- -1 }
    row <- row[1:numberOfFeatures]
    
    features <- NULL
    
    for(j in 1:numberOfFeatures)
    {
      featureLabel <- paste(j, ":", sep="")
      feature <- paste(featureLabel,row[j],sep="")
      
      features <- as.data.frame(cbind(features, feature))
    }
    features <- cbind(class, features)
    
    rows <- rbind(rows, features)
  }
  
  write.table(rows, file = paste("hyperspherebase",index,".csv", sep="") , sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
}

#Concept drift hypersphere (transfer and test)
converthyperspheretransfer <- function()
{
  setwd("C:/Users/dben652/Desktop/Hypersphere/10features/10hyp/data/rep1/")
  data<-read.table("hyperspheretransfer.arff", stringsAsFactors = FALSE)
  data<-lapply(data, FUN = strsplit, ",")
  
  numberOfRows<-1000
  numberOfFeatures<-10
  rows<-NULL
  
  for(i in 1:numberOfRows)
  {
    row <- unlist(data[[1]][i][[1]])
    class <- row[numberOfFeatures+1]
    if(class == "class1") {class <- 1} else{ class <- -1 }
    row <- row[1:numberOfFeatures]
    
    features <- NULL
    
    for(j in 1:numberOfFeatures)
    {
      featureLabel <- paste(j, ":", sep="")
      feature <- paste(featureLabel,row[j],sep="")
      
      features <- as.data.frame(cbind(features, feature))
    }
    features <- cbind(class, features)
    
    rows <- rbind(rows, features)
  }
  
  write.table(rows, file = paste("hyperspheretransfer",".csv", sep="") , sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
}

#split transfer and test

splittransfertest <- function()
{
  setwd("C:/Users/dben652/Desktop/Hypersphere/10features/10hyp/data/rep1/")
  data<-read.table("hyperspheretransfer.csv")
  
  test <- sample(1:nrow(data), (nrow(data)*0.3))
  
  training <- data[-test,]
  test <- data[test,]
  
  write.table(test, file = paste("hyperspheretest",".csv", sep="") , sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
  write.table(training, file = paste("hyperspheretransfer",".csv", sep="") , sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
}

#reference
converthyperplanereference <- function()
{
  setwd("C:/Users/dben652/Desktop/Hyperplane/10features/10hyp/data")
  data<-read.table(paste("hyperplanereference.arff",sep=""), stringsAsFactors = FALSE)
  data<-lapply(data, FUN = strsplit, ",")
  
  numberOfRows<-1000
  numberOfFeatures<-10
  rows<-NULL
  
  for(i in 1:numberOfRows)
  {
    row <- unlist(data[[1]][i][[1]])
    class <- row[numberOfFeatures+1]
    if(class == "class1") {class <- 1} else{ class <- -1 }
    row <- row[1:numberOfFeatures]
    
    features <- NULL
    
    for(j in 1:numberOfFeatures)
    {
      featureLabel <- paste(j, ":", sep="")
      feature <- paste(featureLabel,row[j],sep="")
      
      features <- as.data.frame(cbind(features, feature))
    }
    features <- cbind(class, features)
    
    rows <- rbind(rows, features)
  }
  
  write.table(rows, file = paste("hyperplanereference",index,".csv", sep="") , sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
}



#KL
#source
getmatrixP <- function(rep, hyp)
{
  setwd(paste("C:/Users/dben652/Desktop/Hyperplane/10features/10hyp/data/rep",rep, sep =""))
  p<-read.table(paste("hyperplanebase",hyp,".csv", sep = ""), stringsAsFactors =FALSE)
  matrixPPositives <- NULL
  matrixPNegatives <- NULL
  
  for(i in 1:nrow(p))
  {
    firstSplit<-unlist(sapply(p[i,], function(x) {strsplit(as.character(x), split=":")}))
    class <- as.numeric(firstSplit[1])
    
    attbs <- NULL
    attbs <- firstSplit[(seq(3, length(firstSplit), by = 2))]
    
    if(class == -1) { matrixPPositives<-rbind(matrixPPositives, as.numeric(attbs))} else { matrixPNegatives<-rbind(matrixPNegatives, as.numeric(attbs)) }
  }
  
  matrix <- list(matrixPPositives, matrixPNegatives)
}

#transfer
getmatrixT <- function(x)
{
  setwd(paste("C:/Users/dben652/Desktop/Hyperplane/10features/10hyp/data/rep",x, sep =""))
  p<-read.table("hyperplanetransfer.csv", stringsAsFactors =FALSE)
  matrixPPositivesT <- NULL
  matrixPNegativesT <- NULL
  
  for(i in 1:nrow(p))
  {
    firstSplit<-unlist(sapply(p[i,], function(x) {strsplit(as.character(x), split=":")}))
    class <- as.numeric(firstSplit[1])
    
    attbs <- NULL
    attbs <- firstSplit[(seq(3, length(firstSplit), by = 2))]
    
    if(class == -1) { matrixPPositivesT<-rbind(matrixPPositivesT, as.numeric(attbs))} else { matrixPNegativesT<-rbind(matrixPNegativesT, as.numeric(attbs)) }
  }
  
  matrix <- list(matrixPPositivesT, matrixPNegativesT)
}


#reference
getmatrixQ_allones <- function(rows, cols)
{
  # setwd("C:/Users/dben652/Desktop/Hyperplane/10features/10hyp/data")
  # q<-read.table("hyperplanereference.csv", stringsAsFactors =FALSE)
  # matrixQPositives <- NULL
  # matrixQNegatives <- NULL
  # 
  # for(i in 1:nrow(q))
  # {
  #   firstSplit<-unlist(sapply(q[i,], function(x) {strsplit(as.character(x), split=":")}))
  #   class <- as.numeric(firstSplit[1])
  #   
  #   attbs <- NULL
  #   attbs <- firstSplit[(seq(3, length(firstSplit), by = 2))]
  #   
  #   if(class == -1) { matrixQPositives<-rbind(matrixQPositives, as.numeric(attbs))} else { matrixQNegatives<-rbind(matrixQNegatives, as.numeric(attbs)) }
  # }
  
  matrixQPositives <- matrix(1, nrow = rows, ncol = cols)
  matrixQNegatives <- matrix(1, nrow = rows, ncol = cols)
  
  matrix <- list(matrixQPositives, matrixQNegatives)
}

getmatrixQ_gaussian <- function(rows, cols)
{
  gaussian <- NULL
  for(i in 1:cols)
  {
    gaussian_i <- rnorm(rows, 1, 1/3)
    gaussian <- cbind(gaussian, gaussian_i)
  }
  
  gaussian
}

#sources against reference (all ones)
klDivPos <- KL(as.data.frame(p[[1]]), as.data.frame(q[[1]]))
klDivNeg <- KL(as.data.frame(p[[2]]), as.data.frame(q[[2]]))

#sources against reference (gaussian for every X_i)
klDivPos <- KL(as.data.frame(p[[1]]), getmatrixQ_gaussian(500, 10))
klDivNeg <- KL(as.data.frame(p[[2]]), getmatrixQ_gaussian(500, 10))

#Transfer against reference (all ones)
klDivPos <- KL(as.data.frame(t[[1]]), getmatrixQ_gaussian(500, 10))
klDivNeg <- KL(as.data.frame(t[[2]]), getmatrixQ_gaussian(500, 10))

#sources against transfer
klDivPos <- KL(matrixPPositives, matrixPPositivesT)
klDivNeg <- KL(matrixPNegatives, matrixPNegativesT)



#META-SVM (hyperplane)
#combining all data, including classes for posterior control
gethyperplaneall_forcontrol <- function(rep, num_hyp)
{
  setwd(paste("C:/Users/dben652/Desktop/Hyperplane/10features/10hyp/data/rep",rep, sep =""))
  final_p <- NULL
  
  for(hyp in 1:num_hyp)
  {
    p<-read.table(paste("hyperplanebase",hyp,".csv", sep = ""), stringsAsFactors =FALSE)
    p$hyp <- hyp
    
    final_p <- rbind(final_p, p)
  }
  
  write.table(final_p, "hyperplaneall_control.csv",sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
}

gethyperplaneall_fortrain <- function(rep, num_hyp)
{
  setwd(paste("C:/Users/dben652/Desktop/Hyperplane/10features/10hyp/data/rep",rep, sep =""))
  final_p <- NULL
  
  for(hyp in 1:num_hyp)
  {
    p<-read.table(paste("hyperplanebase",hyp,".csv", sep = ""), stringsAsFactors =FALSE)
    final_p <- rbind(final_p, p)
  }
  
  write.table(final_p, "hyperplaneall.csv",sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
}

match_hyperplaneall_train_control <- function(rep)
{
  setwd(paste("C:/Users/dben652/Desktop/Hyperplane/10features/10hyp/data/rep",rep, sep =""))
  train <- read.table(paste("hyperplaneall.csv", sep = ""), stringsAsFactors =FALSE)
  
  control <- read.table(paste("hyperplaneall.csv", sep = ""), stringsAsFactors =FALSE)
}


#META-SVM (caltech256)
getcaltechall_fortrain <- function(rep, related, num_hyp)
{
  setwd(paste("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/related/",related,"/10percent/training/data",rep, sep =""))
  final_p <- NULL
  
  for(hyp in 1:num_hyp)
  {
    if(hyp != related)
    {
      if(file.size(paste(getwd(),"/samplebase",related,"_",hyp,".csv.model", sep = "")) > 0)
      {
        p<-readLines(paste("samplebase",related,"_",hyp,".csv.model", sep = ""))
        final_p <- c(final_p, p[10:length(p)])
      }
    }
  }
  
  writeLines(final_p, "samplebaseall.csv",sep = "\n")
}

getcaltechall_forcontrol <- function(rep, related, num_hyp)
{
  setwd(paste("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/related/",related,"/10percent/training/data",rep, sep =""))
  final_p <- NULL
  
  for(hyp in 1:num_hyp)
  {
    if(hyp != related)
    {
      if(file.size(paste(getwd(),"/samplebase",related,"_",hyp,".csv.model", sep = "")) > 0)
      {
        p <- readLines(paste("samplebase",related,"_",hyp,".csv.model", sep = ""))
        p <- sapply(p, function(x) { paste(x, hyp, sep = "")})
        final_p <- c(final_p, p[10:length(p)])
      }
    }
  }
  
  writeLines(final_p, "samplebasecontrol.csv",sep = "\n")
}

getmatchall <- function(rep, related)
{
  #model
  setwd(paste("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/related/",related,"/10percent/training/data",rep, sep =""))
  model <- readLines(paste("samplebaseall.csv.model", sep = ""))
  svs <- model[10:length(model)]
  svs_split <- sapply(svs, function(x) { strsplit(x, " ") })
  
  #for each SV (add loop!)
  svs <- NULL
  for(i in 1:length(svs_split))
  {
    sv <- svs_split[[1]][2:length(svs_split[[1]])]
    sv <- sapply(sv, function(x) { if(substr(x, nchar(x)-1, nchar(x)-1) == "-" & substr(x, nchar(x), nchar(x)) == "1") { x <- paste(strsplit(x, ":")[[1]][1], ":", "-1.0", sep="") } else if(substr(x, nchar(x)-1, nchar(x)-1) == ":" & substr(x, nchar(x), nchar(x)) == "1") {  x <- paste(strsplit(x, ":")[[1]][1], ":", "1.0", sep="") } else { x <- x} })  
    sv <- unname(sv)
    
    all_sv <- NULL
    for(j in 1:length(sv))
    {
      all_sv <- paste(all_sv, sv[j], sep = " ")
    }
    
    svs <- rbind(svs, all_sv)
  }

  #data
  setwd(paste("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/related/",related,"/10percent/data/data",rep, sep =""))
  data <- readLines(paste("samplebasecontrol.csv", sep = ""))
  
  #SVs
  nchar(svs[1])
  
}


#KL CALTECH AND OTHER DATASETS
#source
getmatrixP <- function(rep, related, hyp)
{
  setwd(paste("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/related/",related,"/10percent/data/data",rep, sep =""))
  p<-read.table(paste("samplebase248_",hyp,".csv.scale", sep = ""), stringsAsFactors =FALSE)
  matrixPPositives <- NULL
  matrixPNegatives <- NULL
  
  for(i in 1:nrow(p))
  {
    firstSplit<-unlist(sapply(p[i,], function(x) {strsplit(as.character(x), split=":")}))
    class <- as.numeric(firstSplit[1])
    
    attbs <- NULL
    attbs <- firstSplit[(seq(3, length(firstSplit), by = 2))]
    
    if(class == -1) { matrixPPositives<-rbind(matrixPPositives, as.numeric(attbs))} else { matrixPNegatives<-rbind(matrixPNegatives, as.numeric(attbs)) }
  }
  
  matrix <- list(matrixPPositives, matrixPNegatives)
}

#transfer
getmatrixT <- function(x)
{
  setwd(paste("C:/Users/dben652/Desktop/Hyperplane/10features/10hyp/data/rep",x, sep =""))
  p<-read.table("hyperplanetransfer.csv", stringsAsFactors =FALSE)
  matrixPPositivesT <- NULL
  matrixPNegativesT <- NULL
  
  for(i in 1:nrow(p))
  {
    firstSplit<-unlist(sapply(p[i,], function(x) {strsplit(as.character(x), split=":")}))
    class <- as.numeric(firstSplit[1])
    
    attbs <- NULL
    attbs <- firstSplit[(seq(3, length(firstSplit), by = 2))]
    
    if(class == -1) { matrixPPositivesT<-rbind(matrixPPositivesT, as.numeric(attbs))} else { matrixPNegativesT<-rbind(matrixPNegativesT, as.numeric(attbs)) }
  }
  
  matrix <- list(matrixPPositivesT, matrixPNegativesT)
}


#reference
getmatrixQ_allones <- function(rows, cols)
{
  # setwd("C:/Users/dben652/Desktop/Hyperplane/10features/10hyp/data")
  # q<-read.table("hyperplanereference.csv", stringsAsFactors =FALSE)
  # matrixQPositives <- NULL
  # matrixQNegatives <- NULL
  # 
  # for(i in 1:nrow(q))
  # {
  #   firstSplit<-unlist(sapply(q[i,], function(x) {strsplit(as.character(x), split=":")}))
  #   class <- as.numeric(firstSplit[1])
  #   
  #   attbs <- NULL
  #   attbs <- firstSplit[(seq(3, length(firstSplit), by = 2))]
  #   
  #   if(class == -1) { matrixQPositives<-rbind(matrixQPositives, as.numeric(attbs))} else { matrixQNegatives<-rbind(matrixQNegatives, as.numeric(attbs)) }
  # }
  
  matrixQPositives <- matrix(1, nrow = rows, ncol = cols)
  matrixQNegatives <- matrix(1, nrow = rows, ncol = cols)
  
  matrix <- list(matrixQPositives, matrixQNegatives)
}

gaussian <- function(rows, cols)
{
  gaussian <- NULL
  for(i in 1:cols)
  {
    gaussian_i <- rnorm(rows, 1, 1/3)
    gaussian <- cbind(gaussian, gaussian_i)
  }
  
  gaussian
}

#sources against reference (all ones)
klDivPos <- KL(as.data.frame(p[[1]]), as.data.frame(q[[1]]))
klDivNeg <- KL(as.data.frame(p[[2]]), as.data.frame(q[[2]]))

#sources against reference (gaussian for every X_i)
klDivPos <- KL(as.data.frame(p[[1]]), getmatrixQ_gaussian(500, 10))
klDivNeg <- KL(as.data.frame(p[[2]]), getmatrixQ_gaussian(500, 10))

#Transfer against reference (all ones)
klDivPos <- KL(as.data.frame(t[[1]]), getmatrixQ_gaussian(500, 10))
klDivNeg <- KL(as.data.frame(t[[2]]), getmatrixQ_gaussian(500, 10))

#sources against transfer
klDivPos <- KL(matrixPPositives, matrixPPositivesT)
klDivNeg <- KL(matrixPNegatives, matrixPNegativesT)