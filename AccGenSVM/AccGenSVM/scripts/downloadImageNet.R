library("xlsx")
library("RCurl")
library("plyr")
library("ggplot2")
library("sampling")
library("caret")
library("FNN")

#DOWNLOAD IMAGENET DATA
#setwd("C:/Users/dben652/Documents/DIANA/Data/ILSVRC2010-ImageNet")
setwd("/projects/uoa00440")
synsets<-read.xlsx("AccGenSVM/data/imagenet/synsets.xlsx", 1, header=F)

synsetsComplete <- paste("http://www.image-net.org/api/download/imagenet.sbow.synset?wnid=",synsets$X1, sep="")
write.csv(synsetsComplete,"synsetsComplete.csv", row.names=FALSE, quote = FALSE)

for(i in 1:1)
{
  download.file(synsetsComplete[i],destfile=paste(synsets$X1[i],".mat"),method="libcurl")
}