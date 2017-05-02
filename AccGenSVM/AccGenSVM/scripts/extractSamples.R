#SAMPLING FUNCTIONS
extractSample<-function(data, class, percentage)
{
  a<-sampling(data[which(data$V1001==class),],class, percentage)
  a
}

sampling<-function(dataX, indexX, percentage)
{
  if(nrow(dataX)*percentage >=2)
  {
    samp<-t(replicate(92*2,(sample(1:nrow(dataX),nrow(dataX)*percentage))))
  }
  else
  {
    samp<-as.matrix(replicate(92*2,(sample(1:nrow(dataX),1))))
  }
  
  samp
}

extractSampleRow<-function(samp, row)
{
  samp[row,]
}