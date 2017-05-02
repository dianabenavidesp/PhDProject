getSamplesPositives<-function(data, indexX, percent)
{
  setwd("C:/Users/dben652/workspace/AccGenSVM/data/office/positives")
  #setwd("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/related")
  
  #setwd("/gpfs1m/projects/uoa00440/AccGenSVM/data/caltech256/positives/")
  
  for(k in 1:30)
  {
    iter<-k
    actualSamples<-extractSample(data, indexX, percent)
    #base (not for the classes to be predicted)
    # for(i in 1:255)
    # {
    #   actualSampleRow<-extractSampleRow(actualSamples,i)
    #   actualData<-data[which(data$V1001==indexX),]
    #   actualData<-actualData[actualSampleRow,]
    # 
    #   if(i>=indexX){ indexY<-i+1 } else { indexY<-i }
    #   auxSample<-extractSample(data, indexY, percent)
    #   auxSampleRow<-extractSampleRow(auxSample,sample(1:nrow(auxSample),1))
    #   auxData<-data[which(data$V1001==indexY),]
    #   auxData<-auxData[auxSampleRow,]
    # 
    #   finalData<-rbind(actualData,auxData)
    #   finalData<-getfinaldataCaltech(finalData)
    #   finalData$dd.V1001[finalData$dd.V1001==indexY] <- "-1"
    #   finalData$dd.V1001[finalData$dd.V1001==indexX] <- "1"
    #   write.table(finalData,paste(indexX,"/",percent*100,"percent/data/data",iter,"/","samplebase",indexX,"_",indexY,".csv", sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
    # }
    #transfer
    for(i in 93:184)
    {
      actualSampleRow<-extractSampleRow(actualSamples,i)
      actualData<-data[which(data$V1001==indexX),]
      actualData<-actualData[actualSampleRow,]
      
      if((i-92)>=indexX){ indexY<-(i-92)+1 } else { indexY<-(i-92) }
      auxSample<-extractSample(data, indexY, percent)
      auxSampleRow<-extractSampleRow(auxSample,sample(1:nrow(auxSample),1))
      auxData<-data[which(data$V1001==indexY),]
      auxData<-auxData[auxSampleRow,]
      
      finalData<-rbind(actualData,auxData)
      finalData<-getfinaldataCaltech(finalData)
      finalData$dd.V1001[finalData$dd.V1001==indexY] <- "-1"
      finalData$dd.V1001[finalData$dd.V1001==indexX] <- "1"
      write.table(finalData,paste(indexX,"/",percent*100,"percent/data/data",iter,"/","sampletransfer",indexX,"_",indexY,".csv", sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
    }
  }
  
  #test
  if(percent == 0.10) { percent<-0.20 } else if(percent == 0.20) { percent<-0.30 } else if(percent == 0.30) { percent<-0.50 }
  for(k in 1:30)
  {
    iter<-k
    actualSamples<-extractSample(data, indexX, percent)
    
    for(i in 1:92)
    {
      actualSampleRow<-extractSampleRow(actualSamples,i)
      actualData<-data[which(data$V1001==indexX),]
      actualData<-actualData[actualSampleRow,]
      
      if(i>=indexX){ indexY<-i+1 } else { indexY<-i }
      auxSample<-extractSample(data, indexY, percent)
      auxSampleRow<-extractSampleRow(auxSample,sample(1:nrow(auxSample),1))
      auxData<-data[which(data$V1001==indexY),]
      auxData<-auxData[auxSampleRow,]
      
      finalData<-rbind(actualData,auxData)
      finalData<-getfinaldataCaltech(finalData)
      finalData$dd.V1001[finalData$dd.V1001==indexY] <- "-1"
      finalData$dd.V1001[finalData$dd.V1001==indexX] <- "1"

      if(percent == 0.2)
      {
        write.table(finalData,paste(indexX,"/",0.1*100,"percent/data/data",iter,"/","sampletest",indexX,"_",indexY,".csv", sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
      }
      else if(percent == 0.3)
      {
        write.table(finalData,paste(indexX,"/",0.2*100,"percent/data/data",iter,"/","sampletest",indexX,"_",indexY,".csv", sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
      }
      else if(percent == 0.5)
      {
        write.table(finalData,paste(indexX,"/",0.3*100,"percent/data/data",iter,"/","sampletest",indexX,"_",indexY,".csv", sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
      }
     }
  }
}

getSamplesRelated<-function(data, indexX, percent)
{
  #setwd("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/positives")
  setwd("C:/Users/dben652/workspace/AccGenSVM/data/office/related")
  
  #setwd("/gpfs1m/projects/uoa00440/AccGenSVM/data/caltech256/related/")
  
  for(k in 1:30)
  {
    iter<-k
    actualSamples<-extractSample(data, indexX, percent)
    #base (not for the classes to be predicted)
    for(i in 1:92)
    {
      actualSampleRow<-extractSampleRow(actualSamples,i)
      actualData<-data[which(data$V1001==indexX),]
      actualData<-actualData[actualSampleRow,]

      if(i>=indexX){ indexY<-i+1 } else { indexY<-i }
      auxSample<-extractSample(data, indexY, percent)
      auxSampleRow<-extractSampleRow(auxSample,sample(1:nrow(auxSample),1))
      auxData<-data[which(data$V1001==indexY),]
      auxData<-auxData[auxSampleRow,]

      finalData<-rbind(actualData,auxData)
      finalData<-getfinaldataCaltech(finalData)
      finalData$dd.V1001[finalData$dd.V1001==indexY] <- "-1"
      finalData$dd.V1001[finalData$dd.V1001==indexX] <- "1"
      write.table(finalData,paste(indexX,"/",percent*100,"percent/data/data",iter,"/","samplebase",indexX,"_",indexY,".csv", sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
    }
  #   #transfer not for related classes
  #   for(i in 256:510)
  #   {
  #     actualSampleRow<-extractSampleRow(actualSamples,i)
  #     actualData<-data[which(data$V1001==indexX),]
  #     actualData<-actualData[actualSampleRow,]
  #     
  #     if((i-255)>=indexX){ indexY<-(i-255)+1 } else { indexY<-(i-255) }
  #     auxSample<-extractSample(data, indexY, percent)
  #     auxSampleRow<-extractSampleRow(auxSample,sample(1:nrow(auxSample),1))
  #     auxData<-data[which(data$V1001==indexY),]
  #     auxData<-auxData[auxSampleRow,]
  #     
  #     finalData<-rbind(actualData,auxData)
  #     finalData<-getfinaldataCaltech(finalData)
  #     finalData$dd.V1001[finalData$dd.V1001==indexY] <- "-1"
  #     finalData$dd.V1001[finalData$dd.V1001==indexX] <- "1"
  #     write.table(finalData,paste(indexX,"/",percent*100,"percent/data/data",iter,"/","sampletransfer",indexX,"_",indexY,".csv", sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  #   }
  # }
  # 
  # #test
  # if(percent == 0.10) { percent<-0.20 } else if(percent == 0.20) { percent<-0.30 } else if(percent == 0.30) { percent<-0.50 }
  # for(k in 1:10)
  # {
  #   iter<-k
  #   actualSamples<-extractSample(data, indexX, percent)
  #   
  #   for(i in 1:255)
  #   {
  #     actualSampleRow<-extractSampleRow(actualSamples,i)
  #     actualData<-data[which(data$V1001==indexX),]
  #     actualData<-actualData[actualSampleRow,]
  #     
  #     if(i>=indexX){ indexY<-i+1 } else { indexY<-i }
  #     auxSample<-extractSample(data, indexY, percent)
  #     auxSampleRow<-extractSampleRow(auxSample,sample(1:nrow(auxSample),1))
  #     auxData<-data[which(data$V1001==indexY),]
  #     auxData<-auxData[auxSampleRow,]
  #     
  #     finalData<-rbind(actualData,auxData)
  #     finalData<-getfinaldataCaltech(finalData)
  #     finalData$dd.V1001[finalData$dd.V1001==indexY] <- "-1"
  #     finalData$dd.V1001[finalData$dd.V1001==indexX] <- "1"
  #     
  #     if(percent == 0.2)
  #     {
  #       write.table(finalData,paste(indexX,"/",0.1*100,"percent/data/data",iter,"/","sampletest",indexX,"_",indexY,".csv", sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  #     }
  #     else if(percent == 0.3)
  #     {
  #       write.table(finalData,paste(indexX,"/",0.2*100,"percent/data/data",iter,"/","sampletest",indexX,"_",indexY,".csv", sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  #     }
  #     else if(percent == 0.5)
  #     {
  #       write.table(finalData,paste(indexX,"/",0.3*100,"percent/data/data",iter,"/","sampletest",indexX,"_",indexY,".csv", sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  #     }
  #   }
   }
}