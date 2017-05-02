#GENERATING STATEMENTS FOR TRAINING BASE MODELS USING REGULAR SVM

#10 most related, although I will select 5 of them
predictBaseTransfer<-function(percent)
{
  realPositives<-c(251,145,253,232,96,86,99,179,204,223,183,197,200,233,249)
  
  for(i in 1:length(realPositives))
  {
    predictBaseTransferPositive(realPositives[i], percent)
  }
}

predictBaseTransferPositive<-function(realPositive, percent)
{
  general<-"java -cp libsvm.jar svm_predict -s 0 -t 2 "
  wd<-"/projects/00440/AccGenSVM/data/caltech256/positives/"
  instructions<-as.character()
  
  i<-1
  
  for(j in 1:256)
  {
    negativePositive <- j
    
    for(dataRep in 1:10)
    {
      for(negative in 1:256)
      {
        if(negative!=realPositive)
        {
          instructions[i]<-paste(general,wd,realPositive,"/",percent*100,"percent/data/data",dataRep,"/sampletest",realPositive,"_",negative,".csv ",wd,realPositive,"/",percent*100,"percent/training/data",dataRep,"/samplebasetransfer",realPositive,"_",negativePositive,".csv.model ",wd,realPositive,"/",percent*100,"percent/test/testBase/data",dataRep, "/sampletest",realPositive,"_",negative,"usingbasetransfer",realPositive,"_",negativePositive,".out >> ",wd,realPositive,"/",percent*100,"percent/test/testBase/data",dataRep,"/sampletest",realPositive,"_",negative,"usingbasetransfer",realPositive,"_",negativePositive,".txt",sep="")
          i <- i + 1
        }
      }
    }
  }
  write.table(instructions,paste("C:/Users/dben652/Desktop/testScriptAccGenSVMData.txt",sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE)
  #write.table(instructions,paste(wd,realPositive,"/",percent*100,"percent/test/testAccGenSVM/testScriptAccGenSVMData.txt",sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE)
}
