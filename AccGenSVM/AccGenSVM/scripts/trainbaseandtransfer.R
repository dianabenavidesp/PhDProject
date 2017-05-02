#GENERATING STATEMENTS FOR TRAINING BASE MODELS USING REGULAR SVM

#10 most related, although I will select 5 of them
trainBaseTransfer<-function(percent)
{
    realPositives<-c(251,145,253,232,96,86,99,179,204,223,183,197,200,233,249)
  
    related251<-c(248, 255, 244, 249, 243, 242, 254, 238, 246, 247)
    related145<-c(146, 137, 139, 141, 143, 157, 150, 156, 148, 155)
    related253<-c(254, 251, 248, 255, 249, 243, 256, 242, 246, 244)
    related232<-c(238, 227, 231, 226, 235, 229, 243, 224, 234, 221)
    related96<-c(95, 92, 94, 90, 99, 105, 93, 104, 87, 103)
    related86<-c(94, 78, 87, 79, 88, 95, 77, 83, 91, 81)
    related99<-c(94, 95, 104, 96, 101, 97, 108, 105, 91, 103)
    related179<-c(182, 174, 175, 186, 173, 183, 176, 177, 189, 178)
    related204<-c(205, 206, 208, 196, 203, 211, 202, 213, 195, 193)
    related223<-c(221, 216, 226, 227, 219, 224, 232, 225, 217, 213)
    related183<-c(182, 174, 175, 177, 186, 189, 178, 185, 176, 193)
    related197<-c(195, 196, 192, 193, 203, 189, 199, 191, 190, 202)
    related200<-c(195, 196, 203, 197, 205, 208, 202, 193, 199, 192, 189)
    related233<-c(238, 234, 231, 232, 227, 235, 243, 226, 242, 237)
    related249<-c(248, 255, 251, 243, 244, 242, 238, 247, 254, 246)
    
    for(i in 1:length(realPositives))
    {
      if(realPositives[i] == 251){ trainBaseTransferTrain(0.10, realPositives[i], related251) }
      if(realPositives[i] == 145){ trainBaseTransferTrain(0.10, realPositives[i], related251) }
      if(realPositives[i] == 253){ trainBaseTransferTrain(0.10, realPositives[i], related251) }
      if(realPositives[i] == 232){ trainBaseTransferTrain(0.10, realPositives[i], related251) }
      if(realPositives[i] == 96){ trainBaseTransferTrain(0.10, realPositives[i], related251) }
      if(realPositives[i] == 86){ trainBaseTransferTrain(0.10, realPositives[i], related251) }
      if(realPositives[i] == 99){ trainBaseTransferTrain(0.10, realPositives[i], related251) }
      if(realPositives[i] == 179){ trainBaseTransferTrain(0.10, realPositives[i], related251) }
      if(realPositives[i] == 204){ trainBaseTransferTrain(0.10, realPositives[i], related251) }
      if(realPositives[i] == 223){ trainBaseTransferTrain(0.10, realPositives[i], related251) }
      if(realPositives[i] == 183){ trainBaseTransferTrain(0.10, realPositives[i], related251) }
      if(realPositives[i] == 197){ trainBaseTransferTrain(0.10, realPositives[i], related251) }
      if(realPositives[i] == 200){ trainBaseTransferTrain(0.10, realPositives[i], related251) }
      if(realPositives[i] == 233){ trainBaseTransferTrain(0.10, realPositives[i], related251) }
      if(realPositives[i] == 249){ trainBaseTransferTrain(0.10, realPositives[i], related251) }
    }
}

trainBaseTransferTrain<-function(percent, positive, relatedAll)
{
  general<-"java -cp libsvm.jar svm_train -s 0 -t 2 "
  wd<-"/home/dben652/AccGenSVM/data/caltech256/positives/"
  
  for(dataRep in 1:10)
  {
    i<-1
    for(negativePositive in 1:256)
    {
      if(negativePositive != positive)
      {
        for(related in length(relatedAll))
        {
          instructions[i]<-paste(general,wd,positive,"/",percent*100,"percent/data/data",dataRep,"/samplebasetransfer",positive,"_",negativePositive,"_",related,".csv ",wd,positive,"/",percent*100,"percent/training/data",dataRep,"/samplebasetransfer",positive,"_",negativePositive,"_",related,".csv.model", " >> ",wd,positive,"/",percent*100,"percent/training/data",dataRep,"/samplebasetransfer",positive,"_",negativePositive,"_",related,".txt", sep="")
          i<-i+1
        }
      }
    }

    write.table(instructions,paste(wd,positive,"/",percent*100,"percent/training/trainingTransferScriptData",dataRep,".txt",sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE)
  }
}


generateBaseTransferTrain<-function(percent, positive, relatedAll)
{
    wdPositive<-"/home/dben652/AccGenSVM/data/caltech256/positives/"
    wdRelated<-"/home/dben652/AccGenSVM/data/caltech256/related/"

    instructions<-as.character()
    
    for(dataRep in 1:10)
    {
      for(k in 1:length(relatedAll))
      {
        related <- relatedAll[k]
        
        for(i in 1:256)
        {
          positiveNegative<-i
          
          if(positiveNegative != positive)
          {
            positiveNegativeData<-read.csv(wdPositive,positive,"/",percent*100,"percent/data/data",dataRep,"/sampletransfer",positive,"_",positiveNegative,".csv")
            
            for(j in 1:256)
            {
              relatedNegative<-j
              
              if(relatedNegative!=negative)
              {
                relatedData<-read.csv(wdRelated,related,"/",percent*100,"percent/data/data",dataRep,"/samplebase",related,"_",relatedNegative,".csv")
                positiveNegativeData<-rbind(positiveNegativeData,relatedData)
              }
            }
          }
          
          #Save data
          write.csv(positiveNegativeData,paste(positive,"/",percent*100,"percent/data/data",dataRep,"/","samplebasetransfer",positive,"_",negativePositive,"_",related,".csv", sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
          
        }
      }
      
    }
}