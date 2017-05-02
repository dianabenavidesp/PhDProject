existsModelTransfer<-function(wd, dataRep, percentData, percentHyp, positive, negative)
{
  if(file.exists(paste(wd,"/positives/",positive,"/",percentData*100,"percent/transfer/data",dataRep,"/sampletransfer",positive,"_",negative,"ACCGEN_",percentHyp*100,"hyp.csv.model",sep="")) 
     && file.size(paste(wd,"/positives/",positive,"/",percentData*100,"percent/transfer/data",dataRep,"/sampletransfer",positive,"_",negative,"ACCGEN_",percentHyp*100,"hyp.csv.model",sep="")) > 0)
  {
    return(1)
  }
  
  else
  {
    return(0)
  }
}

existsModelRelated<-function(wd, dataRep, percentData, related, negative)
{
  if(file.exists(paste(wd,"/related/",related,"/",percentData*100,"percent/training/data",dataRep,"/samplebase",related,"_",negative,".csv.model",sep="")) 
     && file.size(paste(wd,"/related/",related,"/",percentData*100,"percent/training/data",dataRep,"/samplebase",related,"_",negative,".csv.model",sep="")) > 1100)
  {
    return(1)
  }
  
  else
  {
    return(0)
  }
}

trainasvm<-function(class, relatedAll, percentData, percentHyp)
{
  setwd("C:/Users/dben652/workspace/AccGenSVM/data/office/")
  
  positive<-class
  maxHyps<-5
  
  general<-"adapt_svm_train -s 0 -t 2 -c 1 -g 0.001 "
  
  instructions<-as.character()
  i<-1
  
  for(dataRep in 1:30)
  {
    for(i in 1:256)
    {
      negative <- i
    
      if(existsModelTransfer(getwd(),dataRep,percentData,percentHyp,class,negative)==1)
      {
        targetData <- paste(getwd(),"/positives/",class,"/",percentData*100,"percent/data/data",dataRep, "/sampletransfer",positive,"_",negative,".csv.scale ", sep="")
      
        sampleHyps <-sample(1:256, maxHyps)
        instructionsHyp<-as.character()
      
        for(k in 1:length(sampleHyps))
        {
          related<-sample(relatedAll,1)
          sampleHypsActual<-sampleHyps[k]
        
          if(existsModelRelated(getwd(), dataRep, percentData, related, sampleHypsActual)==1)
          {
            instructionsHyp<-paste(instructionsHyp, "+ ",getwd(),"/related/",related,"/",percentData*100,"percent/training/data",dataRep,"/samplebase",related,"_",sampleHypsActual,".csv.model ",sep="")    
          }
        }
      
        outputModel<-paste(getwd(),"/positives/",class,"/",percentData*100,"percent/transfer/data",dataRep, "/sampletransfer",positive,"_",negative,"ASVM_",percentHyp*100,"hyp.csv.model ", sep="")
      
        outputInfo<-paste(" >> ", getwd(),"/positives/",class,"/",percentData*100,"percent/transfer/data",dataRep, "/sampletransfer",positive,"_",negative,"ASVM_",percentHyp*100,"hyp.out", sep="")
      
        instructions[i]<-paste(general, instructionsHyp, targetData, outputModel, outputInfo, sep="")
        i<-i+1
      }
    }
  }
  
  write.table(instructions, file=paste("C:/Users/dben652/workspace/AccGenSVM/HTL/scripts/asvm-pos",class,"-",percentData*100,"data-",percentHyp*100,"hyp.sl",sep=""), append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
}

trainasvm(35, c(78, 100), 0.10, 1)
