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

existsModelASVM<-function(wd, dataRep, percentData, percentHyp, positive, negative)
{
  if(file.exists(paste(wd,"/positives/",positive,"/",percentData*100,"percent/transfer/data",dataRep,"/sampletransfer",positive,"_",negative,"ASVM_",percentHyp*100,"hyp.csv.model",sep="")) 
     && file.size(paste(wd,"/positives/",positive,"/",percentData*100,"percent/transfer/data",dataRep,"/sampletransfer",positive,"_",negative,"ASVM_",percentHyp*100,"hyp.csv.model",sep="")) > 0)
  {
    return(1)
  }
  
  else
  {
    return(0)
  }
}


existsFilePredict<-function(wd, dataRep, percentData, positive, negative)
{
  if(file.exists(paste(wd,"/positives/",positive,"/",percentData*100,"percent/data/data",dataRep,"/sampletest",positive,"_",negative,".csv.scale",sep="")) 
     && file.size(paste(wd,"/positives/",positive,"/",percentData*100,"percent/data/data",dataRep,"/sampletest",positive,"_",negative,".csv.scale",sep="")) > 0)
  {
    return(1)
  }
  
  else
  {
    return(0)
  }
}

predictaccgen<-function(class, percentData, percentHyp)
{
  setwd("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/")
  
  general<-"java -jar libsvmpredict.jar "
  instructions1<-as.character()
  instructions2<-as.character()
  instructions3<-as.character()
  instructions4<-as.character()
  
  for(dataRep in 1:30)
  {
    for(i in 1:256)
    {
      negative <- i
      
      if(existsModelTransfer(getwd(),dataRep,percentData,percentHyp,class,negative)==1)
      {
        for(j in 1:256)
        {
          negative2<-j
          
          if(existsFilePredict(getwd(),dataRep,percentData,class,negative2)==1 && existsModelTransfer(getwd(),dataRep,percentData,percentHyp,class,negative2)==1)
          {
            instructions1<-paste(general, getwd(),"/positives/",class,"/",percentData*100,"percent/data/data",dataRep,"/sampletest",class,"_",negative2,".csv.scale ", sep="")        
            instructions2<-paste(getwd(),"/positives/",class,"/",percentData*100,"percent/transfer/data",dataRep,"/sampletransfer",class,"_",negative2,"ACCGEN_",percentHyp*100,"hyp.csv.model ", sep="")
            instructions3<-paste(getwd(),"/positives/",class,"/",percentData*100,"percent/test/data",dataRep,"/sampletest",class,"_",negative2,"usingACCGEN_",percentHyp*100,"hyp.out ", sep="")
            instructions4<-paste(">> ",getwd(),"/positives/",class,"/",percentData*100,"percent/test/data",dataRep,"/sampletest",class,"_",negative2,"usingACCGEN_",percentHyp*100,"hyp.txt", sep="")
            
            instructions<-paste(instructions1, instructions2, instructions3, instructions4,sep="")
            
            write.table(instructions, file=paste("C:/Users/dben652/workspace/AccGenSVM/HTL/scripts/testaccgen-pos",class,"-",percentData*100,"data-",percentHyp*100,"hyp.sl",sep=""), append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
          }
        }
      }
    }
  }
}

predictnotransfer<-function(class, percentData, percentHyp)
{
  setwd("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/")
  
  general<-"java -jar libsvmpredict.jar "
  instructions1<-as.character()
  instructions2<-as.character()
  instructions3<-as.character()
  instructions4<-as.character()
  
  for(dataRep in 1:30)
  {
    for(i in 1:256)
    {
      negative <- i
      
      if(existsModelTransfer(getwd(),dataRep,percentData,percentHyp,class,negative)==1)
      {
        for(j in 1:256)
        {
          negative2<-j
          
          if(existsFilePredict(getwd(),dataRep,percentData,class,negative2)==1 && existsModelTransfer(getwd(),dataRep,percentData,percentHyp,class,negative2)==1)
          {
            instructions1<-paste(general, getwd(),"/positives/",class,"/",percentData*100,"percent/data/data",dataRep,"/sampletest",class,"_",negative2,".csv.scale ", sep="")        
            instructions2<-paste(getwd(),"/positives/",class,"/",percentData*100,"percent/notransfer/data",dataRep,"/sampletransfer",class,"_",negative2,"_",percentHyp*100,"hyp.csv.model ", sep="")
            instructions3<-paste(getwd(),"/positives/",class,"/",percentData*100,"percent/test/data",dataRep,"/sampletest",class,"_",negative2,"usingnotransfer_",percentHyp*100,".out ", sep="")
            instructions4<-paste(">> ",getwd(),"/positives/",class,"/",percentData*100,"percent/test/data",dataRep,"/sampletest",class,"_",negative2,"usingnotransfer_",percentHyp*100,".txt", sep="")
          
            instructions<-paste(instructions1, instructions2, instructions3, instructions4,sep="")
          
            write.table(instructions, file=paste("C:/Users/dben652/workspace/AccGenSVM/HTL/scripts/testnotransfer-pos",class,"-",percentData*100,"data.sl",sep=""), append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
          }
        }
      }
    }
  }
}

predictasvm<-function(class, percentData, percentHyp)
{
  setwd("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/")
  
  general<-"java -jar libsvmpredict.jar "
  instructions1<-as.character()
  instructions2<-as.character()
  instructions3<-as.character()
  instructions4<-as.character()
  
  for(dataRep in 1:30)
  {
    for(i in 1:117)
    {
      negative <- i
      
      if(existsModelASVM(getwd(),dataRep,percentData,percentHyp,class,negative)==1)
      {
        for(j in 1:117)
        {
          negative2<-j
          
          if(existsFilePredict(getwd(),dataRep,percentData,class,negative2)==1 && existsModelASVM(getwd(),dataRep,percentData,percentHyp,class,negative2)==1)
          {
            instructions1<-paste(general, getwd(),"/positives/",class,"/",percentData*100,"percent/data/data",dataRep,"/sampletest",class,"_",negative2,".csv.scale ", sep="")        
            instructions2<-paste(getwd(),"/positives/",class,"/",percentData*100,"percent/transfer/data",dataRep,"/sampletransfer",class,"_",negative2,"ASVM_",percentHyp*100,"hyp.csv.model ", sep="")
            instructions3<-paste(getwd(),"/positives/",class,"/",percentData*100,"percent/test/data",dataRep,"/sampletest",class,"_",negative2,"usingASVM_",percentHyp*100,"hyp.out ", sep="")
            instructions4<-paste(">> ",getwd(),"/positives/",class,"/",percentData*100,"percent/test/data",dataRep,"/sampletest",class,"_",negative2,"usingASVM_",percentHyp*100,"hyp.txt", sep="")
          
            instructions<-paste(instructions1, instructions2, instructions3, instructions4,sep="")
          
            write.table(instructions, file=paste("C:/Users/dben652/workspace/AccGenSVM/HTL/scripts/testasvm-pos",class,"-",percentData*100,"data-",percentHyp*100,"hyp.sl",sep=""), append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
          }
        }
      }
    }
  }
}


predictaccgen(35, 0.10, 0.10)
predictnotransfer(35, 0.10, 0.10)
predictasvm(35, 0.10, 0.10)

predictaccgen(43, 0.30, 0.10)
predictaccgen(43, 0.30, 0.25)
predictaccgen(43, 0.30, 0.50)
predictaccgen(43, 0.30, 1)

predictaccgen(43, 0.30, 0.10)

predictnotransfer(251, 0.30, 0.1)
predictnotransfer(251, 0.30, 0.25)
predictnotransfer(251, 0.30, 0.5)
predictnotransfer(251, 0.30, 1)

predictaccgen(251, 0.30, 0.1)
predictaccgen(251, 0.30, 0.25)
predictaccgen(251, 0.30, 0.5)
predictaccgen(251, 0.30, 1)

predictnotransfer(43, 0.10, 1)
predictnotransfer(43, 0.30, 1)

predictnotransfer(43, 0.30, 0.10)

predictasvm(251, 0.10, 1)
