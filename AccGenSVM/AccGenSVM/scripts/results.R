#results
existsFileResultsBase<-function(wd, positive, negative, percentData, percentHyp, dataRep, related, relatedNegative)
{
  if(file.exists(paste(wd,"/positives/",positive,"/",percentData*100,"percent/testbase/data",dataRep,"/sampletest",positive,"_",negative,"usingbase_",related, "_", relatedNegative,"_",percentHyp*100,"hyp.txt",sep="")) 
     && file.size(paste(wd,"/positives/",positive,"/",percentData*100,"percent/testbase/data",dataRep,"/sampletest",positive,"_",negative,"usingbase_",related, "_", relatedNegative,"_",percentHyp*100,"hyp.txt",sep="")) > 0)
  {
    return(1)
  }
  
  else
  {
    return(0)
  }
}

existsFileResultsAccGen<-function(wd, positive, negative, percentData, percentHyp, dataRep)
{
  if(file.exists(paste(wd,"/positives/",positive,"/",percentData*100,"percent/test/data",dataRep,"/sampletest",positive,"_",negative,"usingACCGEN_",percentHyp*100,"hyp.txt",sep="")) 
     && file.size(paste(wd,"/positives/",positive,"/",percentData*100,"percent/test/data",dataRep,"/sampletest",positive,"_",negative,"usingACCGEN_",percentHyp*100,"hyp.txt",sep="")) > 0)
  {
    return(1)
  }
  
  else
  {
    return(0)
  }
}

existsFileResultsASVM<-function(wd, positive, negative, percentData, percentHyp, dataRep)
{
  if(file.exists(paste(wd,"/positives/",positive,"/",percentData*100,"percent/test/data",dataRep,"/sampletest",positive,"_",negative,"usingASVM_",percentHyp*100,"hyp.txt",sep="")) 
     && file.size(paste(wd,"/positives/",positive,"/",percentData*100,"percent/test/data",dataRep,"/sampletest",positive,"_",negative,"usingASVM_",percentHyp*100,"hyp.txt",sep="")) > 0)
  {
    return(1)
  }
  
  else
  {
    return(0)
  }
}

existsFileResultsNoTransfer<-function(wd, positive, negative, percentData, percentHyp, dataRep)
{
  if(file.exists(paste(wd,"/positives/",positive,"/",percentData*100,"percent/test/data",dataRep,"/sampletest",positive,"_",negative,"usingnotransfer_",percentHyp*100,".txt",sep="")) 
     && file.size(paste(wd,"/positives/",positive,"/",percentData*100,"percent/test/data",dataRep,"/sampletest",positive,"_",negative,"usingnotransfer_",percentHyp*100,".txt",sep="")) > 0)
  {
    return(1)
  }
  
  else
  {
    return(0)
  }
}

existsFileConvergenceAccGen<-function(wd, positive, negative, percentData, percentHyp, dataRep)
{
  if(file.exists(paste(wd,"/positives/",positive,"/",percentData*100,"percent/test/data",dataRep,"/sampletest",positive,"_",negative,"usingACCGEN_",percentHyp*100,".out",sep="")) 
     && file.size(paste(wd,"/positives/",positive,"/",percentData*100,"percent/test/data",dataRep,"/sampletest",positive,"_",negative,"usingACCGEN_",percentHyp*100,".out",sep="")) > 0)
  {
    return(1)
  }
  
  else
  {
    return(0)
  }
}

existsFileConvergenceASVM<-function(wd, positive, negative, percentData, percentHyp, dataRep)
{
  if(file.exists(paste(wd,"/positives/",positive,"/",percentData*100,"percent/test/data",dataRep,"/sampletest",positive,"_",negative,"usingASVM_",percentHyp*100,".out",sep="")) 
     && file.size(paste(wd,"/positives/",positive,"/",percentData*100,"percent/test/data",dataRep,"/sampletest",positive,"_",negative,"usingASVM_",percentHyp*100,".out",sep="")) > 0)
  {
    return(1)
  }
  
  else
  {
    return(0)
  }
}

existsFileConvergenceAccGen<-function(wd, positive, negative, percentData, percentHyp, dataRep)
{
  if(file.exists(paste(wd,"/positives/",positive,"/",percentData*100,"percent/transfer/data",dataRep,"/sampletransfer",positive,"_",negative,"ACCGEN_",percentHyp*100,"hyp.out",sep="")) 
     && file.size(paste(wd,"/positives/",positive,"/",percentData*100,"percent/transfer/data",dataRep,"/sampletransfer",positive,"_",negative,"ACCGEN_",percentHyp*100,"hyp.out",sep="")) > 0)
  {
    return(1)
  }
  
  else
  {
    return(0)
  }
}

existsFileConvergenceNoTransfer<-function(wd, positive, negative, percentData, percentHyp, dataRep)
{
  if(file.exists(paste(wd,"/positives/",positive,"/",percentData*100,"percent/notransfer/data",dataRep,"/sampletransfer",positive,"_",negative,"_",percentHyp*100,"hyp.out",sep="")) 
     && file.size(paste(wd,"/positives/",positive,"/",percentData*100,"percent/notransfer/data",dataRep,"/sampletransfer",positive,"_",negative,"_",percentHyp*100,"hyp.out",sep="")) > 0)
  {
    return(1)
  }
  
  else
  {
    return(0)
  }
}

existsFileConvergenceASVM<-function(wd, positive, negative, percentData, percentHyp, dataRep)
{
  if(file.exists(paste(wd,"/positives/",positive,"/",percentData*100,"percent/transfer/data",dataRep,"/sampletransfer",positive,"_",negative,"ASVM_",percentHyp*100,"hyp.out",sep="")) 
     && file.size(paste(wd,"/positives/",positive,"/",percentData*100,"percent/transfer/data",dataRep,"/sampletransfer",positive,"_",negative,"ASVM_",percentHyp*100,"hyp.out",sep="")) > 0)
  {
    return(1)
  }
  
  else
  {
    return(0)
  }
}

resultsaccgen<-function(class, percentData, percentHyp)
{
  setwd("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/")
  
  for(dataRep in 1:30)
  {
    for(i in 1:256)
    {
      negative<-i
      
      if(existsFileResultsAccGen(getwd(), class, negative, percentData, percentHyp, dataRep) ==1)
      {
        result<-read.table(paste(getwd(),"/positives/",class,"/",percentData*100,"percent/test/data",dataRep,"/sampletest",class,"_",negative,"usingACCGEN_",percentHyp*100,"hyp.txt",sep=""))
        result<-result$V3
        result<-as.character(strsplit(as.character(result[1]), split="%")[1])
        result<-as.double(result)
        
        write.table(result, file=paste(getwd(), "/positives/",class,"/",percentData*100,"percent/resultsaccgen_",percentHyp*100,"hyp.txt", sep=""), append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
      }
    }
  }
}

resultsasvm<-function(class, percentData, percentHyp)
{
  setwd("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/")
  
  for(dataRep in 1:30)
  {
    for(i in 1:256)
    {
      negative<-i
      
      if(existsFileResultsASVM(getwd(), class, negative, percentData, percentHyp, dataRep) ==1)
      {
        result<-read.table(paste(getwd(),"/positives/",class,"/",percentData*100,"percent/test/data",dataRep,"/sampletest",class,"_",negative,"usingASVM_",percentHyp*100,"hyp.txt",sep=""))
        
        if(!is.na(result))
        {
        result<-result$V3
        result<-as.character(strsplit(as.character(result[1]), split="%")[1])
        result<-as.double(result)
        
        write.table(result, file=paste(getwd(), "/positives/",class,"/",percentData*100,"percent/resultsasvm_",percentHyp*100,"hyp.txt", sep=""), append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
        }
      }
    }
  }
}

resultsnotransfer<-function(class, percentData, percentHyp)
{
  setwd("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/")
  
  for(dataRep in 1:30)
  {
    for(i in 1:256)
    {
      negative<-i
      
      if(existsFileResultsNoTransfer(getwd(), class, negative, percentData, percentHyp, dataRep) ==1)
      {
        result<-read.table(paste(getwd(),"/positives/",class,"/",percentData*100,"percent/test/data",dataRep,"/sampletest",class,"_",negative,"usingnotransfer_",percentHyp*100,".txt",sep=""))
        result<-result$V3
        result<-as.character(strsplit(as.character(result[1]), split="%")[1])
        result<-as.double(result)
        
        write.table(result, file=paste(getwd(), "/positives/",class,"/",percentData*100,"percent/resultsnotransfer_",percentHyp*100,"hyp.txt", sep=""), append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
      }
    }
  }
}

convergenceaccgen<-function(class, percentData, percentHyp)
{
  setwd("C:/Users/dben652/workspace/AccGenSVM/data/imagenet2/")
  
  for(dataRep in 1:30)
  {
    for(i in 1:256)
    {
      negative<-i
      
      if(existsFileConvergenceAccGen(getwd(), class, negative, percentData, percentHyp, dataRep) ==1)
      {
        result<-readLines(paste(getwd(),"/positives/",class,"/",percentData*100,"percent/transfer/data",dataRep,"/sampletransfer",class,"_",negative,"ACCGEN_",percentHyp*100,"hyp.out",sep=""))
        
        for(j in 1:length(result))
        {
          if(length(result[j]) > 0 && length(grep("#iter = ", result[j]))>0)
          {
            res<-strsplit(result[j], split="=")
            res<-(as.double(res[[1]][2]))
            
            write.table(res, file=paste(getwd(), "/positives/",class,"/",percentData*100,"percent/resultsconvergenceaccgen_",percentHyp*100,"hyp.txt", sep=""), append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
          }
        }
      }
    }
  }
}

convergencenotransfer<-function(class, percentData, percentHyp)
{
  setwd("C:/Users/dben652/workspace/AccGenSVM/data/imagenet2/")
  
  for(dataRep in 1:30)
  {
    for(i in 1:256)
    {
      negative<-i
      
      if(existsFileConvergenceNoTransfer(getwd(), class, negative, percentData, percentHyp, dataRep) ==1)
      {
        result<-readLines(paste(getwd(),"/positives/",class,"/",percentData*100,"percent/notransfer/data",dataRep,"/sampletransfer",class,"_",negative,"_",percentHyp*100,"hyp.out",sep=""))
        
        for(j in 1:length(result))
        {
          if(length(result[j]) > 0 && length(grep("#iter = ", result[j]))>0)
          {
            res<-strsplit(result[j], split="=")
            res<-(as.double(res[[1]][2]))
            
            write.table(res, file=paste(getwd(), "/positives/",class,"/",percentData*100,"percent/resultsconvergencenotransfer_",percentHyp*100,"hyp.txt", sep=""), append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
          }
        }
      }
    }
  }
}

convergenceasvm<-function(class, percentData, percentHyp)
{
  setwd("C:/Users/dben652/workspace/AccGenSVM/data/imagenet2/")
  
  for(dataRep in 1:30)
  {
    for(i in 1:256)
    {
      negative<-i
      
      if(existsFileConvergenceASVM(getwd(), class, negative, percentData, percentHyp, dataRep) ==1)
      {
        result<-readLines(paste(getwd(),"/positives/",class,"/",percentData*100,"percent/transfer/data",dataRep,"/sampletransfer",class,"_",negative,"ASVM_",percentHyp*100,"hyp.out",sep=""))
        
        for(j in 1:length(result))
        {
          if(length(result[j]) > 0 && length(grep("#iter = ", result[j]))>0)
          {
            res<-strsplit(result[j], split="=")
            res<-(as.double(res[[1]][2]))
            
            write.table(res, file=paste(getwd(), "/positives/",class,"/",percentData*100,"percent/resultsconvergenceasvm_",percentHyp*100,"hyp.txt", sep=""), append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
          }
        }
      }
    }
  }
}

resultsbase<-function(class, relatedAll, percentData, percentHyp)
{
  setwd("C:/Users/dben652/workspace/AccGenSVM/data/imagenet2/")

    for(dataRep in 1:30)
    {
      for(i in 1:256)
      {
        negative<-i

        for(j in 1:256)
        {
          negativeRelated <- j
          
          for(rel in 1:length(relatedAll))
          {
            related <- relatedAll[rel]
          
            if(existsFileResultsBase(getwd(), class, negative, percentData, percentHyp, dataRep, related, negativeRelated) == 1)
            {
              result<-read.table(paste(getwd(),"/positives/",class,"/",percentData*100,"percent/testbase/data",dataRep,"/sampletest",class,"_",negative,"usingbase_",related,"_",negativeRelated,"_",percentHyp*100,"hyp.out",sep=""))
              result<-result$V3
              result<-as.character(strsplit(as.character(result[1]), split="%")[1])
              result<-as.double(result)
              
              write.table(result, file=paste(getwd(), "/positives/",class,"/",percentData*100,"percent/resultsbase_",percentHyp*100,"hyp.txt", sep=""), append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
            }
          }
        }
      }
    }
}

resultsaccgen(251, 0.30, 0.10)
resultsaccgen(251, 0.30, 0.25)
resultsaccgen(251, 0.30, 0.50)
resultsaccgen(251, 0.30, 1)

resultsnotransfer(251, 0.30, 0.10)
resultsnotransfer(251, 0.30, 0.25)
resultsnotransfer(251, 0.30, 0.50)
resultsnotransfer(251, 0.30, 1)


