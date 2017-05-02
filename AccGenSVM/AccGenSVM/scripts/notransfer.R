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

notransfer1<-function(class, relatedAll, percentData, percentHyp)
{
  setwd("C:/Users/dben652/workspace/AccGenSVM/data/imagenet2/")
  
  general<-"java -jar libsvmpredict.jar "
  
  instructions<-as.character()
  maxHyps<-256*percentHyp
  numHyps<-0
  
    for(dataRep in 1:30)
    {
      for(i in 1:256)
      {
        negative<-i
        numHyps<-0
        
        for(j in 1:256)
        {
          negativeRelated <- j
          
          for(rel in 1:length(relatedAll))
          {
            related <- relatedAll[rel]
          
            if(numHyps<= maxHyps && existsFilePredict(getwd(), dataRep, percentData, class, negative) == 1 && existsModelRelated(getwd(), dataRep, percentData, related, negativeRelated) == 1)
            {
              instructions1<-paste(general, getwd(),"/positives/",class,"/",percentData*100,"percent/data/data",dataRep,"/sampletest",class,"_",negative,".csv.scale ", sep="")        
              instructions2<-paste(getwd(),"/related/",related,"/",percentData*100,"percent/training/data",dataRep,"/samplebase",related,"_",negativeRelated,".csv.model ", sep="")
              instructions3<-paste(getwd(),"/positives/",class,"/",percentData*100,"percent/testbase/data",dataRep,"/sampletest",class,"_",negative,"usingbase_",related,"_",negativeRelated,"_",percentHyp*100,"hyp.txt ", sep="")
              instructions4<-paste(">> ",getwd(),"/positives/",class,"/",percentData*100,"percent/testbase/data",dataRep,"/sampletest",class,"_",negative,"usingbase_",related,"_",negativeRelated,"_",percentHyp*100,"hyp.out ", sep="")
              
              instructions<-paste(instructions1, instructions2, instructions3, instructions4,sep="")
              
              write.table(instructions, file=paste("C:/Users/dben652/workspace/AccGenSVM/HTL/scripts/testnotransfer",class,"-",percentData*100,"data.sl",sep=""), append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
              
              numHyps<-numHyps + 1
            }
          }
        }
      }
    }
}

notransfer1(251,c(243, 248),0.1, 0.10)

notransfer1(43,c(12, 74),0.1, 0.10)

notransfer1(35,c(78, 100),0.1, 0.10)