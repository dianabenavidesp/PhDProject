existsFileTrain<-function(wd, positive, negative, percent, dataRep)
{
  if(file.exists(paste(wd,"/",positive,"/",percent*100,"percent/data/data",dataRep,"/samplebase",positive,"_",negative,".csv.scale",sep="")) 
    && file.size(paste(wd,"/",positive,"/",percent*100,"percent/data/data",dataRep,"/samplebase",positive,"_",negative,".csv.scale",sep="")) > 1100)
  {
    return(1)
  }
  
  else
  {
    return(0)
  }
}

trainBase<-function(class, percent)
{
    #setwd(paste("/gpfs1m/projects/uoa00440/AccGenSVM/data/caltech256/related/",class,"/",sep=""))
    setwd(paste("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/related/",sep=""))
    positive<-class

    general<-"java -jar libsvmtrain.jar -s 0 -t 2 -g 0.001 "

    instructions<-as.character()
    
    for(dataRep in 1:30)
    {
      for(negative in 1:256)
      {
        if(class!=negative)
        {
          if(existsFileTrain(getwd(), class, negative, percent, dataRep) == 1)
          {
            instructions<-paste(general,getwd(),"/",class,"/",percent*100,"percent/data/data",dataRep,"/samplebase",class,"_",negative,".csv.scale ",getwd(),"/",class,"/",percent*100,"percent/training/data",dataRep,"/samplebase",class,"_",negative,".csv.model", " >> ",getwd(),"/",class,"/",percent*100,"percent/training/data",dataRep,"/samplebase",class,"_",negative,".out", sep="")
            write.table(instructions,file=paste("C:/Users/dben652/workspace/AccGenSVM/HTL/scripts/training",class,"-",percent*100,"percent.sl",sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE, append = TRUE)
          }
        }
      }
    }
}

trainBase(243, 0.10)
trainBase(243, 0.20)
trainBase(243, 0.30)

trainBase(244, 0.10)
trainBase(244, 0.20)
trainBase(244, 0.30)

trainBase(248, 0.10)
trainBase(248, 0.20)
trainBase(248, 0.30)

trainBase(249, 0.10)
trainBase(249, 0.20)
trainBase(249, 0.30)

trainBase(255, 0.10)
trainBase(255, 0.20)
trainBase(255, 0.30)
