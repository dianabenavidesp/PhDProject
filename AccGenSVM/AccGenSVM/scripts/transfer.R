existsFileTransfer<-function(wd, dataRep, percentData, positive, negative)
{
  if(file.exists(paste(wd,"/positives/",positive,"/",percentData*100,"percent/data/data",dataRep,"/sampletransfer",positive,"_",negative,".csv.scale",sep="")) 
  && file.size(paste(wd,"/positives/",positive,"/",percentData*100,"percent/data/data",dataRep,"/sampletransfer",positive,"_",negative,".csv.scale",sep="")) > 1)
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
  && file.size(paste(wd,"/related/",related,"/",percentData*100,"percent/training/data",dataRep,"/samplebase",related,"_",negative,".csv.model",sep="")) > 1)
  {
    return(1)
  }
  
  else
  {
    return(0)
  }
}

transferaccgen<-function(class, relatedAll, percentData, percentHyp, dataRep)
{
    setwd("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/")
  
    positive<-class
    maxModels<-10
    numModels<-0

    general<-"java -jar accgensvm.jar -s 0 -t 2 -c 1 -g 0.001 "

    instructions<-as.character()
    instructionsHyp<-as.character()
    numHyp<-as.integer(256*percentHyp)
    
    i<-1
    numModels<-1 
    
    negatives<-sample(1:256,maxModels)
      
    for(neg in 1:length(negatives))
    {
        negative<-negatives[neg]
      
        if(!negative %in% relatedAll && numModels <= maxModels && class!=negative && existsFileTransfer(getwd(), dataRep, percentData, positive, negative) == 1)
        {
          targetData <- paste("-f ", getwd(),"/positives/",class,"/",percentData*100,"percent/data/data",dataRep, "/sampletransfer",positive,"_",negative,".csv.scale ", sep="")
          
          sampleHyps <-sample(1:256, numHyp)
          instructionsHyp<-as.character()
          
          for(k in 1:length(sampleHyps))
          {
            related<-if(length(relatedAll)==1){ relatedAll[1]} else {sample(relatedAll,1)}
            sampleHypsActual<-sampleHyps[k]
            
            if(existsModelRelated(getwd(), dataRep, percentData, related, sampleHypsActual)==1)
            {
              instructionsHyp<-paste(instructionsHyp, "-H ",getwd(),"/related/",related,"/",percentData*100,"percent/training/data",dataRep,"/samplebase",related,"_",sampleHypsActual,".csv.model ",sep="")    
     
            }
          }
          
          outputModel<-paste("-y ", getwd(),"/positives/",class,"/",percentData*100,"percent/transfer/data",dataRep, "/sampletransfer",positive,"_",negative,"ACCGEN_",percentHyp*100,"hyp.csv.model ", sep="")
          
          outputInfo<-paste("-a ", getwd(),"/positives/",class,"/",percentData*100,"percent/transfer/data",dataRep, "/sampletransfer",positive,"_",negative,"ACCGEN_",percentHyp*100,"hyp.out", sep="")
          
          instructions[i]<-paste(general, targetData, instructionsHyp, outputModel, outputInfo, sep="")
          i<-i+1
          
          numModels<-numModels+1
        }
    }
      
    write.table(instructions, file=paste("C:/Users/dben652/workspace/AccGenSVM/HTL/scripts/accgen-pos",class,"-",percentData*100,"data-",percentHyp*100,"hyp.sl",sep=""), append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
}

transferaccgen(251,c(243, 248),0.3,1,1)
transferaccgen(251,c(243, 248),0.3,1,2)
transferaccgen(251,c(243, 248),0.3,1,3)
transferaccgen(251,c(243, 248),0.3,1,4)
transferaccgen(251,c(243, 248),0.3,1,5)
transferaccgen(251,c(243, 248),0.3,1,6)
transferaccgen(251,c(243, 248),0.3,1,7)
transferaccgen(251,c(243, 248),0.3,1,8)
transferaccgen(251,c(243, 248),0.3,1,9)
transferaccgen(251,c(243, 248),0.3,1,10)
transferaccgen(251,c(243, 248),0.3,1,11)
transferaccgen(251,c(243, 248),0.3,1,12)
transferaccgen(251,c(243, 248),0.3,1,13)
transferaccgen(251,c(243, 248),0.3,1,14)
transferaccgen(251,c(243, 248),0.3,1,15)
transferaccgen(251,c(243, 248),0.3,1,16)
transferaccgen(251,c(243, 248),0.3,1,17)
transferaccgen(251,c(243, 248),0.3,1,18)
transferaccgen(251,c(243, 248),0.3,1,19)
transferaccgen(251,c(243, 248),0.3,1,20)
transferaccgen(251,c(243, 248),0.3,1,21)
transferaccgen(251,c(243, 248),0.3,1,22)
transferaccgen(251,c(243, 248),0.3,1,23)
transferaccgen(251,c(243, 248),0.3,1,24)
transferaccgen(251,c(243, 248),0.3,1,25)
transferaccgen(251,c(243, 248),0.3,1,26)
transferaccgen(251,c(243, 248),0.3,1,27)
transferaccgen(251,c(243, 248),0.3,1,28)
transferaccgen(251,c(243, 248),0.3,1,29)
transferaccgen(251,c(243, 248),0.3,1,30)

transferaccgen(35,c(78, 100),0.1,0.10,1)
transferaccgen(35,c(78, 100),0.1,0.10,2)
transferaccgen(35,c(78, 100),0.1,0.10,3)
transferaccgen(35,c(78, 100),0.1,0.10,4)
transferaccgen(35,c(78, 100),0.1,0.10,5)
transferaccgen(35,c(78, 100),0.1,0.10,6)
transferaccgen(35,c(78, 100),0.1,0.10,7)
transferaccgen(35,c(78, 100),0.1,0.10,8)
transferaccgen(35,c(78, 100),0.1,0.10,9)
transferaccgen(35,c(78, 100),0.1,0.10,10)
transferaccgen(35,c(78, 100),0.1,0.10,11)
transferaccgen(35,c(78, 100),0.1,0.10,12)
transferaccgen(35,c(78, 100),0.1,0.10,13)
transferaccgen(35,c(78, 100),0.1,0.10,14)
transferaccgen(35,c(78, 100),0.1,0.10,15)
transferaccgen(35,c(78, 100),0.1,0.10,16)
transferaccgen(35,c(78, 100),0.1,0.10,17)
transferaccgen(35,c(78, 100),0.1,0.10,18)
transferaccgen(35,c(78, 100),0.1,0.10,19)
transferaccgen(35,c(78, 100),0.1,0.10,20)
transferaccgen(35,c(78, 100),0.1,0.10,21)
transferaccgen(35,c(78, 100),0.1,0.10,22)
transferaccgen(35,c(78, 100),0.1,0.10,23)
transferaccgen(35,c(78, 100),0.1,0.10,24)
transferaccgen(35,c(78, 100),0.1,0.10,25)
transferaccgen(35,c(78, 100),0.1,0.10,26)
transferaccgen(35,c(78, 100),0.1,0.10,27)
transferaccgen(35,c(78, 100),0.1,0.10,28)
transferaccgen(35,c(78, 100),0.1,0.10,29)
transferaccgen(35,c(78, 100),0.1,0.10,30)

transferaccgen(43,c(12, 74),0.3,1,1)
transferaccgen(43,c(12, 74),0.3,1,2)
transferaccgen(43,c(12, 74),0.3,1,3)
transferaccgen(43,c(12, 74),0.3,1,4)
transferaccgen(43,c(12, 74),0.3,1,5)
transferaccgen(43,c(12, 74),0.3,1,6)
transferaccgen(43,c(12, 74),0.3,1,7)
transferaccgen(43,c(12, 74),0.3,1,8)
transferaccgen(43,c(12, 74),0.3,1,9)
transferaccgen(43,c(12, 74),0.3,1,10)
transferaccgen(43,c(12, 74),0.3,1,11)
transferaccgen(43,c(12, 74),0.3,1,12)
transferaccgen(43,c(12, 74),0.3,1,13)
transferaccgen(43,c(12, 74),0.3,1,14)
transferaccgen(43,c(12, 74),0.3,1,15)
transferaccgen(43,c(12, 74),0.3,1,16)
transferaccgen(43,c(12, 74),0.3,1,17)
transferaccgen(43,c(12, 74),0.3,1,18)
transferaccgen(43,c(12, 74),0.3,1,19)
transferaccgen(43,c(12, 74),0.3,1,20)
transferaccgen(43,c(12, 74),0.3,1,21)
transferaccgen(43,c(12, 74),0.3,1,22)
transferaccgen(43,c(12, 74),0.3,1,23)
transferaccgen(43,c(12, 74),0.3,1,24)
transferaccgen(43,c(12, 74),0.3,1,25)
transferaccgen(43,c(12, 74),0.3,1,26)
transferaccgen(43,c(12, 74),0.3,1,27)
transferaccgen(43,c(12, 74),0.3,1,28)
transferaccgen(43,c(12, 74),0.3,1,29)
transferaccgen(43,c(12, 74),0.3,1,30)
