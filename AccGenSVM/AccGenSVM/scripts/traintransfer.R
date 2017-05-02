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

traintransfer<-function(class, percentData, percentHyp)
{
  setwd("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/")
  
  positive<-class

  general<-"java -jar libsvmtrain.jar -s 0 -t 2 -c 1 -g 0.001 "
  
  instructions<-as.character()
  i<-1

  for(dataRep in 1:30)
  {
    for(i in 1:256)
    {
      negative <- i
      
      if(existsModelTransfer(getwd(),dataRep,percentData,percentHyp,class,negative)==1 || existsModelTransfer(getwd(),dataRep,percentData,0.5,class,negative)==1 || existsModelTransfer(getwd(),dataRep,percentData,0.25,class,negative)==1 || existsModelTransfer(getwd(),dataRep,percentData,0.1,class,negative)==1)
      {
          instructions1<-paste(general, getwd(),"/positives/",class,"/",percentData*100,"percent/data/data",dataRep,"/sampletransfer",class,"_",negative,".csv.scale ", sep="")        
          instructions2<-paste(getwd(),"/positives/",class,"/",percentData*100,"percent/notransfer/data",dataRep,"/sampletransfer",class,"_",negative,"_",percentHyp*100,"hyp.csv.model ", sep="")
          instructions3<-paste(">> ", getwd(),"/positives/",class,"/",percentData*100,"percent/notransfer/data",dataRep,"/sampletransfer",class,"_",negative,"_",percentHyp*100,"hyp.out", sep="")

          instructions<-paste(instructions1, instructions2, instructions3, sep="")
          
          write.table(instructions, file=paste("C:/Users/dben652/workspace/AccGenSVM/HTL/scripts/notransfer-pos",class,"-",percentData*100,"data.sl",sep=""), append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
      }
    }
  }
}

traintransfer(35, 0.10, 0.25)
traintransfer(35, 0.10, 0.25)
traintransfer(35, 0.10, 0.25)

traintransfer(43, 0.30, 0.1)
traintransfer(43, 0.30, 0.25)
traintransfer(43, 0.30, 0.50)
traintransfer(43, 0.30, 1)

traintransfer(43, 0.30, 0.10)

traintransfer(251, 0.20, 0.10)