#scale
scaleTransfer <- function(class, percent)
{
  #setwd(paste("/gpfs1m/projects/uoa00440/AccGenSVM/data/caltech256/positives/",class,"/",sep=""))
  #setwd(paste("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/positives/",class,"/",sep=""))
  setwd(paste("C:/Users/dben652/workspace/AccGenSVM/data/imagenet/positives/",class,"/",sep=""))
  
  general <- "java -jar libsvmscale.jar "
  
  for(i in 1:30)
  {
    samples<-sample(1:117, 20)
  
    for(j in 1:length(samples))
    {
      command <- paste(general, "-s ",getwd(),"/",percent*100,"percent/data/data",i,"/sampletransfer",class,"_",samples[j],".out ",getwd(),"/",percent*100,"percent/data/data",i,"/sampletransfer",class,"_",samples[j],".csv > ",getwd(),"/",percent*100,"percent/data/data",i,"/sampletransfer",class,"_",samples[j],".csv.scale",sep="")
      write.table(command, file="C:/Users/dben652/workspace/AccGenSVM/HTL/scripts/scale.sl", append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
    }
  }
}

scaleTest <- function(class, percent)
{
  #setwd(paste("/gpfs1m/projects/uoa00440/AccGenSVM/data/caltech256/positives/",class,"/",sep=""))
  #setwd(paste("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/positives/",class,"/",sep=""))
  setwd(paste("C:/Users/dben652/workspace/AccGenSVM/data/imagenet/positives/",class,"/",sep=""))
  
  general <- "java -jar libsvmscale.jar "
  
  for(i in 1:30)
  {
    samples<-sample(1:117, 20)
  
    for(j in 1:length(samples))
    {
      command <- paste(general, "-s ",getwd(),"/",percent*100,"percent/data/data",i,"/sampletest",class,"_",samples[j],".out ",getwd(),"/",percent*100,"percent/data/data",i,"/sampletest",class,"_",samples[j],".csv > ",getwd(),"/",percent*100,"percent/data/data",i,"/sampletest",class,"_",samples[j],".csv.scale",sep="")
      write.table(command, file="C:/Users/dben652/workspace/AccGenSVM/HTL/scripts/scale2.sl", append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
    }
  }
}

scaleBase <- function(class, percent)
{
  setwd(paste("/gpfs1m/projects/uoa00440/AccGenSVM/data/caltech256/related/",class,"/",sep=""))
  #setwd(paste("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/positives/",class,"/",sep=""))
  
  general <- "srun java -jar libsvmscale.jar "
  
  for(i in 1:30)
  {
    for(j in 1:256)
    {
    if(j != class)
    {
      command <- paste(general, "-s ",getwd(),"/",percent*100,"percent/data/data",i,"/samplebase",class,"_",j,".out ",getwd(),"/",percent*100,"percent/data/data",i,"/samplebase",class,"_",j,".csv > ",getwd(),"/",percent*100,"percent/data/data",i,"/samplebase",class,"_",j,".csv.scale",sep="")
      if(percent == 0.1) { write.table(command, file="/gpfs1m/projects/uoa00440/scale10.sl", append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE) }
      if(percent == 0.2) { write.table(command, file="/gpfs1m/projects/uoa00440/scale20.sl", append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE) }
      if(percent == 0.3) { write.table(command, file="/gpfs1m/projects/uoa00440/scale30.sl", append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE) }
    }
    }
  }
}

scaleTransfer(35, 0.10)
scaleTransfer(35, 0.20)
scaleTransfer(35, 0.30)
