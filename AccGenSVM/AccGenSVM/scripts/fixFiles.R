#Auxiliary function
fixFilesBase<-function(num, percent)
{
  setwd("C:/Users/dben652/workspace/AccGenSVM/data/office/related/")
  #setwd("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/related")
  
  #setwd("/gpfs1m/projects/uoa00440/AccGenSVM/data/caltech256/related/")
  
  for(k in 1:30)
  {
    iter<-k
    filenames<-NULL
    for(i in 1:117)
    {
      if(i!=num)
      {
        filenames[i]<-paste(num,"/",percent*100,"percent/data/data",iter,"/samplebase",num,"_",i,".csv",sep="")
      }
    }
    
    for( f in filenames ){
      
      if(!is.na(f))
      {
        x <- readLines(f)
        y <- gsub( ":,", ":", x )
        yy <- gsub( ",", "\t", y )
        cat(yy, file=f, sep="\n")
      }
    }
  }
}

fixFilesTransfer<-function(num, percent)
{
  setwd("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/positives/")
  #setwd("/gpfs1m/projects/uoa00440/AccGenSVM/data/caltech256/positives/")
  
  for(k in 1:30)
  {
    iter<-k
    filenames<-NULL
    for(i in 1:117)
    {
      if(i!=num)
      {
        filenames[i]<-paste(num,"/",percent*100,"percent/data/data",iter,"/sampletransfer",num,"_",i,".csv",sep="")
      }
    }
    
    for( f in filenames ){
      
      if(!is.na(f))
      {
        x <- readLines(f)
        y <- gsub( ":,", ":", x )
        yy <- gsub( ",", "\t", y )
        cat(yy, file=f, sep="\n")
      }
    }
  }
}

fixFilesTest<-function(num, percent)
{
  #setwd("/gpfs1m/projects/uoa00440/AccGenSVM/data/caltech256/positives/")
  setwd("C:/Users/dben652/workspace/AccGenSVM/data/imagenet/positives/")
   
  for(k in 1:30)
  {
    iter<-k
    filenames<-NULL
    for(i in 1:117)
    {
      if(i!=num)
      {
        filenames[i]<-paste(num,"/",percent*100,"percent/data/data",iter,"/sampletest",num,"_",i,".csv",sep="")
      }
    }
    
    for( f in filenames ){
      
      if(!is.na(f))
      {
        x <- readLines(f)
        y <- gsub( ":,", ":", x )
        yy <- gsub( ",", "\t", y )
        cat(yy, file=f, sep="\n")
      }
    }
  }
}