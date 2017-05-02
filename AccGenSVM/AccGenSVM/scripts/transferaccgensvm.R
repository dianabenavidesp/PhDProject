#GENERATING STATEMENTS FOR TRAINING MODELS FROM TRANSFER DATA USING REGULAR SVM

#15 more frequent classes
transferAccGenSVM<-function(percent)
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
    if(i==1) { transferAccGenSVMPositive(realPositives[i], related251, percent) }
    if(i==2) { transferAccGenSVMPositive(realPositives[i], related145, percent) }
    if(i==3) { transferAccGenSVMPositive(realPositives[i], related253, percent) }
    if(i==4) { transferAccGenSVMPositive(realPositives[i], related232, percent) }
    if(i==5) { transferAccGenSVMPositive(realPositives[i], related96, percent) }
    if(i==6) { transferAccGenSVMPositive(realPositives[i], related86, percent) }
    if(i==7) { transferAccGenSVMPositive(realPositives[i], related99, percent) }
    if(i==8) { transferAccGenSVMPositive(realPositives[i], related179, percent) }
    if(i==9) { transferAccGenSVMPositive(realPositives[i], related204, percent) }
    if(i==10) { transferAccGenSVMPositive(realPositives[i], related223, percent) }
    if(i==11) { transferAccGenSVMPositive(realPositives[i], related183, percent) }
    if(i==12) { transferAccGenSVMPositive(realPositives[i], related197, percent) }
    if(i==13) { transferAccGenSVMPositive(realPositives[i], related200, percent) }
    if(i==14) { transferAccGenSVMPositive(realPositives[i], related233, percent) }
    if(i==15) { transferAccGenSVMPositive(realPositives[i], related249, percent) }
  }
  
}

transferAccGenSVMPositive<-function(positive, related, percent)
{
  wd<-"/projects/uoa00440/AccGenSVM/data/caltech256/positives/"
  wdRelated<-"/projects/uoa00440/AccGenSVM/data/caltech256/related/"
  generalInstruction<-"java -Djava.library.path=/share/easybuild/RHEL6.3/westmere/software/R/3.3.0-intel-2015a/lib64/R/library/rJava/jri -jar accgensvm.jar -s 0 -t 2 -c 1 -g 0.001"

  instructions<-as.character()
  class<-positive
  i<-1
  
  for(dataRep in 1:10)
  {
    for(negative in 1:256)
    {
      if(class!=negative)
      {
        transferData<-paste(generalInstruction, "-f ",wd,class,"/",percent*100,"percent/data/data",dataRep,"/sampletransfer",class,"_",negative,".csv ",sep="")
        transferModel<-paste("-y ",wdRelated,class,"/",percent*100,"percent/transfer/transferAccGenSVM/data",dataRep,"/sampletransferACCGEN",class,"_",negative,".csv.model ",sep="")

        for(j in 1:length(related))
        {
          baseModel<-""
          rel <- related[j]
          
          for(k in 1:256)
          {
            negRel <- k
            if(negRel != rel)
            {
              baseModel<-paste(baseModel,"-H ", wd, rel,"/",percent*100,"percent/training/data",dataRep,"/samplebase",rel,"_",negRel,".csv.model ", sep="")
            }
          }
          
          outputLog<-paste("-a ",wd,class,"/",percent*100,"percent/transfer/transferAccGenSVM/data",dataRep,"/sampletransferACCGENLog",class,"_",negative,"using",rel,".txt",sep="")
          
          instructions[i]<-paste(transferData,transferModel,baseModel,outputLog)
          i<-i+1
        }
      }
    }
  }
  #write.table(instructions,paste("C:/Users/dben652/Desktop/testScript.txt",sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(instructions,paste(wd,class,"/",percent*100,"percent/transfer/transferAccGenSVMScriptData",dataRep,".txt",sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE)
}