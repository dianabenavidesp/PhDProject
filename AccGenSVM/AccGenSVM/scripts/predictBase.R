#GENERATING STATEMENTS FOR TRAINING BASE MODELS USING REGULAR SVM

#10 most related, although I will select 5 of them
predictBase<-function(percent)
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
    if(i==1) { predictBasePositive(realPositives[i], related251, percent) }
    if(i==2) { predictBasePositive(realPositives[i], related145, percent) }
    if(i==3) { predictBasePositive(realPositives[i], related253, percent) }
    if(i==4) { predictBasePositive(realPositives[i], related232, percent) }
    if(i==5) { predictBasePositive(realPositives[i], related96, percent) }
    if(i==6) { predictBasePositive(realPositives[i], related86, percent) }
    if(i==7) { predictBasePositive(realPositives[i], related99, percent) }
    if(i==8) { predictBasePositive(realPositives[i], related179, percent) }
    if(i==9) { predictBasePositive(realPositives[i], related204, percent) }
    if(i==10) { predictBasePositive(realPositives[i], related223, percent) }
    if(i==11) { predictBasePositive(realPositives[i], related183, percent) }
    if(i==12) { predictBasePositive(realPositives[i], related197, percent) }
    if(i==13) { predictBasePositive(realPositives[i], related200, percent) }
    if(i==14) { predictBasePositive(realPositives[i], related233, percent) }
    if(i==15) { predictBasePositive(realPositives[i], related249, percent) }
  }
  
}

predictBasePositive<-function(realPositive, negative, percent)
{
  general<-"java -cp libsvm.jar svm_predict -s 0 -t 2 "
  wdRelated<-"/projects/00440/AccGenSVM/data/caltech256/related/"
  wdPositive<-"/projects/00440/AccGenSVM/data/caltech256/positives/"
  instructions<-as.character()
  
  for(j in 1:length(negative))
  {
    class<-negative[j]
    i<-1
    
    for(k in 1:256)
    {
        negativeRealPositive<-k
        
        if(realPositive != negativeRealPositive)
        {
          for(dataRep in 1:10)
          {
            for(negative2 in 1:256)
            {
              if(class!=negative2)
              {
                instructions[i]<-paste(general,wdPositive,realPositive,"/",percent*100,"percent/data/data",dataRep,"/sampletest",realPositive,"_",negativeRealPositive,".csv ",wdRelated,negative,"/",percent*100,"percent/training/data",dataRep,"/samplebase",negative,"_",negative2,".csv.model ",wdPositive,realPositive,"/",percent*100,"percent/test/testBase/data",dataRep, "/sampletest",realPositive,"_",negativeRealPositive,"usingsamplebase",negative,"_",negative2,".out >> ",wdPositive,realPositive,"/",percent*100,"percent/test/testBase/data",dataRep,"/sampletest",realPositive,"_",negativeRealPositive,"usingsamplebase",negative,"_",negative2,".txt",sep="")
                i<-i+1
              }
            }
          }
        }
    }
    write.table(instructions,paste(wdPositive,realPositive,"/",percent*100,"percent/test/testBase/testScriptBaseData.txt",sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE)
  }
}