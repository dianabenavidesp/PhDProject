#attribute noise plus 1sd, equal mean

attributenoise2sd <- function(positive, percent)
{
  for(rep in 1:10)
  {
    setwd(paste("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/positives/",positive,"/10percent/data/data",rep,sep=""))
    fileName<-"sampletransfer251_145"
    data<-read.csv(paste(fileName,".csv",sep=""), stringsAsFactors =FALSE, header=FALSE)
    matrix<-matrix(0,nrow(data), 1000)
    classes<-matrix(0,nrow(data), 1)
    
    for(i in 1:nrow(data))
    {
      firstSplit<-strsplit(data[i,], split=":")
      
      secondSplit<-strsplit(firstSplit[[1]],split="\t")
      for(k in 2:length(secondSplit))
      {
        third<-as.numeric(unlist(secondSplit[[k]])[1])
        matrix[i,k-1]<-third
      }
      
      classes[i,1]<-as.numeric(unlist(secondSplit[[1]])[1])
    }
    data<-as.data.frame(matrix)
    
    cols<-c(19, 37, 64, 68, 109, 162, 302, 320, 323, 335, 348, 384, 395, 445, 455, 497, 536, 575, 585, 622, 682, 741, 790, 802, 824, 859, 890, 893, 906, 996)
    
    #means and sds
    means<-c(mean(data$V19), mean(data$V37), mean(data$V64), mean(data$V68), mean(data$V109), mean(data$V162), mean(data$V302), mean(data$V320), mean(data$V323), mean(data$V335), mean(data$V348), mean(data$V384), mean(data$V395), mean(data$V445), mean(data$V455), mean(data$V497), mean(data$V536), mean(data$V575), mean(data$V585), mean(data$V622), mean(data$V682), mean(data$V741), mean(data$V790), mean(data$V802), mean(data$V824), mean(data$V859), mean(data$V890), mean(data$V893), mean(data$V906), mean(data$V996))
    means<-data.frame(attb = cols, mean = means)
    sds<-c(sd(data$V19), sd(data$V37), sd(data$V64), sd(data$V68), sd(data$V109), sd(data$V162), sd(data$V302), sd(data$V320), sd(data$V323), sd(data$V335), sd(data$V348), sd(data$V384), sd(data$V395), sd(data$V445), sd(data$V455), sd(data$V497), sd(data$V536), sd(data$V575), sd(data$V585), sd(data$V622), sd(data$V682), sd(data$V741), sd(data$V790), sd(data$V802), sd(data$V824), sd(data$V859), sd(data$V890), sd(data$V893), sd(data$V906), sd(data$V996))
    sds<-sds * 2
    sds<-data.frame(attb = cols, sd = sds)
    
    
    sampleAttributes<-sample(cols,length(cols)*percent)
    sampleInstances<-sample(1:nrow(data),nrow(data)*percent)
    
    for(i in 1:ncol(data))
    {
      if(i %in% sampleAttributes)
      {
        for(j in 1:nrow(data))
        {
          if(j %in% sampleInstances)
          {
            data[j,i] <- rnorm(1, means$mean[means$attb==i], sds$sd[sds$attb==i])
          }
        }
        sampleAttributes<-sample(cols,length(cols)*percent)
      }
    }
    
    data<-cbind(data, classes)
    data<-getfinaldataCaltech(data)
    
    setwd(paste("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/noise/attributenoise2sd/",positive,"/",percent*100,"/data/data",rep,sep=""))
    write.table(data,paste(fileName, ".csv", sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  }
  
  fixFilesNoise2sd(positive, percent)
}



fixFilesNoise2sd<-function(positive, percent)
{
  setwd(paste("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/noise/attributenoise2sd/",positive,sep=""))
  i<-1
  for(k in 1:10)
  {
    filenames<-NULL
    filenames[i]<-paste(percent*100,"/data/data",k,"/sampletransfer251_145.csv",sep="")
    i<-i+1
    
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


#attribute noise between min and max

attributenoisemaxmin <- function(positive, percent)
{
  for(rep in 1:10)
  {
    setwd(paste("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/positives/",positive,"/10percent/data/data",rep,sep=""))
    fileName<-"sampletransfer251_145"
    data<-read.csv(paste(fileName,".csv",sep=""), stringsAsFactors =FALSE, header=FALSE)
    matrix<-matrix(0,nrow(data), 1000)
    classes<-matrix(0,nrow(data), 1)
    
    for(i in 1:nrow(data))
    {
      firstSplit<-strsplit(data[i,], split=":")
      
      secondSplit<-strsplit(firstSplit[[1]],split="\t")
      for(k in 2:length(secondSplit))
      {
        third<-as.numeric(unlist(secondSplit[[k]])[1])
        matrix[i,k-1]<-third
      }
      
      classes[i,1]<-as.numeric(unlist(secondSplit[[1]])[1])
    }
    data<-as.data.frame(matrix)

    cols<-c(19, 37, 64, 68, 109, 162, 302, 320, 323, 335, 348, 384, 395, 445, 455, 497, 536, 575, 585, 622, 682, 741, 790, 802, 824, 859, 890, 893, 906, 996)
    
    sampleAttributes<-sample(cols,length(cols)*percent)
    sampleInstances<-sample(1:nrow(data),nrow(data)*percent)
    
    for(j in 1:nrow(data))
    {
      if(j %in% sampleInstances)
      {
        sampleAttributes<-sample(cols,length(cols)*percent)
        
        for(i in 1:ncol(data))
        {
          if(i %in% sampleAttributes)
          {
            data[j,i] <- sample(min(as.numeric(data[,i])):max(as.numeric(data[,i])),1)[1]
            #data[j,i] <- sample(min(as.numeric(data[(data[,i]) > 0,i])):max(as.numeric(data[(data[,i]) > 0,i])),1)[1]
          }
        }
        
      }
    }
    
    data<-cbind(data, classes)
    data<-getfinaldataCaltech(data)
    
    setwd(paste("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/noise/attributenoisemaxmin/",positive,"/",percent*100,"/data/data",rep,sep=""))
    write.table(data,paste(fileName, ".csv", sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  }
  
  fixFilesNoiseMaxMin(positive, percent)
}



fixFilesNoiseMaxMin<-function(positive, percent)
{
  setwd(paste("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/noise/attributenoisemaxmin/",positive,sep=""))
  i<-1
  for(k in 1:10)
  {
    filenames<-NULL
    filenames[i]<-paste(percent*100,"/data/data",k,"/sampletransfer251_145.csv",sep="")
    i<-i+1
    
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

#a lot of non-systematic noise!!!
setwd("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/positives/251/10percent/data/data10")
fileName<-"sampletransfer251_145"
data<-read.csv(paste(fileName,".csv.scale",sep=""), stringsAsFactors =FALSE, header=FALSE)
matrix<-matrix(0,nrow(data), 1000)
classes<-matrix(0,nrow(data), 1)

for(i in 1:nrow(data))
{
  firstSplit<-strsplit(data[i,], split=":")
  
  secondSplit<-strsplit(firstSplit[[1]],split=" ")
  for(k in 2:length(secondSplit))
  {
    third<-as.numeric(unlist(secondSplit[[k]])[1])
    matrix[i,k-1]<-third
  }
  
  classes[i,1]<-as.numeric(unlist(secondSplit[[1]])[1])
}
data<-as.data.frame(matrix)

percent<-0.3
sample(1:nrow(data)*10000,nrow(data)*percent)

cols<-c(19, 37, 64, 68, 109, 162, 302, 320, 323, 335, 348, 384, 395, 445, 455, 497, 536, 575, 585, 622, 682, 741, 790, 802, 824, 859, 890, 893, 906, 996)

sampleAttributes<-sample(cols,length(cols)*percent)
sampleInstances<-sample(1:nrow(data),nrow(data)*percent)

for(i in 1:ncol(data))
{
  if(i %in% sampleAttributes)
  {
    for(j in 1:nrow(data))
    {
      if(j %in% sampleInstances)
      {
        data[j,i] <- sample(1:nrow(data)*10000,nrow(data)*percent)[1]
        print(data[j,i])
      }
    }
    sampleAttributes<-sample(cols,length(cols)*percent)
  }
}

data<-cbind(data, classes)
data<-getfinaldataCaltech(data)

setwd("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/noise/attributenoisenonsys/30/data/data10")
write.table(data,paste(fileName, ".csv.scale", sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")

fixFilesNoiseNonSys(percent)

fixFilesNoiseNonSys<-function(percent)
{
  setwd("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/noise/attributenoisenonsys")
  i<-1
  for(k in 1:10)
  {
    filenames<-NULL
    filenames[i]<-paste(percent*100,"/data/data",k,"/sampletransfer251_145.csv.scale",sep="")
    i<-i+1

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


attributenoise <- function(positive, percent)
{
    #attribute noise
    setwd(paste("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/positives/",positive,"/10percent/data/data1",sep=""))
    fileName<-"sampletransfer251_145"
    data<-read.csv(paste(fileName,".csv",sep=""), stringsAsFactors =FALSE, header=FALSE)
    matrix<-matrix(0,nrow(data), 1000)
    classes<-matrix(0,nrow(data), 1)
    
    for(i in 1:nrow(data))
    {
      firstSplit<-strsplit(data[i,], split=":")
    
      secondSplit<-strsplit(firstSplit[[1]],split="\t")
      for(k in 2:length(secondSplit))
      {
        third<-as.numeric(unlist(secondSplit[[k]])[1])
        matrix[i,k-1]<-third
      }
      
      classes[i,1]<-as.numeric(unlist(secondSplit[[1]])[1])
    }
    data<-as.data.frame(matrix)
    
    setwd(paste("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/noise/attributenoise/",percent*100,"/data/data1",sep=""))
    noise(1, data, classes, matrix, fileName)
}

#-------FUNCTIONS-------
noise<-function(percent, data, classes, matrix, fileName)
{
  cols<-c(19, 37, 64, 68, 109, 162, 302, 320, 323, 335, 348, 384, 395, 445, 455, 497, 536, 575, 585, 622, 682, 741, 790, 802, 824, 859, 890, 893, 906, 996)
  
  sampleAttributes1<-sample(cols,length(cols)*percent)
  sampleAttributes2<-sample(cols,length(cols)*percent)
  sampleAttributes3<-sample(cols,length(cols)*percent)
  sampleAttributes4<-sample(cols,length(cols)*percent)
  sampleAttributes5<-sample(cols,length(cols)*percent)
  sampleAttributes6<-sample(cols,length(cols)*percent)
  sampleAttributes7<-sample(cols,length(cols)*percent)
  sampleAttributes8<-sample(cols,length(cols)*percent)
  sampleAttributes9<-sample(cols,length(cols)*percent)
  sampleAttributes10<-sample(cols,length(cols)*percent)
  
  meanp1_19<-NA;  meanp1_37<-NA;  meanp1_64<-NA;  meanp1_68<-NA;  meanp1_109<-NA;  meanp1_162<-NA;  meanp1_302<-NA;  meanp1_320<-NA;  meanp1_323<-NA;  meanp1_335<-NA; meanp1_348<-NA; meanp1_384<-NA;  meanp1_395<-NA;  meanp1_445<-NA;  meanp1_455<-NA;  meanp1_497<-NA;  meanp1_536<-NA;  meanp1_575<-NA;  meanp1_585<-NA;  meanp1_622<-NA;  meanp1_682<-NA;  meanp1_741<-NA;  meanp1_790<-NA;  meanp1_802<-NA;  meanp1_824<-NA;  meanp1_859<-NA;  meanp1_890<-NA;  meanp1_893<-NA;  meanp1_906<-NA;  meanp1_996<-NA
  meanp2_19<-NA;  meanp2_37<-NA;  meanp2_64<-NA;  meanp2_68<-NA;  meanp2_109<-NA;  meanp2_162<-NA;  meanp2_302<-NA;  meanp2_320<-NA;  meanp2_323<-NA;  meanp2_335<-NA; meanp2_348<-NA; meanp2_384<-NA;  meanp2_395<-NA;  meanp2_445<-NA;  meanp2_455<-NA;  meanp2_497<-NA;  meanp2_536<-NA;  meanp2_575<-NA;  meanp2_585<-NA;  meanp2_622<-NA;  meanp2_682<-NA;  meanp2_741<-NA;  meanp2_790<-NA;  meanp2_802<-NA;  meanp2_824<-NA;  meanp2_859<-NA;  meanp2_890<-NA;  meanp2_893<-NA;  meanp2_906<-NA;  meanp2_996<-NA
  meanp3_19<-NA;  meanp3_37<-NA;  meanp3_64<-NA;  meanp3_68<-NA;  meanp3_109<-NA;  meanp3_162<-NA;  meanp3_302<-NA;  meanp3_320<-NA;  meanp3_323<-NA;  meanp3_335<-NA; meanp3_348<-NA; meanp3_384<-NA;  meanp3_395<-NA;  meanp3_445<-NA;  meanp3_455<-NA;  meanp3_497<-NA;  meanp3_536<-NA;  meanp3_575<-NA;  meanp3_585<-NA;  meanp3_622<-NA;  meanp3_682<-NA;  meanp3_741<-NA;  meanp3_790<-NA;  meanp3_802<-NA;  meanp3_824<-NA;  meanp3_859<-NA;  meanp3_890<-NA;  meanp3_893<-NA;  meanp3_906<-NA;  meanp3_996<-NA
  meanm1_19<-NA;  meanm1_37<-NA;  meanm1_64<-NA;  meanm1_68<-NA;  meanm1_109<-NA;  meanm1_162<-NA;  meanm1_302<-NA;  meanm1_320<-NA;  meanm1_323<-NA;  meanm1_335<-NA; meanm1_348<-NA; meanm1_384<-NA;  meanm1_395<-NA;  meanm1_445<-NA;  meanm1_455<-NA;  meanm1_497<-NA;  meanm1_536<-NA;  meanm1_575<-NA;  meanm1_585<-NA;  meanm1_622<-NA;  meanm1_682<-NA;  meanm1_741<-NA;  meanm1_790<-NA;  meanm1_802<-NA;  meanm1_824<-NA;  meanm1_859<-NA;  meanm1_890<-NA;  meanm1_893<-NA;  meanm1_906<-NA;  meanm1_996<-NA
  meanm2_19<-NA;  meanm2_37<-NA;  meanm2_64<-NA;  meanm2_68<-NA;  meanm2_109<-NA;  meanm2_162<-NA;  meanm2_302<-NA;  meanm2_320<-NA;  meanm2_323<-NA;  meanm2_335<-NA; meanm2_348<-NA; meanm2_384<-NA;  meanm2_395<-NA;  meanm2_445<-NA;  meanm2_455<-NA;  meanm2_497<-NA;  meanm2_536<-NA;  meanm2_575<-NA;  meanm2_585<-NA;  meanm2_622<-NA;  meanm2_682<-NA;  meanm2_741<-NA;  meanm2_790<-NA;  meanm2_802<-NA;  meanm2_824<-NA;  meanm2_859<-NA;  meanm2_890<-NA;  meanm2_893<-NA;  meanm2_906<-NA;  meanm2_996<-NA
  meanm3_19<-NA;  meanm3_37<-NA;  meanm3_64<-NA;  meanm3_68<-NA;  meanm3_109<-NA;  meanm3_162<-NA;  meanm3_302<-NA;  meanm3_320<-NA;  meanm3_323<-NA;  meanm3_335<-NA; meanm3_348<-NA; meanm3_384<-NA;  meanm3_395<-NA;  meanm3_445<-NA;  meanm3_455<-NA;  meanm3_497<-NA;  meanm3_536<-NA;  meanm3_575<-NA;  meanm3_585<-NA;  meanm3_622<-NA;  meanm3_682<-NA;  meanm3_741<-NA;  meanm3_790<-NA;  meanm3_802<-NA;  meanm3_824<-NA;  meanm3_859<-NA;  meanm3_890<-NA;  meanm3_893<-NA;  meanm3_906<-NA;  meanm3_996<-NA
  
  if(19 %in% sampleAttributes1 || 19 %in% sampleAttributes2 || 19 %in% sampleAttributes3 || 19 %in% sampleAttributes4 || 19 %in% sampleAttributes5 || 19 %in% sampleAttributes6 || 19 %in% sampleAttributes7 || 19 %in% sampleAttributes8 || 19 %in% sampleAttributes9 || 19 %in% sampleAttributes10 )
  {
    #mean + 1 stdev
    meanp1_19<-rnorm(nrow(matrix)*percent, 2.241328, 2.747328)
    #mean + 2 stdev
    meanp2_19<-rnorm(nrow(matrix)*percent, 3.988656,	3.747328)
    #mean + 3 stdev
    meanp3_19<-rnorm(nrow(matrix)*percent, 5.735984,	4.747328)
    #mean - 1 stdev
    meanm1_19<-rnorm(nrow(matrix)*percent, -1.253328,	0.747328)
    #mean - 2 stdev
    meanm2_19<-rnorm(nrow(matrix)*percent, -3.000656,	0.252672)
    #mean - 3 stdev
    meanm3_19<-rnorm(nrow(matrix)*percent, -4.747984,	0.252672)
  }
  
  if(37 %in% sampleAttributes1 || 37 %in% sampleAttributes2 || 37 %in% sampleAttributes3 || 37 %in% sampleAttributes4 || 37 %in% sampleAttributes5 || 37 %in% sampleAttributes6 || 37 %in% sampleAttributes7 || 37 %in% sampleAttributes8 || 37 %in% sampleAttributes9 || 37 %in% sampleAttributes10 )
  {
    meanp1_37<-rnorm(nrow(matrix)*percent, 1.995121,	2.446021)
    meanp2_37<-rnorm(nrow(matrix)*percent, 3.441142,	3.446021)
    meanp3_37<-rnorm(nrow(matrix)*percent, 4.887163,	4.446021)
    meanm1_37<-rnorm(nrow(matrix)*percent, -0.896921,	0.446021)
    meanm2_37<-rnorm(nrow(matrix)*percent, -2.342942,	0.553979)
    meanm3_37<-rnorm(nrow(matrix)*percent, -3.788963,	1.553979)
  }
  
  if(64 %in% sampleAttributes1 || 64 %in% sampleAttributes2 || 64 %in% sampleAttributes3 || 64 %in% sampleAttributes4 || 64 %in% sampleAttributes5 || 64 %in% sampleAttributes6 || 64 %in% sampleAttributes7 || 64 %in% sampleAttributes8 || 64 %in% sampleAttributes9 || 64 %in% sampleAttributes10 )
  {
    meanp1_64<-rnorm(nrow(matrix)*percent, 2.068559,	2.605859)
    meanp2_64<-rnorm(nrow(matrix)*percent, 3.674418,	3.605859)
    meanp3_64<-rnorm(nrow(matrix)*percent, 5.280277,	4.605859)
    meanm1_64<-rnorm(nrow(matrix)*percent, -1.143159,	0.605859)
    meanm2_64<-rnorm(nrow(matrix)*percent, -2.749018,	0.394141)
    meanm3_64<-rnorm(nrow(matrix)*percent, -4.354877,	1.394141)
  }
    
  if(68 %in% sampleAttributes1 || 68 %in% sampleAttributes2 || 68 %in% sampleAttributes3 || 68 %in% sampleAttributes4 || 68 %in% sampleAttributes5 || 68 %in% sampleAttributes6 || 68 %in% sampleAttributes7 || 68 %in% sampleAttributes8 || 68 %in% sampleAttributes9 || 68 %in% sampleAttributes10 )
  {
    meanp1_68<-rnorm(nrow(matrix)*percent,1.1135554,	1.7619554)
    meanp2_68<-rnorm(nrow(matrix)*percent, 1.8755108,	2.7619554)
    meanp3_68<-rnorm(nrow(matrix)*percent, 2.6374662,	3.7619554)
    meanm1_68<-rnorm(nrow(matrix)*percent, -0.4103554,	0.2380446)
    meanm2_68<-rnorm(nrow(matrix)*percent, -1.1723108,	1.2380446)
    meanm3_68<-rnorm(nrow(matrix)*percent, -1.9342662,	2.2380446)
  }
  
  if(109 %in% sampleAttributes1 || 109 %in% sampleAttributes2 || 109 %in% sampleAttributes3 || 109 %in% sampleAttributes4 || 109 %in% sampleAttributes5 || 109 %in% sampleAttributes6 || 109 %in% sampleAttributes7 || 109 %in% sampleAttributes8 || 109 %in% sampleAttributes9 || 109 %in% sampleAttributes10 )
  {
    meanp1_109<-rnorm(nrow(matrix)*percent,4.172871, 3.970871)
    meanp2_109<-rnorm(nrow(matrix)*percent, 7.143742,	4.970871)
    meanp3_109<-rnorm(nrow(matrix)*percent, 10.114613,	5.970871)
    meanm1_109<-rnorm(nrow(matrix)*percent, -1.768871,	1.970871)
    meanm2_109<-rnorm(nrow(matrix)*percent, -4.739742,	0.970871)
    meanm3_109<-rnorm(nrow(matrix)*percent, -7.710613,	0.029129)
  }
  
  if(162 %in% sampleAttributes1 || 162 %in% sampleAttributes2 || 162 %in% sampleAttributes3 || 162 %in% sampleAttributes4 || 162 %in% sampleAttributes5 || 162 %in% sampleAttributes6 || 162 %in% sampleAttributes7 || 162 %in% sampleAttributes8 || 162 %in% sampleAttributes9 || 162 %in% sampleAttributes10 )
  {
    meanp1_162<-rnorm(nrow(matrix)*percent,2.480138,	2.908538)
    meanp2_162<-rnorm(nrow(matrix)*percent, 4.388676,	3.908538)
    meanp3_162<-rnorm(nrow(matrix)*percent, 6.297214,	4.908538)
    meanm1_162<-rnorm(nrow(matrix)*percent, -1.336938,	0.908538)
    meanm2_162<-rnorm(nrow(matrix)*percent, -3.245476,	0.091462)
    meanm3_162<-rnorm(nrow(matrix)*percent, -5.154014,	1.091462)
  }
  
  if(302 %in% sampleAttributes1 || 302 %in% sampleAttributes2 || 302 %in% sampleAttributes3 || 302 %in% sampleAttributes4 || 302 %in% sampleAttributes5 || 302 %in% sampleAttributes6 || 302 %in% sampleAttributes7 || 302 %in% sampleAttributes8 || 302 %in% sampleAttributes9 || 302 %in% sampleAttributes10 )
  {
    meanp1_302<-rnorm(nrow(matrix)*percent,2.94682,	3.18782)
    meanp2_302<-rnorm(nrow(matrix)*percent, 5.13464,	4.18782)
    meanp3_302<-rnorm(nrow(matrix)*percent, 7.32246,	5.18782)
    meanm1_302<-rnorm(nrow(matrix)*percent, -1.42882,	1.18782)
    meanm2_302<-rnorm(nrow(matrix)*percent, -3.61664,	0.18782)
    meanm3_302<-rnorm(nrow(matrix)*percent, -5.80446,	0.81218)
  }
  
  if(320 %in% sampleAttributes1 || 320 %in% sampleAttributes2 || 320 %in% sampleAttributes3 || 320 %in% sampleAttributes4 || 320 %in% sampleAttributes5 || 320 %in% sampleAttributes6 || 320 %in% sampleAttributes7 || 320 %in% sampleAttributes8 || 320 %in% sampleAttributes9 || 320 %in% sampleAttributes10 )
  {
    meanp1_320<-rnorm(nrow(matrix)*percent,2.915301,	3.359001)
    meanp2_320<-rnorm(nrow(matrix)*percent, 5.274302,	4.359001)
    meanp3_320<-rnorm(nrow(matrix)*percent, 7.633303,	5.359001)
    meanm1_320<-rnorm(nrow(matrix)*percent, -1.802701,	1.359001)
    meanm2_320<-rnorm(nrow(matrix)*percent, -4.161702,	0.359001)
    meanm3_320<-rnorm(nrow(matrix)*percent, -6.520703,	0.640999)
  }
  
  if(323 %in% sampleAttributes1 || 323 %in% sampleAttributes2 || 323 %in% sampleAttributes3 || 323 %in% sampleAttributes4 || 323 %in% sampleAttributes5 || 323 %in% sampleAttributes6 || 323 %in% sampleAttributes7 || 323 %in% sampleAttributes8 || 323 %in% sampleAttributes9 || 323 %in% sampleAttributes10 )
  {
    meanp1_323<-rnorm(nrow(matrix)*percent,3.1723,	3.6153)
    meanp2_323<-rnorm(nrow(matrix)*percent, 5.7876,	4.6153)
    meanp3_323<-rnorm(nrow(matrix)*percent, 8.4029,	5.6153)
    meanm1_323<-rnorm(nrow(matrix)*percent, -2.0583,	1.6153)
    meanm2_323<-rnorm(nrow(matrix)*percent, -4.6736,	0.6153)
    meanm3_323<-rnorm(nrow(matrix)*percent, -7.2889,	0.3847)
  }
  
  if(335 %in% sampleAttributes1 || 335 %in% sampleAttributes2 || 335 %in% sampleAttributes3 || 335 %in% sampleAttributes4 || 335 %in% sampleAttributes5 || 335 %in% sampleAttributes6 || 335 %in% sampleAttributes7 || 335 %in% sampleAttributes8 || 335 %in% sampleAttributes9 || 335 %in% sampleAttributes10 )
  {
    meanp1_335<-rnorm(nrow(matrix)*percent,1.965642,	2.428242)
    meanp2_335<-rnorm(nrow(matrix)*percent, 3.393884,	3.428242)
    meanp3_335<-rnorm(nrow(matrix)*percent, 4.822126,	4.428242)
    meanm1_335<-rnorm(nrow(matrix)*percent, -0.890842,	0.428242)
    meanm2_335<-rnorm(nrow(matrix)*percent, -2.319084,	0.571758)
    meanm3_335<-rnorm(nrow(matrix)*percent, -3.747326,	1.571758)
  }
  
  if(348 %in% sampleAttributes1 || 348 %in% sampleAttributes2 || 348 %in% sampleAttributes3 || 348 %in% sampleAttributes4 || 348 %in% sampleAttributes5 || 348 %in% sampleAttributes6 || 348 %in% sampleAttributes7 || 348 %in% sampleAttributes8 || 348 %in% sampleAttributes9 || 348 %in% sampleAttributes10 )
  {
    meanp1_348<-rnorm(nrow(matrix)*percent,3.637385,	3.912185)
    meanp2_348<-rnorm(nrow(matrix)*percent, 6.54957,	4.912185)
    meanp3_348<-rnorm(nrow(matrix)*percent, 9.461755,	5.912185)
    meanm1_348<-rnorm(nrow(matrix)*percent, -2.186985,	1.912185)
    meanm2_348<-rnorm(nrow(matrix)*percent, -5.09917,	0.912185)
    meanm3_348<-rnorm(nrow(matrix)*percent, -8.011355,	0.087815)
  }
  
  if(384 %in% sampleAttributes1 || 384 %in% sampleAttributes2 || 384 %in% sampleAttributes3 || 384 %in% sampleAttributes4 || 384 %in% sampleAttributes5 || 384 %in% sampleAttributes6 || 384 %in% sampleAttributes7 || 384 %in% sampleAttributes8 || 384 %in% sampleAttributes9 || 384 %in% sampleAttributes10)
  {
    meanp1_384<-rnorm(nrow(matrix)*percent,6.116086,	5.620086)
    meanp2_384<-rnorm(nrow(matrix)*percent, 10.736172,	6.620086)
    meanp3_384<-rnorm(nrow(matrix)*percent, 15.356258,	7.620086)
    meanm1_384<-rnorm(nrow(matrix)*percent, -3.124086,	3.620086)
    meanm2_384<-rnorm(nrow(matrix)*percent, -7.744172,	2.620086)
    meanm3_384<-rnorm(nrow(matrix)*percent, -12.364258,	1.620086)
  }
  
  if(395 %in% sampleAttributes1 || 395 %in% sampleAttributes2 || 395 %in% sampleAttributes3 || 395 %in% sampleAttributes4 || 395 %in% sampleAttributes5 || 395 %in% sampleAttributes6 || 395 %in% sampleAttributes7 || 395 %in% sampleAttributes8 || 395 %in% sampleAttributes9 || 395 %in% sampleAttributes10)
  {
    meanp1_395<-rnorm(nrow(matrix)*percent,2.024427,	2.464827)
    meanp2_395<-rnorm(nrow(matrix)*percent, 3.489254,	3.464827)
    meanp3_395<-rnorm(nrow(matrix)*percent, 4.954081,	4.464827)
    meanm1_395<-rnorm(nrow(matrix)*percent, -0.905227,	0.464827)
    meanm2_395<-rnorm(nrow(matrix)*percent, -2.370054,	0.535173)
    meanm3_395<-rnorm(nrow(matrix)*percent, -3.834881,	1.535173)
  }
  
  if(445 %in% sampleAttributes1 || 445 %in% sampleAttributes2 || 445 %in% sampleAttributes3 || 445 %in% sampleAttributes4 || 445 %in% sampleAttributes5 || 445 %in% sampleAttributes6 || 445 %in% sampleAttributes7 || 445 %in% sampleAttributes8 || 445 %in% sampleAttributes9 || 445 %in% sampleAttributes10)
  {
    meanp1_445<-rnorm(nrow(matrix)*percent,2.675794,	3.067894)
    meanp2_445<-rnorm(nrow(matrix)*percent, 4.743688,	4.067894)
    meanp3_445<-rnorm(nrow(matrix)*percent, 6.811582,	5.067894)
    meanm1_445<-rnorm(nrow(matrix)*percent, -1.459994,	1.067894)
    meanm2_445<-rnorm(nrow(matrix)*percent, -3.527888,	0.067894)
    meanm3_445<-rnorm(nrow(matrix)*percent, -5.595782,	0.932106)
  }
  
  if(455 %in% sampleAttributes1 || 455 %in% sampleAttributes2 || 455 %in% sampleAttributes3 || 455 %in% sampleAttributes4 || 455 %in% sampleAttributes5 || 455 %in% sampleAttributes6 || 455 %in% sampleAttributes7 || 455 %in% sampleAttributes8 || 455 %in% sampleAttributes9 || 455 %in% sampleAttributes10)
  {
    meanp1_455<-rnorm(nrow(matrix)*percent,2.615772,2.918372)
    meanp2_455<-rnorm(nrow(matrix)*percent, 4.534144,	3.918372)
    meanp3_455<-rnorm(nrow(matrix)*percent, 6.452516,	4.918372)
    meanm1_455<-rnorm(nrow(matrix)*percent, -1.220972,	0.918372)
    meanm2_455<-rnorm(nrow(matrix)*percent, -3.139344,	0.081628)
    meanm3_455<-rnorm(nrow(matrix)*percent, -5.057716,	1.081628)
  }
  
  if(497 %in% sampleAttributes1 || 497 %in% sampleAttributes2 || 497 %in% sampleAttributes3 || 497 %in% sampleAttributes4 || 497 %in% sampleAttributes5 || 497 %in% sampleAttributes6 || 497 %in% sampleAttributes7 || 497 %in% sampleAttributes8 || 497 %in% sampleAttributes9 || 497 %in% sampleAttributes10) 
  {
    meanp1_497<-rnorm(nrow(matrix)*percent,1.3652945,	1.9693945)
    meanp1_497<-rnorm(nrow(matrix)*percent, 2.334689,	2.9693945)
    meanp1_497<-rnorm(nrow(matrix)*percent, 3.3040835,	3.9693945)
    meanp1_497<-rnorm(nrow(matrix)*percent, -0.5734945,	0.0306055)
    meanp1_497<-rnorm(nrow(matrix)*percent, -1.542889,	1.0306055)
    meanp1_497<-rnorm(nrow(matrix)*percent, -2.5122835,	2.0306055)
  }
  
  if(536 %in% sampleAttributes1 || 536 %in% sampleAttributes2 || 536 %in% sampleAttributes3 || 536 %in% sampleAttributes4 || 536 %in% sampleAttributes5 || 536 %in% sampleAttributes6 || 536 %in% sampleAttributes7 || 536 %in% sampleAttributes8 || 536 %in% sampleAttributes9 || 536 %in% sampleAttributes10)
  {
    meanp1_536<-rnorm(nrow(matrix)*percent,1.401971,	2.040471)
    meanp2_536<-rnorm(nrow(matrix)*percent, 2.442442,	3.040471)
    meanp3_536<-rnorm(nrow(matrix)*percent, 3.482913,	4.040471)
    meanm1_536<-rnorm(nrow(matrix)*percent, -0.678971,	0.040471)
    meanm2_536<-rnorm(nrow(matrix)*percent, -1.719442,	0.959529)
    meanm3_536<-rnorm(nrow(matrix)*percent, -2.759913,	1.959529)
  }
  
  if(575 %in% sampleAttributes1 || 575 %in% sampleAttributes2 || 575 %in% sampleAttributes3 || 575 %in% sampleAttributes4 || 575 %in% sampleAttributes5 || 575 %in% sampleAttributes6 || 575 %in% sampleAttributes7 || 575 %in% sampleAttributes8 || 575 %in% sampleAttributes9 || 575 %in% sampleAttributes10)
  {
    meanp1_575<-rnorm(nrow(matrix)*percent,1.673754,	2.264754)
    meanp2_575<-rnorm(nrow(matrix)*percent, 2.938508,	3.264754)
    meanp3_575<-rnorm(nrow(matrix)*percent, 4.203262,	4.264754)
    meanm1_575<-rnorm(nrow(matrix)*percent, -0.855754,	0.264754)
    meanm2_575<-rnorm(nrow(matrix)*percent, -2.120508,	0.735246)
    meanm3_575<-rnorm(nrow(matrix)*percent, -3.385262,	1.735246)
  }
  
  if(585 %in% sampleAttributes1 || 585 %in% sampleAttributes2 || 585 %in% sampleAttributes3 || 585 %in% sampleAttributes4 || 585 %in% sampleAttributes5 || 585 %in% sampleAttributes6 || 585 %in% sampleAttributes7 || 585 %in% sampleAttributes8 || 585 %in% sampleAttributes9 || 585 %in% sampleAttributes10)
  {
    meanp1_585<-rnorm(nrow(matrix)*percent,2.545829,	2.942829)
    meanp2_585<-rnorm(nrow(matrix)*percent, 4.488658,	3.942829)
    meanp3_585<-rnorm(nrow(matrix)*percent, 6.431487,	4.942829)
    meanm1_585<-rnorm(nrow(matrix)*percent, -1.339829,	0.942829)
    meanm2_585<-rnorm(nrow(matrix)*percent, -3.282658,	0.057171)
    meanm3_585<-rnorm(nrow(matrix)*percent, -5.225487,	1.057171)
  }
  
  if(622 %in% sampleAttributes1 || 622 %in% sampleAttributes2 || 622 %in% sampleAttributes3 || 622 %in% sampleAttributes4 || 622 %in% sampleAttributes5 || 622 %in% sampleAttributes6 || 622 %in% sampleAttributes7 || 622 %in% sampleAttributes8 || 622 %in% sampleAttributes9 || 622 %in% sampleAttributes10)
  {
    meanp1_622<-rnorm(nrow(matrix)*percent,3.9376,	4.0231)
    meanp2_622<-rnorm(nrow(matrix)*percent, 6.9607,	5.0231)
    meanp3_622<-rnorm(nrow(matrix)*percent, 9.9838,	6.0231)
    meanm1_622<-rnorm(nrow(matrix)*percent, -2.1086,	2.0231)
    meanm2_622<-rnorm(nrow(matrix)*percent, -5.1317,	1.0231)
    meanm3_622<-rnorm(nrow(matrix)*percent, -8.1548,	0.0231)
  }
  
  if(682 %in% sampleAttributes1 || 682 %in% sampleAttributes2 || 682 %in% sampleAttributes3 || 682 %in% sampleAttributes4 || 682 %in% sampleAttributes5 || 682 %in% sampleAttributes6 || 682 %in% sampleAttributes7 || 682 %in% sampleAttributes8 || 682 %in% sampleAttributes9 || 682 %in% sampleAttributes10)
  {
    meanp1_682<-rnorm(nrow(matrix)*percent,2.32225,	2.78395)
    meanp2_682<-rnorm(nrow(matrix)*percent, 4.1062,	3.78395)
    meanp3_682<-rnorm(nrow(matrix)*percent, 5.89015,	4.78395)
    meanm1_682<-rnorm(nrow(matrix)*percent, -1.24565,	0.78395)
    meanm2_682<-rnorm(nrow(matrix)*percent, -4.81355,	1.21605)
    meanm3_682<-rnorm(nrow(matrix)*percent, -4.81355,	1.21605)
  }
  
  if(741 %in% sampleAttributes1 || 741 %in% sampleAttributes2 || 741 %in% sampleAttributes3 || 741 %in% sampleAttributes4 || 741 %in% sampleAttributes5 || 741 %in% sampleAttributes6 || 741 %in% sampleAttributes7 || 741 %in% sampleAttributes8 || 741 %in% sampleAttributes9 || 741 %in% sampleAttributes10)
  {
    meanp1_741<-rnorm(nrow(matrix)*percent,6.148502,	5.449502)
    meanp2_741<-rnorm(nrow(matrix)*percent, 10.598004,	6.449502)
    meanp3_741<-rnorm(nrow(matrix)*percent, 15.047506,	7.449502)
    meanm1_741<-rnorm(nrow(matrix)*percent, -2.750502,	3.449502)
    meanm2_741<-rnorm(nrow(matrix)*percent, -11.649506,	1.449502)
    meanm3_741<-rnorm(nrow(matrix)*percent, -11.649506,	1.449502)
  }
  
  if(790 %in% sampleAttributes1 || 790 %in% sampleAttributes2 || 790 %in% sampleAttributes3 || 790 %in% sampleAttributes4 || 790 %in% sampleAttributes5 || 790 %in% sampleAttributes6 || 790 %in% sampleAttributes7 || 790 %in% sampleAttributes8 || 790 %in% sampleAttributes9 || 790 %in% sampleAttributes10)
  {
    meanp1_790<-rnorm(nrow(matrix)*percent,1.652281,	2.189981)
    meanp2_790<-rnorm(nrow(matrix)*percent, 2.842262,	3.189981)
    meanp3_790<-rnorm(nrow(matrix)*percent, 4.032243,	4.189981)
    meanm1_790<-rnorm(nrow(matrix)*percent, -0.727681, 0.189981)
    meanm2_790<-rnorm(nrow(matrix)*percent, -1.917662,	0.810019)
    meanm3_790<-rnorm(nrow(matrix)*percent, -3.107643,	1.810019)
  }
  
  if(802 %in% sampleAttributes1 || 802 %in% sampleAttributes2 || 802 %in% sampleAttributes3 || 802 %in% sampleAttributes4 || 802 %in% sampleAttributes5 || 802 %in% sampleAttributes6 || 802 %in% sampleAttributes7 || 802 %in% sampleAttributes8 || 802 %in% sampleAttributes9 || 802 %in% sampleAttributes10)
  {
    meanp1_802<-rnorm(nrow(matrix)*percent,1.504062,	2.080362)
    meanp2_802<-rnorm(nrow(matrix)*percent, 2.584424,	3.080362)
    meanp3_802<-rnorm(nrow(matrix)*percent, 3.664786,	4.080362)
    meanm1_802<-rnorm(nrow(matrix)*percent, -0.656662,	0.080362)
    meanm2_802<-rnorm(nrow(matrix)*percent, -1.737024,	0.919638)
    meanm3_802<-rnorm(nrow(matrix)*percent, -2.817386,	1.919638)
  }
  
  if(824 %in% sampleAttributes1 || 824 %in% sampleAttributes2 || 824 %in% sampleAttributes3 || 824 %in% sampleAttributes4 || 824 %in% sampleAttributes5 || 824 %in% sampleAttributes6 || 824 %in% sampleAttributes7 || 824 %in% sampleAttributes8 || 824 %in% sampleAttributes9 || 824 %in% sampleAttributes10)
  {
    meanp1_824<-rnorm(nrow(matrix)*percent, 1.592239,	2.092839)
    meanp2_824<-rnorm(nrow(matrix)*percent, 2.685078,	3.092839)
    meanp3_824<-rnorm(nrow(matrix)*percent, 3.777917,	4.092839)
    meanm1_824<-rnorm(nrow(matrix)*percent, -0.593439,	0.092839)
    meanm2_824<-rnorm(nrow(matrix)*percent, -1.686278,	0.907161)
    meanm3_824<-rnorm(nrow(matrix)*percent, -2.779117,	1.907161)
  }
  
  if(859 %in% sampleAttributes1 || 859 %in% sampleAttributes2 || 859 %in% sampleAttributes3 || 859 %in% sampleAttributes4 || 859 %in% sampleAttributes5 || 859 %in% sampleAttributes6 || 859 %in% sampleAttributes7 || 859 %in% sampleAttributes8 || 859 %in% sampleAttributes9 || 859 %in% sampleAttributes10)
  {
    meanp1_859<-rnorm(nrow(matrix)*percent,4.250561,	4.169561)
    meanp2_859<-rnorm(nrow(matrix)*percent, 7.420122,	5.169561)
    meanp3_859<-rnorm(nrow(matrix)*percent, 10.589683,	6.169561)
    meanm1_859<-rnorm(nrow(matrix)*percent, -2.088561,	2.169561)
    meanm2_859<-rnorm(nrow(matrix)*percent, -5.258122,	1.169561)
    meanm3_859<-rnorm(nrow(matrix)*percent, -8.427683,	0.169561)
  }
  
  if(890 %in% sampleAttributes1 || 890 %in% sampleAttributes2 || 890 %in% sampleAttributes3 || 890 %in% sampleAttributes4 || 890 %in% sampleAttributes5 || 890 %in% sampleAttributes6 || 890 %in% sampleAttributes7 || 890 %in% sampleAttributes8 || 890 %in% sampleAttributes9 || 890 %in% sampleAttributes10)
  {
    meanp1_890<-rnorm(nrow(matrix)*percent,2.569013,	3.052213)
    meanp2_890<-rnorm(nrow(matrix)*percent, 4.621226,	4.052213)
    meanp3_890<-rnorm(nrow(matrix)*percent, 6.673439,	5.052213)
    meanm1_890<-rnorm(nrow(matrix)*percent, -1.535413,	1.052213)
    meanm2_890<-rnorm(nrow(matrix)*percent, -3.587626,	0.052213)
    meanm3_890<-rnorm(nrow(matrix)*percent, -5.639839,	0.947787)
  }
  
  if(893 %in% sampleAttributes1 || 893 %in% sampleAttributes2 || 893 %in% sampleAttributes3 || 893 %in% sampleAttributes4 || 893 %in% sampleAttributes5 || 893 %in% sampleAttributes6 || 893 %in% sampleAttributes7 || 893 %in% sampleAttributes8 || 893 %in% sampleAttributes9 || 893 %in% sampleAttributes10)
  {
    meanp1_893<-rnorm(nrow(matrix)*percent,11.41718,	11.19118)
    meanp2_893<-rnorm(nrow(matrix)*percent, 21.60836,	12.19118)
    meanp3_893<-rnorm(nrow(matrix)*percent, 31.79954,	13.19118)
    meanm1_893<-rnorm(nrow(matrix)*percent, -8.96518,	9.19118)
    meanm2_893<-rnorm(nrow(matrix)*percent, -19.15636,	8.19118)
    meanm3_893<-rnorm(nrow(matrix)*percent, -29.34754,	7.19118)
  }
  
  if(906 %in% sampleAttributes1 || 906 %in% sampleAttributes2 || 906 %in% sampleAttributes3 || 906 %in% sampleAttributes4 || 906 %in% sampleAttributes5 || 906 %in% sampleAttributes6 || 906 %in% sampleAttributes7 || 906 %in% sampleAttributes8 || 906 %in% sampleAttributes9 || 906 %in% sampleAttributes10)
  {
    meanp1_906<-rnorm(nrow(matrix)*percent,1.0998504,	1.7553504)
    meanp2_906<-rnorm(nrow(matrix)*percent, 1.8552008,	2.7553504)
    meanp3_906<-rnorm(nrow(matrix)*percent, 2.6105512,	3.7553504)
    meanp1_906<-rnorm(nrow(matrix)*percent, -0.4108504,	0.2446496)
    meanp2_906<-rnorm(nrow(matrix)*percent, -1.1662008,	1.2446496)
    meanp3_906<-rnorm(nrow(matrix)*percent, -1.9215512,	2.2446496)
  }
  
  if(996 %in% sampleAttributes1 || 996 %in% sampleAttributes2 || 996 %in% sampleAttributes3 || 996 %in% sampleAttributes4 || 996 %in% sampleAttributes5 || 996 %in% sampleAttributes6 || 996 %in% sampleAttributes7 || 996 %in% sampleAttributes8 || 996 %in% sampleAttributes9 || 996 %in% sampleAttributes10)
  {
    meanp1_996<-rnorm(nrow(matrix)*percent,2.59285,	2.95375)
    meanp2_996<-rnorm(nrow(matrix)*percent, 4.5466,	3.95375)
    meanp3_996<-rnorm(nrow(matrix)*percent, 6.50035,	4.95375)
    meanm1_996<-rnorm(nrow(matrix)*percent, -1.31465,	0.95375)
    meanm2_996<-rnorm(nrow(matrix)*percent, -3.2684,	0.04625)
    meanm3_996<-rnorm(nrow(matrix)*percent, -5.22215,	1.04625)
  }
  
  sampleInstances<-sample(1:nrow(data),nrow(data)*percent)
  numAttributes <- length(cols)*percent
  
  # datap1<-meanp1(data, classes, numAttributes, sampleInstances, meanp1_19, meanp1_37, meanp1_64, meanp1_68, meanp1_109, meanp1_162, meanp1_302, meanp1_320, meanp1_323, meanp1_335, meanp1_348, meanp1_384, meanp1_395, meanp1_445, meanp1_455, meanp1_497, meanp1_536, meanp1_575, meanp1_585, meanp1_622, meanp1_682, meanp1_741, meanp1_790, meanp1_802, meanp1_824, meanp1_859, meanp1_890, meanp1_893, meanp1_906, meanp1_996)
  # datap1<-getfinaldataCaltech(datap1)
  # write.table(datap1,paste(fileName, percent*100, "percent_p1", ".csv", sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  
  # datap2<-meanp2(data, classes, numAttributes, sampleInstances, meanp2_19, meanp2_37, meanp2_64, meanp2_68, meanp2_109, meanp2_162, meanp2_302, meanp2_320, meanp2_323, meanp2_335, meanp2_348, meanp2_384, meanp2_395, meanp2_445, meanp2_455, meanp2_497, meanp2_536, meanp2_575, meanp2_585, meanp2_622, meanp2_682, meanp2_741, meanp2_790, meanp2_802, meanp2_824, meanp2_859, meanp2_890, meanp2_893, meanp2_906, meanp2_996)
  # datap2<-getfinaldataCaltech(datap2)
  # write.table(datap2,paste(fileName, percent*100, "percent_p2",".csv", sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  # 
  datap3<-meanp3(data, classes, numAttributes,  sampleInstances, meanp3_19, meanp3_37, meanp3_64, meanp3_68, meanp3_109, meanp3_162, meanp3_302, meanp3_320, meanp3_323, meanp3_335, meanp3_348, meanp3_384, meanp3_395, meanp3_445, meanp3_455, meanp3_497, meanp3_536, meanp3_575, meanp3_585, meanp3_622, meanp3_682, meanp3_741, meanp3_790, meanp3_802, meanp3_824, meanp3_859, meanp3_890, meanp3_893, meanp3_906, meanp3_996)
  datap3<-getfinaldataCaltech(datap3)
  write.table(datap3,paste(fileName, percent*100, "percent_p3",".csv", sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  # 
  # datam1<-meanm1(data, classes, numAttributes, sampleInstances, meanm1_19, meanm1_37, meanm1_64, meanm1_68, meanm1_109, meanm1_162, meanm1_302, meanm1_320, meanm1_323, meanm1_335, meanm1_348, meanm1_384, meanm1_395, meanm1_445, meanm1_455, meanm1_497, meanm1_536, meanm1_575, meanm1_585, meanm1_622, meanm1_682, meanm1_741, meanm1_790, meanm1_802, meanm1_824, meanm1_859, meanm1_890, meanm1_893, meanm1_906, meanm1_996)
  # datam1<-getfinaldataCaltech(datam1)
  # write.table(datam1,paste(fileName, percent*100, "percent_m1",".csv", sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  # 
  # datam2<-meanm2(data, classes, numAttributes, sampleInstances, meanm2_19, meanm2_37, meanm2_64, meanm2_68, meanm2_109, meanm2_162, meanm2_302, meanm2_320, meanm2_323, meanm2_335, meanm2_348, meanm2_384, meanm2_395, meanm2_445, meanm2_455, meanm2_497, meanm2_536, meanm2_575, meanm2_585, meanm2_622, meanm2_682, meanm2_741, meanm2_790, meanm2_802, meanm2_824, meanm2_859, meanm2_890, meanm2_893, meanm2_906, meanm2_996)
  # datam2<-getfinaldataCaltech(datam2)
  # write.table(datam2,paste(fileName, percent*100, "percent_m2",".csv", sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  # 
  datam3<-meanm3(data, classes, numAttributes, sampleInstances, meanm3_19, meanm3_37, meanm3_64, meanm3_68, meanm3_109, meanm3_162, meanm3_302, meanm3_320, meanm3_323, meanm3_335, meanm3_348, meanm3_384, meanm3_395, meanm3_445, meanm3_455, meanm3_497, meanm3_536, meanm3_575, meanm3_585, meanm3_622, meanm3_682, meanm3_741, meanm3_790, meanm3_802, meanm3_824, meanm3_859, meanm3_890, meanm3_893, meanm3_906, meanm3_996)
  datam3<-getfinaldataCaltech(datam3)
  write.table(datam3,paste(fileName, percent*100, "percent_m3",".csv", sep=""),row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
}

meanp1<- function(data, classes, numAttributes, percent, sampleInstances, meanp1_19, meanp1_37, meanp1_64, meanp1_68, meanp1_109, meanp1_162, meanp1_302, meanp1_320, meanp1_323, meanp1_335, meanp1_348, meanp1_384, meanp1_395, meanp1_445, meanp1_455, meanp1_497, meanp1_536, meanp1_575, meanp1_585, meanp1_622, meanp1_682, meanp1_741, meanp1_790, meanp1_802, meanp1_824, meanp1_859, meanp1_890, meanp1_893, meanp1_906, meanp1_996)
{
  j19<-1; j37<-1; j64<-1; j68<-1; j109<-1; j162<-1; j302<-1; j320<-1; j323<-1; j335<-1; j348<-1; j384<-1; j395<-1; j445<-1; j455<-1; j497<-1; j536<-1; j575<-1; j585<-1; j622<-1; j682<-1; j741<-1; j790<-1; j802<-1; j824<-1; j859<-1; j890<-1; j893<-1; j906<-1; j996<-1
  
  for(i in 1:nrow(data))
  {
    if(i %in% sampleInstances)
    {
      numAttributesInstance <- 0
      randAttbs <- sample(1:30,30*percent)
      
      if(!is.na(meanp1_19) && numAttributesInstance < numAttributes && 1 %in% randAttbs) { data[i, 19]<-meanp1_19[j19]; j19<-j19 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_37) && numAttributesInstance < numAttributes && 2 %in% randAttbs) { data[i, 37]<-meanp1_37[j37]; j37<-j37 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_64) && numAttributesInstance < numAttributes && 3 %in% randAttbs) { data[i, 64]<-meanp1_64[j64]; j64<-j64 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_68) && numAttributesInstance < numAttributes && 4 %in% randAttbs) { data[i, 68]<-meanp1_68[j68]; j68<-j68 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_109) && numAttributesInstance < numAttributes && 5 %in% randAttbs) { data[i, 109]<-meanp1_109[j109]; j109<-j109 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_162) && numAttributesInstance < numAttributes && 6 %in% randAttbs) { data[i, 162]<-meanp1_162[j162]; j162<-j162 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_302) && numAttributesInstance < numAttributes && 7 %in% randAttbs) { data[i, 302]<-meanp1_302[j302]; j302<-j302 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_320) && numAttributesInstance < numAttributes && 8 %in% randAttbs) { data[i, 320]<-meanp1_320[j320]; j320<-j320 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_323) && numAttributesInstance < numAttributes && 9 %in% randAttbs) { data[i, 323]<-meanp1_323[j323]; j323<-j323 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_335) && numAttributesInstance < numAttributes && 10 %in% randAttbs) { data[i, 335]<-meanp1_335[j335]; j335<-j335 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_348) && numAttributesInstance < numAttributes && 11 %in% randAttbs) { data[i, 348]<-meanp1_348[j348]; j348<-j348 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_384) && numAttributesInstance < numAttributes && 12 %in% randAttbs) { data[i, 384]<-meanp1_384[j384]; j384<-j384 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_395) && numAttributesInstance < numAttributes && 13 %in% randAttbs) { data[i, 395]<-meanp1_395[j395]; j395<-j395 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_445) && numAttributesInstance < numAttributes && 14 %in% randAttbs) { data[i, 445]<-meanp1_445[j445]; j445<-j445 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_455) && numAttributesInstance < numAttributes && 15 %in% randAttbs) { data[i, 455]<-meanp1_455[j455]; j455<-j455 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_497) && numAttributesInstance < numAttributes && 16 %in% randAttbs) { data[i, 497]<-meanp1_497[j497]; j497<-j497 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_536) && numAttributesInstance < numAttributes && 17 %in% randAttbs) { data[i, 536]<-meanp1_536[j536]; j536<-j536 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_575) && numAttributesInstance < numAttributes && 18 %in% randAttbs) { data[i, 575]<-meanp1_575[j575]; j575<-j575 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_585) && numAttributesInstance < numAttributes && 19 %in% randAttbs) { data[i, 585]<-meanp1_585[j585]; j585<-j585 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_622) && numAttributesInstance < numAttributes && 20 %in% randAttbs) { data[i, 622]<-meanp1_622[j622]; j622<-j622 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_682) && numAttributesInstance < numAttributes && 21 %in% randAttbs) { data[i, 682]<-meanp1_682[j682]; j682<-j682 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_741) && numAttributesInstance < numAttributes && 22 %in% randAttbs) { data[i, 741]<-meanp1_741[j741]; j741<-j741 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_790) && numAttributesInstance < numAttributes && 23 %in% randAttbs) { data[i, 790]<-meanp1_790[j790]; j790<-j790 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_802) && numAttributesInstance < numAttributes && 24 %in% randAttbs) { data[i, 802]<-meanp1_802[j802]; j802<-j802 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_824) && numAttributesInstance < numAttributes && 25 %in% randAttbs) { data[i, 824]<-meanp1_824[j824]; j824<-j824 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_859) && numAttributesInstance < numAttributes && 26 %in% randAttbs) { data[i, 859]<-meanp1_859[j859]; j859<-j859 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_890) && numAttributesInstance < numAttributes && 27 %in% randAttbs) { data[i, 890]<-meanp1_890[j890]; j890<-j890 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_893) && numAttributesInstance < numAttributes && 28 %in% randAttbs) { data[i, 893]<-meanp1_893[j893]; j893<-j893 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_906) && numAttributesInstance < numAttributes && 29 %in% randAttbs) { data[i, 906]<-meanp1_906[j906]; j906<-j906 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp1_996) && numAttributesInstance < numAttributes && 30 %in% randAttbs) { data[i, 996]<-meanp1_996[j996]; j996<-j996 + 1; numAttributesInstance <- numAttributesInstance + 1}
    }
  }
  data<-cbind(data, classes)
  data
}

meanp2<- function(data, classes, numAttributes, percent, sampleInstances, meanp2_19, meanp2_37, meanp2_64, meanp2_68, meanp2_109, meanp2_162, meanp2_302, meanp2_320, meanp2_323, meanp2_335, meanp2_348, meanp2_384, meanp2_395, meanp2_445, meanp2_455, meanp2_497, meanp2_536, meanp2_575, meanp2_585, meanp2_622, meanp2_682, meanp2_741, meanp2_790, meanp2_802, meanp2_824, meanp2_859, meanp2_890, meanp2_893, meanp2_906, meanp2_996)
{
  j19<-1; j37<-1; j64<-1; j68<-1; j109<-1; j162<-1; j302<-1; j320<-1; j323<-1; j335<-1; j348<-1; j384<-1; j395<-1; j445<-1; j455<-1; j497<-1; j536<-1; j575<-1; j585<-1; j622<-1; j682<-1; j741<-1; j790<-1; j802<-1; j824<-1; j859<-1; j890<-1; j893<-1; j906<-1; j996<-1
  
  for(i in 1:nrow(data))
  {
    if(i %in% sampleInstances)
    {
      if(!is.na(meanp2_19)) { data[i, 19]<-meanp2_19[j19]; j19<-j19 + 1}
      if(!is.na(meanp2_37)) { data[i, 37]<-meanp2_37[j37]; j37<-j37 + 1}
      if(!is.na(meanp2_64)) { data[i, 64]<-meanp2_64[j64]; j64<-j64 + 1}
      if(!is.na(meanp2_68)) { data[i, 68]<-meanp2_68[j68]; j68<-j68 + 1}
      if(!is.na(meanp2_109)) { data[i, 109]<-meanp2_109[j109]; j109<-j109 + 1}
      if(!is.na(meanp2_162)) { data[i, 162]<-meanp2_162[j162]; j162<-j162 + 1}
      if(!is.na(meanp2_302)) { data[i, 302]<-meanp2_302[j302]; j302<-j302 + 1}
      if(!is.na(meanp2_320)) { data[i, 320]<-meanp2_320[j320]; j320<-j320 + 1}
      if(!is.na(meanp2_323)) { data[i, 323]<-meanp2_323[j323]; j323<-j323 + 1}
      if(!is.na(meanp2_335)) { data[i, 335]<-meanp2_335[j335]; j335<-j335 + 1}
      if(!is.na(meanp2_348)) { data[i, 348]<-meanp2_348[j348]; j348<-j348 + 1}
      if(!is.na(meanp2_384)) { data[i, 384]<-meanp2_384[j384]; j384<-j384 + 1}
      if(!is.na(meanp2_395)) { data[i, 395]<-meanp2_395[j395]; j395<-j395 + 1}
      if(!is.na(meanp2_445)) { data[i, 445]<-meanp2_445[j445]; j445<-j445 + 1}
      if(!is.na(meanp2_455)) { data[i, 455]<-meanp2_455[j455]; j455<-j455 + 1}
      if(!is.na(meanp2_497)) { data[i, 497]<-meanp2_497[j497]; j497<-j497 + 1}
      if(!is.na(meanp2_536)) { data[i, 536]<-meanp2_536[j536]; j536<-j536 + 1}
      if(!is.na(meanp2_575)) { data[i, 575]<-meanp2_575[j575]; j575<-j575 + 1}
      if(!is.na(meanp2_585)) { data[i, 585]<-meanp2_585[j585]; j585<-j585 + 1}
      if(!is.na(meanp2_622)) { data[i, 622]<-meanp2_622[j622]; j622<-j622 + 1}
      if(!is.na(meanp2_682)) { data[i, 682]<-meanp2_682[j682]; j682<-j682 + 1}
      if(!is.na(meanp2_741)) { data[i, 741]<-meanp2_741[j741]; j741<-j741 + 1}
      if(!is.na(meanp2_790)) { data[i, 790]<-meanp2_790[j790]; j790<-j790 + 1}
      if(!is.na(meanp2_802)) { data[i, 802]<-meanp2_802[j802]; j802<-j802 + 1}
      if(!is.na(meanp2_824)) { data[i, 824]<-meanp2_824[j824]; j824<-j824 + 1}
      if(!is.na(meanp2_859)) { data[i, 859]<-meanp2_859[j859]; j859<-j859 + 1}
      if(!is.na(meanp2_890)) { data[i, 890]<-meanp2_890[j890]; j890<-j890 + 1}
      if(!is.na(meanp2_893)) { data[i, 893]<-meanp2_893[j893]; j893<-j893 + 1}
      if(!is.na(meanp2_906)) { data[i, 906]<-meanp2_906[j906]; j906<-j906 + 1}
      if(!is.na(meanp2_996)) { data[i, 996]<-meanp2_996[j996]; j996<-j996 + 1}
    }
  }
  data<-cbind(data, classes)
  data
}

meanp3<- function(data, classes, numAttributes, percent, sampleInstances, meanp3_19, meanp3_37, meanp3_64, meanp3_68, meanp3_109, meanp3_162, meanp3_302, meanp3_320, meanp3_323, meanp3_335, meanp3_348, meanp3_384, meanp3_395, meanp3_445, meanp3_455, meanp3_497, meanp3_536, meanp3_575, meanp3_585, meanp3_622, meanp3_682, meanp3_741, meanp3_790, meanp3_802, meanp3_824, meanp3_859, meanp3_890, meanp3_893, meanp3_906, meanp3_996)
{
  j19<-1; j37<-1; j64<-1; j68<-1; j109<-1; j162<-1; j302<-1; j320<-1; j323<-1; j335<-1; j348<-1; j384<-1; j395<-1; j445<-1; j455<-1; j497<-1; j536<-1; j575<-1; j585<-1; j622<-1; j682<-1; j741<-1; j790<-1; j802<-1; j824<-1; j859<-1; j890<-1; j893<-1; j906<-1; j996<-1
  
  for(i in 1:nrow(data))
  {
    if(i %in% sampleInstances)
    {
      numAttributesInstance <- 0
      randAttbs <- sample(1:30,30*percent)
      
      if(!is.na(meanp3_19) && numAttributesInstance < numAttributes && 1 %in% randAttbs) { data[i, 19]<-meanp3_19[j19]; j19<-j19 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_37) && numAttributesInstance < numAttributes && 2 %in% randAttbs) { data[i, 37]<-meanp3_37[j37]; j37<-j37 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_64) && numAttributesInstance < numAttributes && 3 %in% randAttbs) { data[i, 64]<-meanp3_64[j64]; j64<-j64 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_68) && numAttributesInstance < numAttributes && 4 %in% randAttbs) { data[i, 68]<-meanp3_68[j68]; j68<-j68 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_109) && numAttributesInstance < numAttributes && 5 %in% randAttbs) { data[i, 109]<-meanp3_109[j109]; j109<-j109 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_162) && numAttributesInstance < numAttributes && 6 %in% randAttbs) { data[i, 162]<-meanp3_162[j162]; j162<-j162 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_302) && numAttributesInstance < numAttributes && 7 %in% randAttbs) { data[i, 302]<-meanp3_302[j302]; j302<-j302 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_320) && numAttributesInstance < numAttributes && 8 %in% randAttbs) { data[i, 320]<-meanp3_320[j320]; j320<-j320 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_323) && numAttributesInstance < numAttributes && 9 %in% randAttbs) { data[i, 323]<-meanp3_323[j323]; j323<-j323 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_335) && numAttributesInstance < numAttributes && 10 %in% randAttbs) { data[i, 335]<-meanp3_335[j335]; j335<-j335 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_348) && numAttributesInstance < numAttributes && 11 %in% randAttbs) { data[i, 348]<-meanp3_348[j348]; j348<-j348 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_384) && numAttributesInstance < numAttributes && 12 %in% randAttbs) { data[i, 384]<-meanp3_384[j384]; j384<-j384 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_395) && numAttributesInstance < numAttributes && 13 %in% randAttbs) { data[i, 395]<-meanp3_395[j395]; j395<-j395 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_445) && numAttributesInstance < numAttributes && 14 %in% randAttbs) { data[i, 445]<-meanp3_445[j445]; j445<-j445 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_455) && numAttributesInstance < numAttributes && 15 %in% randAttbs) { data[i, 455]<-meanp3_455[j455]; j455<-j455 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_497) && numAttributesInstance < numAttributes && 16 %in% randAttbs) { data[i, 497]<-meanp3_497[j497]; j497<-j497 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_536) && numAttributesInstance < numAttributes && 17 %in% randAttbs) { data[i, 536]<-meanp3_536[j536]; j536<-j536 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_575) && numAttributesInstance < numAttributes && 18 %in% randAttbs) { data[i, 575]<-meanp3_575[j575]; j575<-j575 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_585) && numAttributesInstance < numAttributes && 19 %in% randAttbs) { data[i, 585]<-meanp3_585[j585]; j585<-j585 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_622) && numAttributesInstance < numAttributes && 20 %in% randAttbs) { data[i, 622]<-meanp3_622[j622]; j622<-j622 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_682) && numAttributesInstance < numAttributes && 21 %in% randAttbs) { data[i, 682]<-meanp3_682[j682]; j682<-j682 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_741) && numAttributesInstance < numAttributes && 22 %in% randAttbs) { data[i, 741]<-meanp3_741[j741]; j741<-j741 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_790) && numAttributesInstance < numAttributes && 23 %in% randAttbs) { data[i, 790]<-meanp3_790[j790]; j790<-j790 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_802) && numAttributesInstance < numAttributes && 24 %in% randAttbs) { data[i, 802]<-meanp3_802[j802]; j802<-j802 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_824) && numAttributesInstance < numAttributes && 25 %in% randAttbs) { data[i, 824]<-meanp3_824[j824]; j824<-j824 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_859) && numAttributesInstance < numAttributes && 26 %in% randAttbs) { data[i, 859]<-meanp3_859[j859]; j859<-j859 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_890) && numAttributesInstance < numAttributes && 27 %in% randAttbs) { data[i, 890]<-meanp3_890[j890]; j890<-j890 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_893) && numAttributesInstance < numAttributes && 28 %in% randAttbs) { data[i, 893]<-meanp3_893[j893]; j893<-j893 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_906) && numAttributesInstance < numAttributes && 29 %in% randAttbs) { data[i, 906]<-meanp3_906[j906]; j906<-j906 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanp3_996) && numAttributesInstance < numAttributes && 30 %in% randAttbs) { data[i, 996]<-meanp3_996[j996]; j996<-j996 + 1; numAttributesInstance <- numAttributesInstance + 1}
    }
  }
  data<-cbind(data, classes)
  data
}

meanm1<- function(data, classes, numAttributes, percent, sampleInstances, meanm1_19, meanm1_37, meanm1_64, meanm1_68, meanm1_109, meanm1_162, meanm1_302, meanm1_320, meanm1_323, meanm1_335, meanm1_348, meanm1_384, meanm1_395, meanm1_445, meanm1_455, meanm1_497, meanm1_536, meanm1_575, meanm1_585, meanm1_622, meanm1_682, meanm1_741, meanm1_790, meanm1_802, meanm1_824, meanm1_859, meanm1_890, meanm1_893, meanm1_906, meanm1_996)
{
  j19<-1; j37<-1; j64<-1; j68<-1; j109<-1; j162<-1; j302<-1; j320<-1; j323<-1; j335<-1; j348<-1; j384<-1; j395<-1; j445<-1; j455<-1; j497<-1; j536<-1; j575<-1; j585<-1; j622<-1; j682<-1; j741<-1; j790<-1; j802<-1; j824<-1; j859<-1; j890<-1; j893<-1; j906<-1; j996<-1
  
  for(i in 1:nrow(data))
  {
    if(i %in% sampleInstances)
    {
      if(!is.na(meanm1_19)) { data[i, 19]<-meanm1_19[j19]; j19<-j19 + 1}
      if(!is.na(meanm1_37)) { data[i, 37]<-meanm1_37[j37]; j37<-j37 + 1}
      if(!is.na(meanm1_64)) { data[i, 64]<-meanm1_64[j64]; j64<-j64 + 1}
      if(!is.na(meanm1_68)) { data[i, 68]<-meanm1_68[j68]; j68<-j68 + 1}
      if(!is.na(meanm1_109)) { data[i, 109]<-meanm1_109[j109]; j109<-j109 + 1}
      if(!is.na(meanm1_162)) { data[i, 162]<-meanm1_162[j162]; j162<-j162 + 1}
      if(!is.na(meanm1_302)) { data[i, 302]<-meanm1_302[j302]; j302<-j302 + 1}
      if(!is.na(meanm1_320)) { data[i, 320]<-meanm1_320[j320]; j320<-j320 + 1}
      if(!is.na(meanm1_323)) { data[i, 323]<-meanm1_323[j323]; j323<-j323 + 1}
      if(!is.na(meanm1_335)) { data[i, 335]<-meanm1_335[j335]; j335<-j335 + 1}
      if(!is.na(meanm1_348)) { data[i, 348]<-meanm1_348[j348]; j348<-j348 + 1}
      if(!is.na(meanm1_384)) { data[i, 384]<-meanm1_384[j384]; j384<-j384 + 1}
      if(!is.na(meanm1_395)) { data[i, 395]<-meanm1_395[j395]; j395<-j395 + 1}
      if(!is.na(meanm1_445)) { data[i, 445]<-meanm1_445[j445]; j445<-j445 + 1}
      if(!is.na(meanm1_455)) { data[i, 455]<-meanm1_455[j455]; j455<-j455 + 1}
      if(!is.na(meanm1_497)) { data[i, 497]<-meanm1_497[j497]; j497<-j497 + 1}
      if(!is.na(meanm1_536)) { data[i, 536]<-meanm1_536[j536]; j536<-j536 + 1}
      if(!is.na(meanm1_575)) { data[i, 575]<-meanm1_575[j575]; j575<-j575 + 1}
      if(!is.na(meanm1_585)) { data[i, 585]<-meanm1_585[j585]; j585<-j585 + 1}
      if(!is.na(meanm1_622)) { data[i, 622]<-meanm1_622[j622]; j622<-j622 + 1}
      if(!is.na(meanm1_682)) { data[i, 682]<-meanm1_682[j682]; j682<-j682 + 1}
      if(!is.na(meanm1_741)) { data[i, 741]<-meanm1_741[j741]; j741<-j741 + 1}
      if(!is.na(meanm1_790)) { data[i, 790]<-meanm1_790[j790]; j790<-j790 + 1}
      if(!is.na(meanm1_802)) { data[i, 802]<-meanm1_802[j802]; j802<-j802 + 1}
      if(!is.na(meanm1_824)) { data[i, 824]<-meanm1_824[j824]; j824<-j824 + 1}
      if(!is.na(meanm1_859)) { data[i, 859]<-meanm1_859[j859]; j859<-j859 + 1}
      if(!is.na(meanm1_890)) { data[i, 890]<-meanm1_890[j890]; j890<-j890 + 1}
      if(!is.na(meanm1_893)) { data[i, 893]<-meanm1_893[j893]; j893<-j893 + 1}
      if(!is.na(meanm1_906)) { data[i, 906]<-meanm1_906[j906]; j906<-j906 + 1}
      if(!is.na(meanm1_996)) { data[i, 996]<-meanm1_996[j996]; j996<-j996 + 1}
    }
  }
  data<-cbind(data, classes)
  data
}

meanm2<- function(data, classes, numAttributes, percent,  sampleInstances, meanm2_19, meanm2_37, meanm2_64, meanm2_68, meanm2_109, meanm2_162, meanm2_302, meanm2_320, meanm2_323, meanm2_335, meanm2_348, meanm2_384, meanm2_395, meanm2_445, meanm2_455, meanm2_497, meanm2_536, meanm2_575, meanm2_585, meanm2_622, meanm2_682, meanm2_741, meanm2_790, meanm2_802, meanm2_824, meanm2_859, meanm2_890, meanm2_893, meanm2_906, meanm2_996)
{
  j19<-1; j37<-1; j64<-1; j68<-1; j109<-1; j162<-1; j302<-1; j320<-1; j323<-1; j335<-1; j348<-1; j384<-1; j395<-1; j445<-1; j455<-1; j497<-1; j536<-1; j575<-1; j585<-1; j622<-1; j682<-1; j741<-1; j790<-1; j802<-1; j824<-1; j859<-1; j890<-1; j893<-1; j906<-1; j996<-1
  
  for(i in 1:nrow(data))
  {
    if(i %in% sampleInstances)
    {
      if(!is.na(meanm2_19)) { data[i, 19]<-meanm2_19[j19]; j19<-j19 + 1}
      if(!is.na(meanm2_37)) { data[i, 37]<-meanm2_37[j37]; j37<-j37 + 1}
      if(!is.na(meanm2_64)) { data[i, 64]<-meanm2_64[j64]; j64<-j64 + 1}
      if(!is.na(meanm2_68)) { data[i, 68]<-meanm2_68[j68]; j68<-j68 + 1}
      if(!is.na(meanm2_109)) { data[i, 109]<-meanm2_109[j109]; j109<-j109 + 1}
      if(!is.na(meanm2_162)) { data[i, 162]<-meanm2_162[j162]; j162<-j162 + 1}
      if(!is.na(meanm2_302)) { data[i, 302]<-meanm2_302[j302]; j302<-j302 + 1}
      if(!is.na(meanm2_320)) { data[i, 320]<-meanm2_320[j320]; j320<-j320 + 1}
      if(!is.na(meanm2_323)) { data[i, 323]<-meanm2_323[j323]; j323<-j323 + 1}
      if(!is.na(meanm2_335)) { data[i, 335]<-meanm2_335[j335]; j335<-j335 + 1}
      if(!is.na(meanm2_348)) { data[i, 348]<-meanm2_348[j348]; j348<-j348 + 1}
      if(!is.na(meanm2_384)) { data[i, 384]<-meanm2_384[j384]; j384<-j384 + 1}
      if(!is.na(meanm2_395)) { data[i, 395]<-meanm2_395[j395]; j395<-j395 + 1}
      if(!is.na(meanm2_445)) { data[i, 445]<-meanm2_445[j445]; j445<-j445 + 1}
      if(!is.na(meanm2_455)) { data[i, 455]<-meanm2_455[j455]; j455<-j455 + 1}
      if(!is.na(meanm2_497)) { data[i, 497]<-meanm2_497[j497]; j497<-j497 + 1}
      if(!is.na(meanm2_536)) { data[i, 536]<-meanm2_536[j536]; j536<-j536 + 1}
      if(!is.na(meanm2_575)) { data[i, 575]<-meanm2_575[j575]; j575<-j575 + 1}
      if(!is.na(meanm2_585)) { data[i, 585]<-meanm2_585[j585]; j585<-j585 + 1}
      if(!is.na(meanm2_622)) { data[i, 622]<-meanm2_622[j622]; j622<-j622 + 1}
      if(!is.na(meanm2_682)) { data[i, 682]<-meanm2_682[j682]; j682<-j682 + 1}
      if(!is.na(meanm2_741)) { data[i, 741]<-meanm2_741[j741]; j741<-j741 + 1}
      if(!is.na(meanm2_790)) { data[i, 790]<-meanm2_790[j790]; j790<-j790 + 1}
      if(!is.na(meanm2_802)) { data[i, 802]<-meanm2_802[j802]; j802<-j802 + 1}
      if(!is.na(meanm2_824)) { data[i, 824]<-meanm2_824[j824]; j824<-j824 + 1}
      if(!is.na(meanm2_859)) { data[i, 859]<-meanm2_859[j859]; j859<-j859 + 1}
      if(!is.na(meanm2_890)) { data[i, 890]<-meanm2_890[j890]; j890<-j890 + 1}
      if(!is.na(meanm2_893)) { data[i, 893]<-meanm2_893[j893]; j893<-j893 + 1}
      if(!is.na(meanm2_906)) { data[i, 906]<-meanm2_906[j906]; j906<-j906 + 1}
      if(!is.na(meanm2_996)) { data[i, 996]<-meanm2_996[j996]; j996<-j996 + 1}
    }
  }
  data<-cbind(data, classes)
  data
}

meanm3<- function(data, classes, numAttributes, percent, sampleInstances, meanm3_19, meanm3_37, meanm3_64, meanm3_68, meanm3_109, meanm3_162, meanm3_302, meanm3_320, meanm3_323, meanm3_335, meanm3_348, meanm3_384, meanm3_395, meanm3_445, meanm3_455, meanm3_497, meanm3_536, meanm3_575, meanm3_585, meanm3_622, meanm3_682, meanm3_741, meanm3_790, meanm3_802, meanm3_824, meanm3_859, meanm3_890, meanm3_893, meanm3_906, meanm3_996)
{
  j19<-1; j37<-1; j64<-1; j68<-1; j109<-1; j162<-1; j302<-1; j320<-1; j323<-1; j335<-1; j348<-1; j384<-1; j395<-1; j445<-1; j455<-1; j497<-1; j536<-1; j575<-1; j585<-1; j622<-1; j682<-1; j741<-1; j790<-1; j802<-1; j824<-1; j859<-1; j890<-1; j893<-1; j906<-1; j996<-1
  
  for(i in 1:nrow(data))
  {
    if(i %in% sampleInstances)
    {
      numAttributesInstance <- 0
      randAttbs <- sample(1:30,30*percent)
      
      if(!is.na(meanm3_19) && numAttributesInstance < numAttributes && 1 %in% randAttbs) { data[i, 19]<-meanm3_19[j19]; j19<-j19 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_37) && numAttributesInstance < numAttributes && 2 %in% randAttbs) { data[i, 37]<-meanm3_37[j37]; j37<-j37 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_64) && numAttributesInstance < numAttributes && 3 %in% randAttbs) { data[i, 64]<-meanm3_64[j64]; j64<-j64 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_68) && numAttributesInstance < numAttributes && 4 %in% randAttbs) { data[i, 68]<-meanm3_68[j68]; j68<-j68 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_109) && numAttributesInstance < numAttributes && 5 %in% randAttbs) { data[i, 109]<-meanm3_109[j109]; j109<-j109 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_162) && numAttributesInstance < numAttributes && 6 %in% randAttbs) { data[i, 162]<-meanm3_162[j162]; j162<-j162 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_302) && numAttributesInstance < numAttributes && 7 %in% randAttbs) { data[i, 302]<-meanm3_302[j302]; j302<-j302 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_320) && numAttributesInstance < numAttributes && 8 %in% randAttbs) { data[i, 320]<-meanm3_320[j320]; j320<-j320 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_323) && numAttributesInstance < numAttributes && 9 %in% randAttbs) { data[i, 323]<-meanm3_323[j323]; j323<-j323 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_335) && numAttributesInstance < numAttributes && 10 %in% randAttbs) { data[i, 335]<-meanm3_335[j335]; j335<-j335 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_348) && numAttributesInstance < numAttributes && 11 %in% randAttbs) { data[i, 348]<-meanm3_348[j348]; j348<-j348 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_384) && numAttributesInstance < numAttributes && 12 %in% randAttbs) { data[i, 384]<-meanm3_384[j384]; j384<-j384 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_395) && numAttributesInstance < numAttributes && 13 %in% randAttbs) { data[i, 395]<-meanm3_395[j395]; j395<-j395 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_445) && numAttributesInstance < numAttributes && 14 %in% randAttbs) { data[i, 445]<-meanm3_445[j445]; j445<-j445 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_455) && numAttributesInstance < numAttributes && 15 %in% randAttbs) { data[i, 455]<-meanm3_455[j455]; j455<-j455 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_497) && numAttributesInstance < numAttributes && 16 %in% randAttbs) { data[i, 497]<-meanm3_497[j497]; j497<-j497 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_536) && numAttributesInstance < numAttributes && 17 %in% randAttbs) { data[i, 536]<-meanm3_536[j536]; j536<-j536 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_575) && numAttributesInstance < numAttributes && 18 %in% randAttbs) { data[i, 575]<-meanm3_575[j575]; j575<-j575 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_585) && numAttributesInstance < numAttributes && 19 %in% randAttbs) { data[i, 585]<-meanm3_585[j585]; j585<-j585 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_622) && numAttributesInstance < numAttributes && 20 %in% randAttbs) { data[i, 622]<-meanm3_622[j622]; j622<-j622 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_682) && numAttributesInstance < numAttributes && 21 %in% randAttbs) { data[i, 682]<-meanm3_682[j682]; j682<-j682 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_741) && numAttributesInstance < numAttributes && 22 %in% randAttbs) { data[i, 741]<-meanm3_741[j741]; j741<-j741 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_790) && numAttributesInstance < numAttributes && 23 %in% randAttbs) { data[i, 790]<-meanm3_790[j790]; j790<-j790 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_802) && numAttributesInstance < numAttributes && 24 %in% randAttbs) { data[i, 802]<-meanm3_802[j802]; j802<-j802 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_824) && numAttributesInstance < numAttributes && 25 %in% randAttbs) { data[i, 824]<-meanm3_824[j824]; j824<-j824 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_859) && numAttributesInstance < numAttributes && 26 %in% randAttbs) { data[i, 859]<-meanm3_859[j859]; j859<-j859 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_890) && numAttributesInstance < numAttributes && 27 %in% randAttbs) { data[i, 890]<-meanm3_890[j890]; j890<-j890 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_893) && numAttributesInstance < numAttributes && 28 %in% randAttbs) { data[i, 893]<-meanm3_893[j893]; j893<-j893 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_906) && numAttributesInstance < numAttributes && 29 %in% randAttbs) { data[i, 906]<-meanm3_906[j906]; j906<-j906 + 1; numAttributesInstance <- numAttributesInstance + 1}
      if(!is.na(meanm3_996) && numAttributesInstance < numAttributes && 30 %in% randAttbs) { data[i, 996]<-meanm3_996[j996]; j996<-j996 + 1; numAttributesInstance <- numAttributesInstance + 1}
    }
  }
  data<-cbind(data, classes)
  data
}


fixFilesNoise<-function(percent)
{
  setwd("C:/Users/dben652/workspace/AccGenSVM/data/caltech256/noise/attributenoise")
  i<-1
  for(k in 1:10)
  {
    filenames<-NULL
    # filenames[i]<-paste(percent*100,"/data/data",k,"/sampletransfer251_145",percent*100,"percent_m1.csv",sep="")
    # i<-i+1
    # filenames[i]<-paste(percent*100,"/data/data",k,"/sampletransfer251_145",percent*100,"percent_m2.csv",sep="")
    # i<-i+1
    filenames[i]<-paste(percent*100,"/data/data",k,"/sampletransfer251_145",percent*100,"percent_m3.csv",sep="")
    i<-i+1
    # filenames[i]<-paste(percent*100,"/data/data",k,"/sampletransfer251_145",percent*100,"percent_p1.csv",sep="")
    # i<-i+1
    # filenames[i]<-paste(percent*100,"/data/data",k,"/sampletransfer251_145",percent*100,"percent_p2.csv",sep="")
    # i<-i+1
    filenames[i]<-paste(percent*100,"/data/data",k,"/sampletransfer251_145",percent*100,"percent_p3.csv",sep="")
    i<-1
    
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

getfinaldataCaltech<-function(dd)
{
  dd<-data.frame(dd[1001],
                 rep("1:",nrow(dd)),dd[1],
                 rep("2:",nrow(dd)),dd[2],
                 rep("3:",nrow(dd)),dd[3],
                 rep("4:",nrow(dd)),dd[4],
                 rep("5:",nrow(dd)),dd[5],
                 rep("6:",nrow(dd)),dd[6],
                 rep("7:",nrow(dd)),dd[7],
                 rep("8:",nrow(dd)),dd[8],
                 rep("9:",nrow(dd)),dd[9],
                 rep("10:",nrow(dd)),dd[10],
                 rep("11:",nrow(dd)),dd[11],
                 rep("12:",nrow(dd)),dd[12],
                 rep("13:",nrow(dd)),dd[13],
                 rep("14:",nrow(dd)),dd[14],
                 rep("15:",nrow(dd)),dd[15],
                 rep("16:",nrow(dd)),dd[16],
                 rep("17:",nrow(dd)),dd[17],
                 rep("18:",nrow(dd)),dd[18],
                 rep("19:",nrow(dd)),dd[19],
                 rep("20:",nrow(dd)),dd[20],
                 rep("21:",nrow(dd)),dd[21],
                 rep("22:",nrow(dd)),dd[22],
                 rep("23:",nrow(dd)),dd[23],
                 rep("24:",nrow(dd)),dd[24],
                 rep("25:",nrow(dd)),dd[25],
                 rep("26:",nrow(dd)),dd[26],
                 rep("27:",nrow(dd)),dd[27],
                 rep("28:",nrow(dd)),dd[28],
                 rep("29:",nrow(dd)),dd[29],
                 rep("30:",nrow(dd)),dd[30],
                 rep("31:",nrow(dd)),dd[31],
                 rep("32:",nrow(dd)),dd[32],
                 rep("33:",nrow(dd)),dd[33],
                 rep("34:",nrow(dd)),dd[34],
                 rep("35:",nrow(dd)),dd[35],
                 rep("36:",nrow(dd)),dd[36],
                 rep("37:",nrow(dd)),dd[37],
                 rep("38:",nrow(dd)),dd[38],
                 rep("39:",nrow(dd)),dd[39],
                 rep("40:",nrow(dd)),dd[40],
                 rep("41:",nrow(dd)),dd[41],
                 rep("42:",nrow(dd)),dd[42],
                 rep("43:",nrow(dd)),dd[43],
                 rep("44:",nrow(dd)),dd[44],
                 rep("45:",nrow(dd)),dd[45],
                 rep("46:",nrow(dd)),dd[46],
                 rep("47:",nrow(dd)),dd[47],
                 rep("48:",nrow(dd)),dd[48],
                 rep("49:",nrow(dd)),dd[49],
                 rep("50:",nrow(dd)),dd[50],
                 rep("51:",nrow(dd)),dd[51],
                 rep("52:",nrow(dd)),dd[52],
                 rep("53:",nrow(dd)),dd[53],
                 rep("54:",nrow(dd)),dd[54],
                 rep("55:",nrow(dd)),dd[55],
                 rep("56:",nrow(dd)),dd[56],
                 rep("57:",nrow(dd)),dd[57],
                 rep("58:",nrow(dd)),dd[58],
                 rep("59:",nrow(dd)),dd[59],
                 rep("60:",nrow(dd)),dd[60],
                 rep("61:",nrow(dd)),dd[61],
                 rep("62:",nrow(dd)),dd[62],
                 rep("63:",nrow(dd)),dd[63],
                 rep("64:",nrow(dd)),dd[64],
                 rep("65:",nrow(dd)),dd[65],
                 rep("66:",nrow(dd)),dd[66],
                 rep("67:",nrow(dd)),dd[67],
                 rep("68:",nrow(dd)),dd[68],
                 rep("69:",nrow(dd)),dd[69],
                 rep("70:",nrow(dd)),dd[70],
                 rep("71:",nrow(dd)),dd[71],
                 rep("72:",nrow(dd)),dd[72],
                 rep("73:",nrow(dd)),dd[73],
                 rep("74:",nrow(dd)),dd[74],
                 rep("75:",nrow(dd)),dd[75],
                 rep("76:",nrow(dd)),dd[76],
                 rep("77:",nrow(dd)),dd[77],
                 rep("78:",nrow(dd)),dd[78],
                 rep("79:",nrow(dd)),dd[79],
                 rep("80:",nrow(dd)),dd[80],
                 rep("81:",nrow(dd)),dd[81],
                 rep("82:",nrow(dd)),dd[82],
                 rep("83:",nrow(dd)),dd[83],
                 rep("84:",nrow(dd)),dd[84],
                 rep("85:",nrow(dd)),dd[85],
                 rep("86:",nrow(dd)),dd[86],
                 rep("87:",nrow(dd)),dd[87],
                 rep("88:",nrow(dd)),dd[88],
                 rep("89:",nrow(dd)),dd[89],
                 rep("90:",nrow(dd)),dd[90],
                 rep("91:",nrow(dd)),dd[91],
                 rep("92:",nrow(dd)),dd[92],
                 rep("93:",nrow(dd)),dd[93],
                 rep("94:",nrow(dd)),dd[94],
                 rep("95:",nrow(dd)),dd[95],
                 rep("96:",nrow(dd)),dd[96],
                 rep("97:",nrow(dd)),dd[97],
                 rep("98:",nrow(dd)),dd[98],
                 rep("99:",nrow(dd)),dd[99],
                 rep("100:",nrow(dd)),dd[100],
                 rep("101:",nrow(dd)),dd[101],
                 rep("102:",nrow(dd)),dd[102],
                 rep("103:",nrow(dd)),dd[103],
                 rep("104:",nrow(dd)),dd[104],
                 rep("105:",nrow(dd)),dd[105],
                 rep("106:",nrow(dd)),dd[106],
                 rep("107:",nrow(dd)),dd[107],
                 rep("108:",nrow(dd)),dd[108],
                 rep("109:",nrow(dd)),dd[109],
                 rep("110:",nrow(dd)),dd[110],
                 rep("111:",nrow(dd)),dd[111],
                 rep("112:",nrow(dd)),dd[112],
                 rep("113:",nrow(dd)),dd[113],
                 rep("114:",nrow(dd)),dd[114],
                 rep("115:",nrow(dd)),dd[115],
                 rep("116:",nrow(dd)),dd[116],
                 rep("117:",nrow(dd)),dd[117],
                 rep("118:",nrow(dd)),dd[118],
                 rep("119:",nrow(dd)),dd[119],
                 rep("120:",nrow(dd)),dd[120],
                 rep("121:",nrow(dd)),dd[121],
                 rep("122:",nrow(dd)),dd[122],
                 rep("123:",nrow(dd)),dd[123],
                 rep("124:",nrow(dd)),dd[124],
                 rep("125:",nrow(dd)),dd[125],
                 rep("126:",nrow(dd)),dd[126],
                 rep("127:",nrow(dd)),dd[127],
                 rep("128:",nrow(dd)),dd[128],
                 rep("129:",nrow(dd)),dd[129],
                 rep("130:",nrow(dd)),dd[130],
                 rep("131:",nrow(dd)),dd[131],
                 rep("132:",nrow(dd)),dd[132],
                 rep("133:",nrow(dd)),dd[133],
                 rep("134:",nrow(dd)),dd[134],
                 rep("135:",nrow(dd)),dd[135],
                 rep("136:",nrow(dd)),dd[136],
                 rep("137:",nrow(dd)),dd[137],
                 rep("138:",nrow(dd)),dd[138],
                 rep("139:",nrow(dd)),dd[139],
                 rep("140:",nrow(dd)),dd[140],
                 rep("141:",nrow(dd)),dd[141],
                 rep("142:",nrow(dd)),dd[142],
                 rep("143:",nrow(dd)),dd[143],
                 rep("144:",nrow(dd)),dd[144],
                 rep("145:",nrow(dd)),dd[145],
                 rep("146:",nrow(dd)),dd[146],
                 rep("147:",nrow(dd)),dd[147],
                 rep("148:",nrow(dd)),dd[148],
                 rep("149:",nrow(dd)),dd[149],
                 rep("150:",nrow(dd)),dd[150],
                 rep("151:",nrow(dd)),dd[151],
                 rep("152:",nrow(dd)),dd[152],
                 rep("153:",nrow(dd)),dd[153],
                 rep("154:",nrow(dd)),dd[154],
                 rep("155:",nrow(dd)),dd[155],
                 rep("156:",nrow(dd)),dd[156],
                 rep("157:",nrow(dd)),dd[157],
                 rep("158:",nrow(dd)),dd[158],
                 rep("159:",nrow(dd)),dd[159],
                 rep("160:",nrow(dd)),dd[160],
                 rep("161:",nrow(dd)),dd[161],
                 rep("162:",nrow(dd)),dd[162],
                 rep("163:",nrow(dd)),dd[163],
                 rep("164:",nrow(dd)),dd[164],
                 rep("165:",nrow(dd)),dd[165],
                 rep("166:",nrow(dd)),dd[166],
                 rep("167:",nrow(dd)),dd[167],
                 rep("168:",nrow(dd)),dd[168],
                 rep("169:",nrow(dd)),dd[169],
                 rep("170:",nrow(dd)),dd[170],
                 rep("171:",nrow(dd)),dd[171],
                 rep("172:",nrow(dd)),dd[172],
                 rep("173:",nrow(dd)),dd[173],
                 rep("174:",nrow(dd)),dd[174],
                 rep("175:",nrow(dd)),dd[175],
                 rep("176:",nrow(dd)),dd[176],
                 rep("177:",nrow(dd)),dd[177],
                 rep("178:",nrow(dd)),dd[178],
                 rep("179:",nrow(dd)),dd[179],
                 rep("180:",nrow(dd)),dd[180],
                 rep("181:",nrow(dd)),dd[181],
                 rep("182:",nrow(dd)),dd[182],
                 rep("183:",nrow(dd)),dd[183],
                 rep("184:",nrow(dd)),dd[184],
                 rep("185:",nrow(dd)),dd[185],
                 rep("186:",nrow(dd)),dd[186],
                 rep("187:",nrow(dd)),dd[187],
                 rep("188:",nrow(dd)),dd[188],
                 rep("189:",nrow(dd)),dd[189],
                 rep("190:",nrow(dd)),dd[190],
                 rep("191:",nrow(dd)),dd[191],
                 rep("192:",nrow(dd)),dd[192],
                 rep("193:",nrow(dd)),dd[193],
                 rep("194:",nrow(dd)),dd[194],
                 rep("195:",nrow(dd)),dd[195],
                 rep("196:",nrow(dd)),dd[196],
                 rep("197:",nrow(dd)),dd[197],
                 rep("198:",nrow(dd)),dd[198],
                 rep("199:",nrow(dd)),dd[199],	
                 rep("200:",nrow(dd)),dd[200],
                 rep("201:",nrow(dd)),dd[201],
                 rep("202:",nrow(dd)),dd[202],
                 rep("203:",nrow(dd)),dd[203],
                 rep("204:",nrow(dd)),dd[204],
                 rep("205:",nrow(dd)),dd[205],
                 rep("206:",nrow(dd)),dd[206],
                 rep("207:",nrow(dd)),dd[207],
                 rep("208:",nrow(dd)),dd[208],
                 rep("209:",nrow(dd)),dd[209],
                 rep("210:",nrow(dd)),dd[210],
                 rep("211:",nrow(dd)),dd[211],
                 rep("212:",nrow(dd)),dd[212],
                 rep("213:",nrow(dd)),dd[213],
                 rep("214:",nrow(dd)),dd[214],
                 rep("215:",nrow(dd)),dd[215],
                 rep("216:",nrow(dd)),dd[216],
                 rep("217:",nrow(dd)),dd[217],
                 rep("218:",nrow(dd)),dd[218],
                 rep("219:",nrow(dd)),dd[219],
                 rep("220:",nrow(dd)),dd[220],
                 rep("221:",nrow(dd)),dd[221],
                 rep("222:",nrow(dd)),dd[222],
                 rep("223:",nrow(dd)),dd[223],
                 rep("224:",nrow(dd)),dd[224],
                 rep("225:",nrow(dd)),dd[225],
                 rep("226:",nrow(dd)),dd[226],
                 rep("227:",nrow(dd)),dd[227],
                 rep("228:",nrow(dd)),dd[228],
                 rep("229:",nrow(dd)),dd[229],
                 rep("230:",nrow(dd)),dd[230],
                 rep("231:",nrow(dd)),dd[231],
                 rep("232:",nrow(dd)),dd[232],
                 rep("233:",nrow(dd)),dd[233],
                 rep("234:",nrow(dd)),dd[234],
                 rep("235:",nrow(dd)),dd[235],
                 rep("236:",nrow(dd)),dd[236],
                 rep("237:",nrow(dd)),dd[237],
                 rep("238:",nrow(dd)),dd[238],
                 rep("239:",nrow(dd)),dd[239],
                 rep("240:",nrow(dd)),dd[240],
                 rep("241:",nrow(dd)),dd[241],
                 rep("242:",nrow(dd)),dd[242],
                 rep("243:",nrow(dd)),dd[243],
                 rep("244:",nrow(dd)),dd[244],
                 rep("245:",nrow(dd)),dd[245],
                 rep("246:",nrow(dd)),dd[246],
                 rep("247:",nrow(dd)),dd[247],
                 rep("248:",nrow(dd)),dd[248],
                 rep("249:",nrow(dd)),dd[249],
                 rep("250:",nrow(dd)),dd[250],
                 rep("251:",nrow(dd)),dd[251],
                 rep("252:",nrow(dd)),dd[252],
                 rep("253:",nrow(dd)),dd[253],
                 rep("254:",nrow(dd)),dd[254],
                 rep("255:",nrow(dd)),dd[255],
                 rep("256:",nrow(dd)),dd[256],
                 rep("257:",nrow(dd)),dd[257],
                 rep("258:",nrow(dd)),dd[258],
                 rep("259:",nrow(dd)),dd[259],
                 rep("260:",nrow(dd)),dd[260],
                 rep("261:",nrow(dd)),dd[261],
                 rep("262:",nrow(dd)),dd[262],
                 rep("263:",nrow(dd)),dd[263],
                 rep("264:",nrow(dd)),dd[264],
                 rep("265:",nrow(dd)),dd[265],
                 rep("266:",nrow(dd)),dd[266],
                 rep("267:",nrow(dd)),dd[267],
                 rep("268:",nrow(dd)),dd[268],
                 rep("269:",nrow(dd)),dd[269],
                 rep("270:",nrow(dd)),dd[270],
                 rep("271:",nrow(dd)),dd[271],
                 rep("272:",nrow(dd)),dd[272],
                 rep("273:",nrow(dd)),dd[273],
                 rep("274:",nrow(dd)),dd[274],
                 rep("275:",nrow(dd)),dd[275],
                 rep("276:",nrow(dd)),dd[276],
                 rep("277:",nrow(dd)),dd[277],
                 rep("278:",nrow(dd)),dd[278],
                 rep("279:",nrow(dd)),dd[279],
                 rep("280:",nrow(dd)),dd[280],
                 rep("281:",nrow(dd)),dd[281],
                 rep("282:",nrow(dd)),dd[282],
                 rep("283:",nrow(dd)),dd[283],
                 rep("284:",nrow(dd)),dd[284],
                 rep("285:",nrow(dd)),dd[285],
                 rep("286:",nrow(dd)),dd[286],
                 rep("287:",nrow(dd)),dd[287],
                 rep("288:",nrow(dd)),dd[288],
                 rep("289:",nrow(dd)),dd[289],
                 rep("290:",nrow(dd)),dd[290],
                 rep("291:",nrow(dd)),dd[291],
                 rep("292:",nrow(dd)),dd[292],
                 rep("293:",nrow(dd)),dd[293],
                 rep("294:",nrow(dd)),dd[294],
                 rep("295:",nrow(dd)),dd[295],
                 rep("296:",nrow(dd)),dd[296],
                 rep("297:",nrow(dd)),dd[297],
                 rep("298:",nrow(dd)),dd[298],
                 rep("299:",nrow(dd)),dd[299],
                 rep("300:",nrow(dd)),dd[300],
                 rep("301:",nrow(dd)),dd[301],
                 rep("302:",nrow(dd)),dd[302],
                 rep("303:",nrow(dd)),dd[303],
                 rep("304:",nrow(dd)),dd[304],
                 rep("305:",nrow(dd)),dd[305],
                 rep("306:",nrow(dd)),dd[306],
                 rep("307:",nrow(dd)),dd[307],
                 rep("308:",nrow(dd)),dd[308],
                 rep("309:",nrow(dd)),dd[309],
                 rep("310:",nrow(dd)),dd[310],
                 rep("311:",nrow(dd)),dd[311],
                 rep("312:",nrow(dd)),dd[312],
                 rep("313:",nrow(dd)),dd[313],
                 rep("314:",nrow(dd)),dd[314],
                 rep("315:",nrow(dd)),dd[315],
                 rep("316:",nrow(dd)),dd[316],
                 rep("317:",nrow(dd)),dd[317],
                 rep("318:",nrow(dd)),dd[318],
                 rep("319:",nrow(dd)),dd[319],
                 rep("320:",nrow(dd)),dd[320],
                 rep("321:",nrow(dd)),dd[321],
                 rep("322:",nrow(dd)),dd[322],
                 rep("323:",nrow(dd)),dd[323],
                 rep("324:",nrow(dd)),dd[324],
                 rep("325:",nrow(dd)),dd[325],
                 rep("326:",nrow(dd)),dd[326],
                 rep("327:",nrow(dd)),dd[327],
                 rep("328:",nrow(dd)),dd[328],
                 rep("329:",nrow(dd)),dd[329],
                 rep("330:",nrow(dd)),dd[330],
                 rep("331:",nrow(dd)),dd[331],
                 rep("332:",nrow(dd)),dd[332],
                 rep("333:",nrow(dd)),dd[333],
                 rep("334:",nrow(dd)),dd[334],
                 rep("335:",nrow(dd)),dd[335],
                 rep("336:",nrow(dd)),dd[336],
                 rep("337:",nrow(dd)),dd[337],
                 rep("338:",nrow(dd)),dd[338],
                 rep("339:",nrow(dd)),dd[339],
                 rep("340:",nrow(dd)),dd[340],
                 rep("341:",nrow(dd)),dd[341],
                 rep("342:",nrow(dd)),dd[342],
                 rep("343:",nrow(dd)),dd[343],
                 rep("344:",nrow(dd)),dd[344],
                 rep("345:",nrow(dd)),dd[345],
                 rep("346:",nrow(dd)),dd[346],
                 rep("347:",nrow(dd)),dd[347],
                 rep("348:",nrow(dd)),dd[348],
                 rep("349:",nrow(dd)),dd[349],
                 rep("350:",nrow(dd)),dd[350],
                 rep("351:",nrow(dd)),dd[351],
                 rep("352:",nrow(dd)),dd[352],
                 rep("353:",nrow(dd)),dd[353],
                 rep("354:",nrow(dd)),dd[354],
                 rep("355:",nrow(dd)),dd[355],
                 rep("356:",nrow(dd)),dd[356],
                 rep("357:",nrow(dd)),dd[357],
                 rep("358:",nrow(dd)),dd[358],
                 rep("359:",nrow(dd)),dd[359],
                 rep("360:",nrow(dd)),dd[360],
                 rep("361:",nrow(dd)),dd[361],
                 rep("362:",nrow(dd)),dd[362],
                 rep("363:",nrow(dd)),dd[363],
                 rep("364:",nrow(dd)),dd[364],
                 rep("365:",nrow(dd)),dd[365],
                 rep("366:",nrow(dd)),dd[366],
                 rep("367:",nrow(dd)),dd[367],
                 rep("368:",nrow(dd)),dd[368],
                 rep("369:",nrow(dd)),dd[369],
                 rep("370:",nrow(dd)),dd[370],
                 rep("371:",nrow(dd)),dd[371],
                 rep("372:",nrow(dd)),dd[372],
                 rep("373:",nrow(dd)),dd[373],
                 rep("374:",nrow(dd)),dd[374],
                 rep("375:",nrow(dd)),dd[375],
                 rep("376:",nrow(dd)),dd[376],
                 rep("377:",nrow(dd)),dd[377],
                 rep("378:",nrow(dd)),dd[378],
                 rep("379:",nrow(dd)),dd[379],
                 rep("380:",nrow(dd)),dd[380],
                 rep("381:",nrow(dd)),dd[381],
                 rep("382:",nrow(dd)),dd[382],
                 rep("383:",nrow(dd)),dd[383],
                 rep("384:",nrow(dd)),dd[384],
                 rep("385:",nrow(dd)),dd[385],
                 rep("386:",nrow(dd)),dd[386],
                 rep("387:",nrow(dd)),dd[387],
                 rep("388:",nrow(dd)),dd[388],
                 rep("389:",nrow(dd)),dd[389],
                 rep("390:",nrow(dd)),dd[390],
                 rep("391:",nrow(dd)),dd[391],
                 rep("392:",nrow(dd)),dd[392],
                 rep("393:",nrow(dd)),dd[393],
                 rep("394:",nrow(dd)),dd[394],
                 rep("395:",nrow(dd)),dd[395],
                 rep("396:",nrow(dd)),dd[396],
                 rep("397:",nrow(dd)),dd[397],
                 rep("398:",nrow(dd)),dd[398],
                 rep("399:",nrow(dd)),dd[399],
                 rep("400:",nrow(dd)),dd[400],
                 rep("401:",nrow(dd)),dd[401],
                 rep("402:",nrow(dd)),dd[402],
                 rep("403:",nrow(dd)),dd[403],
                 rep("404:",nrow(dd)),dd[404],
                 rep("405:",nrow(dd)),dd[405],
                 rep("406:",nrow(dd)),dd[406],
                 rep("407:",nrow(dd)),dd[407],
                 rep("408:",nrow(dd)),dd[408],
                 rep("409:",nrow(dd)),dd[409],
                 rep("410:",nrow(dd)),dd[410],
                 rep("411:",nrow(dd)),dd[411],
                 rep("412:",nrow(dd)),dd[412],
                 rep("413:",nrow(dd)),dd[413],
                 rep("414:",nrow(dd)),dd[414],
                 rep("415:",nrow(dd)),dd[415],
                 rep("416:",nrow(dd)),dd[416],
                 rep("417:",nrow(dd)),dd[417],
                 rep("418:",nrow(dd)),dd[418],
                 rep("419:",nrow(dd)),dd[419],
                 rep("420:",nrow(dd)),dd[420],
                 rep("421:",nrow(dd)),dd[421],
                 rep("422:",nrow(dd)),dd[422],
                 rep("423:",nrow(dd)),dd[423],
                 rep("424:",nrow(dd)),dd[424],
                 rep("425:",nrow(dd)),dd[425],
                 rep("426:",nrow(dd)),dd[426],
                 rep("427:",nrow(dd)),dd[427],
                 rep("428:",nrow(dd)),dd[428],
                 rep("429:",nrow(dd)),dd[429],
                 rep("430:",nrow(dd)),dd[430],
                 rep("431:",nrow(dd)),dd[431],
                 rep("432:",nrow(dd)),dd[432],
                 rep("433:",nrow(dd)),dd[433],
                 rep("434:",nrow(dd)),dd[434],
                 rep("435:",nrow(dd)),dd[435],
                 rep("436:",nrow(dd)),dd[436],
                 rep("437:",nrow(dd)),dd[437],
                 rep("438:",nrow(dd)),dd[438],
                 rep("439:",nrow(dd)),dd[439],
                 rep("440:",nrow(dd)),dd[440],
                 rep("441:",nrow(dd)),dd[441],
                 rep("442:",nrow(dd)),dd[442],
                 rep("443:",nrow(dd)),dd[443],
                 rep("444:",nrow(dd)),dd[444],
                 rep("445:",nrow(dd)),dd[445],
                 rep("446:",nrow(dd)),dd[446],
                 rep("447:",nrow(dd)),dd[447],
                 rep("448:",nrow(dd)),dd[448],
                 rep("449:",nrow(dd)),dd[449],
                 rep("450:",nrow(dd)),dd[450],
                 rep("451:",nrow(dd)),dd[451],
                 rep("452:",nrow(dd)),dd[452],
                 rep("453:",nrow(dd)),dd[453],
                 rep("454:",nrow(dd)),dd[454],
                 rep("455:",nrow(dd)),dd[455],
                 rep("456:",nrow(dd)),dd[456],
                 rep("457:",nrow(dd)),dd[457],
                 rep("458:",nrow(dd)),dd[458],
                 rep("459:",nrow(dd)),dd[459],
                 rep("460:",nrow(dd)),dd[460],
                 rep("461:",nrow(dd)),dd[461],
                 rep("462:",nrow(dd)),dd[462],
                 rep("463:",nrow(dd)),dd[463],
                 rep("464:",nrow(dd)),dd[464],
                 rep("465:",nrow(dd)),dd[465],
                 rep("466:",nrow(dd)),dd[466],
                 rep("467:",nrow(dd)),dd[467],
                 rep("468:",nrow(dd)),dd[468],
                 rep("469:",nrow(dd)),dd[469],
                 rep("470:",nrow(dd)),dd[470],
                 rep("471:",nrow(dd)),dd[471],
                 rep("472:",nrow(dd)),dd[472],
                 rep("473:",nrow(dd)),dd[473],
                 rep("474:",nrow(dd)),dd[474],
                 rep("475:",nrow(dd)),dd[475],
                 rep("476:",nrow(dd)),dd[476],
                 rep("477:",nrow(dd)),dd[477],
                 rep("478:",nrow(dd)),dd[478],
                 rep("479:",nrow(dd)),dd[479],
                 rep("480:",nrow(dd)),dd[480],
                 rep("481:",nrow(dd)),dd[481],
                 rep("482:",nrow(dd)),dd[482],
                 rep("483:",nrow(dd)),dd[483],
                 rep("484:",nrow(dd)),dd[484],
                 rep("485:",nrow(dd)),dd[485],
                 rep("486:",nrow(dd)),dd[486],
                 rep("487:",nrow(dd)),dd[487],
                 rep("488:",nrow(dd)),dd[488],
                 rep("489:",nrow(dd)),dd[489],
                 rep("490:",nrow(dd)),dd[490],
                 rep("491:",nrow(dd)),dd[491],
                 rep("492:",nrow(dd)),dd[492],
                 rep("493:",nrow(dd)),dd[493],
                 rep("494:",nrow(dd)),dd[494],
                 rep("495:",nrow(dd)),dd[495],
                 rep("496:",nrow(dd)),dd[496],
                 rep("497:",nrow(dd)),dd[497],
                 rep("498:",nrow(dd)),dd[498],
                 rep("499:",nrow(dd)),dd[499],
                 rep("500:",nrow(dd)),dd[500],
                 rep("501:",nrow(dd)),dd[501],
                 rep("502:",nrow(dd)),dd[502],
                 rep("503:",nrow(dd)),dd[503],
                 rep("504:",nrow(dd)),dd[504],
                 rep("505:",nrow(dd)),dd[505],
                 rep("506:",nrow(dd)),dd[506],
                 rep("507:",nrow(dd)),dd[507],
                 rep("508:",nrow(dd)),dd[508],
                 rep("509:",nrow(dd)),dd[509],
                 rep("510:",nrow(dd)),dd[510],
                 rep("511:",nrow(dd)),dd[511],
                 rep("512:",nrow(dd)),dd[512],
                 rep("513:",nrow(dd)),dd[513],
                 rep("514:",nrow(dd)),dd[514],
                 rep("515:",nrow(dd)),dd[515],
                 rep("516:",nrow(dd)),dd[516],
                 rep("517:",nrow(dd)),dd[517],
                 rep("518:",nrow(dd)),dd[518],
                 rep("519:",nrow(dd)),dd[519],
                 rep("520:",nrow(dd)),dd[520],
                 rep("521:",nrow(dd)),dd[521],
                 rep("522:",nrow(dd)),dd[522],
                 rep("523:",nrow(dd)),dd[523],
                 rep("524:",nrow(dd)),dd[524],
                 rep("525:",nrow(dd)),dd[525],
                 rep("526:",nrow(dd)),dd[526],
                 rep("527:",nrow(dd)),dd[527],
                 rep("528:",nrow(dd)),dd[528],
                 rep("529:",nrow(dd)),dd[529],
                 rep("530:",nrow(dd)),dd[530],
                 rep("531:",nrow(dd)),dd[531],
                 rep("532:",nrow(dd)),dd[532],
                 rep("533:",nrow(dd)),dd[533],
                 rep("534:",nrow(dd)),dd[534],
                 rep("535:",nrow(dd)),dd[535],
                 rep("536:",nrow(dd)),dd[536],
                 rep("537:",nrow(dd)),dd[537],
                 rep("538:",nrow(dd)),dd[538],
                 rep("539:",nrow(dd)),dd[539],
                 rep("540:",nrow(dd)),dd[540],
                 rep("541:",nrow(dd)),dd[541],
                 rep("542:",nrow(dd)),dd[542],
                 rep("543:",nrow(dd)),dd[543],
                 rep("544:",nrow(dd)),dd[544],
                 rep("545:",nrow(dd)),dd[545],
                 rep("546:",nrow(dd)),dd[546],
                 rep("547:",nrow(dd)),dd[547],
                 rep("548:",nrow(dd)),dd[548],
                 rep("549:",nrow(dd)),dd[549],
                 rep("550:",nrow(dd)),dd[550],
                 rep("551:",nrow(dd)),dd[551],
                 rep("552:",nrow(dd)),dd[552],
                 rep("553:",nrow(dd)),dd[553],
                 rep("554:",nrow(dd)),dd[554],
                 rep("555:",nrow(dd)),dd[555],
                 rep("556:",nrow(dd)),dd[556],
                 rep("557:",nrow(dd)),dd[557],
                 rep("558:",nrow(dd)),dd[558],
                 rep("559:",nrow(dd)),dd[559],
                 rep("560:",nrow(dd)),dd[560],
                 rep("561:",nrow(dd)),dd[561],
                 rep("562:",nrow(dd)),dd[562],
                 rep("563:",nrow(dd)),dd[563],
                 rep("564:",nrow(dd)),dd[564],
                 rep("565:",nrow(dd)),dd[565],
                 rep("566:",nrow(dd)),dd[566],
                 rep("567:",nrow(dd)),dd[567],
                 rep("568:",nrow(dd)),dd[568],
                 rep("569:",nrow(dd)),dd[569],
                 rep("570:",nrow(dd)),dd[570],
                 rep("571:",nrow(dd)),dd[571],
                 rep("572:",nrow(dd)),dd[572],
                 rep("573:",nrow(dd)),dd[573],
                 rep("574:",nrow(dd)),dd[574],
                 rep("575:",nrow(dd)),dd[575],
                 rep("576:",nrow(dd)),dd[576],
                 rep("577:",nrow(dd)),dd[577],
                 rep("578:",nrow(dd)),dd[578],
                 rep("579:",nrow(dd)),dd[579],
                 rep("580:",nrow(dd)),dd[580],
                 rep("581:",nrow(dd)),dd[581],
                 rep("582:",nrow(dd)),dd[582],
                 rep("583:",nrow(dd)),dd[583],
                 rep("584:",nrow(dd)),dd[584],
                 rep("585:",nrow(dd)),dd[585],
                 rep("586:",nrow(dd)),dd[586],
                 rep("587:",nrow(dd)),dd[587],
                 rep("588:",nrow(dd)),dd[588],
                 rep("589:",nrow(dd)),dd[589],
                 rep("590:",nrow(dd)),dd[590],
                 rep("591:",nrow(dd)),dd[591],
                 rep("592:",nrow(dd)),dd[592],
                 rep("593:",nrow(dd)),dd[593],
                 rep("594:",nrow(dd)),dd[594],
                 rep("595:",nrow(dd)),dd[595],
                 rep("596:",nrow(dd)),dd[596],
                 rep("597:",nrow(dd)),dd[597],
                 rep("598:",nrow(dd)),dd[598],
                 rep("599:",nrow(dd)),dd[599],
                 rep("600:",nrow(dd)),dd[600],
                 rep("601:",nrow(dd)),dd[601],
                 rep("602:",nrow(dd)),dd[602],
                 rep("603:",nrow(dd)),dd[603],
                 rep("604:",nrow(dd)),dd[604],
                 rep("605:",nrow(dd)),dd[605],
                 rep("606:",nrow(dd)),dd[606],
                 rep("607:",nrow(dd)),dd[607],
                 rep("608:",nrow(dd)),dd[608],
                 rep("609:",nrow(dd)),dd[609],
                 rep("610:",nrow(dd)),dd[610],
                 rep("611:",nrow(dd)),dd[611],
                 rep("612:",nrow(dd)),dd[612],
                 rep("613:",nrow(dd)),dd[613],
                 rep("614:",nrow(dd)),dd[614],
                 rep("615:",nrow(dd)),dd[615],
                 rep("616:",nrow(dd)),dd[616],
                 rep("617:",nrow(dd)),dd[617],
                 rep("618:",nrow(dd)),dd[618],
                 rep("619:",nrow(dd)),dd[619],
                 rep("620:",nrow(dd)),dd[620],
                 rep("621:",nrow(dd)),dd[621],
                 rep("622:",nrow(dd)),dd[622],
                 rep("623:",nrow(dd)),dd[623],
                 rep("624:",nrow(dd)),dd[624],
                 rep("625:",nrow(dd)),dd[625],
                 rep("626:",nrow(dd)),dd[626],
                 rep("627:",nrow(dd)),dd[627],
                 rep("628:",nrow(dd)),dd[628],
                 rep("629:",nrow(dd)),dd[629],
                 rep("630:",nrow(dd)),dd[630],
                 rep("631:",nrow(dd)),dd[631],
                 rep("632:",nrow(dd)),dd[632],
                 rep("633:",nrow(dd)),dd[633],
                 rep("634:",nrow(dd)),dd[634],
                 rep("635:",nrow(dd)),dd[635],
                 rep("636:",nrow(dd)),dd[636],
                 rep("637:",nrow(dd)),dd[637],
                 rep("638:",nrow(dd)),dd[638],
                 rep("639:",nrow(dd)),dd[639],
                 rep("640:",nrow(dd)),dd[640],
                 rep("641:",nrow(dd)),dd[641],
                 rep("642:",nrow(dd)),dd[642],
                 rep("643:",nrow(dd)),dd[643],
                 rep("644:",nrow(dd)),dd[644],
                 rep("645:",nrow(dd)),dd[645],
                 rep("646:",nrow(dd)),dd[646],
                 rep("647:",nrow(dd)),dd[647],
                 rep("648:",nrow(dd)),dd[648],
                 rep("649:",nrow(dd)),dd[649],
                 rep("650:",nrow(dd)),dd[650],
                 rep("651:",nrow(dd)),dd[651],
                 rep("652:",nrow(dd)),dd[652],
                 rep("653:",nrow(dd)),dd[653],
                 rep("654:",nrow(dd)),dd[654],
                 rep("655:",nrow(dd)),dd[655],
                 rep("656:",nrow(dd)),dd[656],
                 rep("657:",nrow(dd)),dd[657],
                 rep("658:",nrow(dd)),dd[658],
                 rep("659:",nrow(dd)),dd[659],
                 rep("660:",nrow(dd)),dd[660],
                 rep("661:",nrow(dd)),dd[661],
                 rep("662:",nrow(dd)),dd[662],
                 rep("663:",nrow(dd)),dd[663],
                 rep("664:",nrow(dd)),dd[664],
                 rep("665:",nrow(dd)),dd[665],
                 rep("666:",nrow(dd)),dd[666],
                 rep("667:",nrow(dd)),dd[667],
                 rep("668:",nrow(dd)),dd[668],
                 rep("669:",nrow(dd)),dd[669],
                 rep("670:",nrow(dd)),dd[670],
                 rep("671:",nrow(dd)),dd[671],
                 rep("672:",nrow(dd)),dd[672],
                 rep("673:",nrow(dd)),dd[673],
                 rep("674:",nrow(dd)),dd[674],
                 rep("675:",nrow(dd)),dd[675],
                 rep("676:",nrow(dd)),dd[676],
                 rep("677:",nrow(dd)),dd[677],
                 rep("678:",nrow(dd)),dd[678],
                 rep("679:",nrow(dd)),dd[679],
                 rep("680:",nrow(dd)),dd[680],
                 rep("681:",nrow(dd)),dd[681],
                 rep("682:",nrow(dd)),dd[682],
                 rep("683:",nrow(dd)),dd[683],
                 rep("684:",nrow(dd)),dd[684],
                 rep("685:",nrow(dd)),dd[685],
                 rep("686:",nrow(dd)),dd[686],
                 rep("687:",nrow(dd)),dd[687],
                 rep("688:",nrow(dd)),dd[688],
                 rep("689:",nrow(dd)),dd[689],
                 rep("690:",nrow(dd)),dd[690],
                 rep("691:",nrow(dd)),dd[691],
                 rep("692:",nrow(dd)),dd[692],
                 rep("693:",nrow(dd)),dd[693],
                 rep("694:",nrow(dd)),dd[694],
                 rep("695:",nrow(dd)),dd[695],
                 rep("696:",nrow(dd)),dd[696],
                 rep("697:",nrow(dd)),dd[697],
                 rep("698:",nrow(dd)),dd[698],
                 rep("699:",nrow(dd)),dd[699],
                 rep("700:",nrow(dd)),dd[700],
                 rep("701:",nrow(dd)),dd[701],
                 rep("702:",nrow(dd)),dd[702],
                 rep("703:",nrow(dd)),dd[703],
                 rep("704:",nrow(dd)),dd[704],
                 rep("705:",nrow(dd)),dd[705],
                 rep("706:",nrow(dd)),dd[706],
                 rep("707:",nrow(dd)),dd[707],
                 rep("708:",nrow(dd)),dd[708],
                 rep("709:",nrow(dd)),dd[709],
                 rep("710:",nrow(dd)),dd[710],
                 rep("711:",nrow(dd)),dd[711],
                 rep("712:",nrow(dd)),dd[712],
                 rep("713:",nrow(dd)),dd[713],
                 rep("714:",nrow(dd)),dd[714],
                 rep("715:",nrow(dd)),dd[715],
                 rep("716:",nrow(dd)),dd[716],
                 rep("717:",nrow(dd)),dd[717],
                 rep("718:",nrow(dd)),dd[718],
                 rep("719:",nrow(dd)),dd[719],
                 rep("720:",nrow(dd)),dd[720],
                 rep("721:",nrow(dd)),dd[721],
                 rep("722:",nrow(dd)),dd[722],
                 rep("723:",nrow(dd)),dd[723],
                 rep("724:",nrow(dd)),dd[724],
                 rep("725:",nrow(dd)),dd[725],
                 rep("726:",nrow(dd)),dd[726],
                 rep("727:",nrow(dd)),dd[727],
                 rep("728:",nrow(dd)),dd[728],
                 rep("729:",nrow(dd)),dd[729],
                 rep("730:",nrow(dd)),dd[730],
                 rep("731:",nrow(dd)),dd[731],
                 rep("732:",nrow(dd)),dd[732],
                 rep("733:",nrow(dd)),dd[733],
                 rep("734:",nrow(dd)),dd[734],
                 rep("735:",nrow(dd)),dd[735],
                 rep("736:",nrow(dd)),dd[736],
                 rep("737:",nrow(dd)),dd[737],
                 rep("738:",nrow(dd)),dd[738],
                 rep("739:",nrow(dd)),dd[739],
                 rep("740:",nrow(dd)),dd[740],
                 rep("741:",nrow(dd)),dd[741],
                 rep("742:",nrow(dd)),dd[742],
                 rep("743:",nrow(dd)),dd[743],
                 rep("744:",nrow(dd)),dd[744],
                 rep("745:",nrow(dd)),dd[745],
                 rep("746:",nrow(dd)),dd[746],
                 rep("747:",nrow(dd)),dd[747],
                 rep("748:",nrow(dd)),dd[748],
                 rep("749:",nrow(dd)),dd[749],
                 rep("750:",nrow(dd)),dd[750],
                 rep("751:",nrow(dd)),dd[751],
                 rep("752:",nrow(dd)),dd[752],
                 rep("753:",nrow(dd)),dd[753],
                 rep("754:",nrow(dd)),dd[754],
                 rep("755:",nrow(dd)),dd[755],
                 rep("756:",nrow(dd)),dd[756],
                 rep("757:",nrow(dd)),dd[757],
                 rep("758:",nrow(dd)),dd[758],
                 rep("759:",nrow(dd)),dd[759],
                 rep("760:",nrow(dd)),dd[760],
                 rep("761:",nrow(dd)),dd[761],
                 rep("762:",nrow(dd)),dd[762],
                 rep("763:",nrow(dd)),dd[763],
                 rep("764:",nrow(dd)),dd[764],
                 rep("765:",nrow(dd)),dd[765],
                 rep("766:",nrow(dd)),dd[766],
                 rep("767:",nrow(dd)),dd[767],
                 rep("768:",nrow(dd)),dd[768],
                 rep("769:",nrow(dd)),dd[769],
                 rep("770:",nrow(dd)),dd[770],
                 rep("771:",nrow(dd)),dd[771],
                 rep("772:",nrow(dd)),dd[772],
                 rep("773:",nrow(dd)),dd[773],
                 rep("774:",nrow(dd)),dd[774],
                 rep("775:",nrow(dd)),dd[775],
                 rep("776:",nrow(dd)),dd[776],
                 rep("777:",nrow(dd)),dd[777],
                 rep("778:",nrow(dd)),dd[778],
                 rep("779:",nrow(dd)),dd[779],
                 rep("780:",nrow(dd)),dd[780],
                 rep("781:",nrow(dd)),dd[781],
                 rep("782:",nrow(dd)),dd[782],
                 rep("783:",nrow(dd)),dd[783],
                 rep("784:",nrow(dd)),dd[784],
                 rep("785:",nrow(dd)),dd[785],
                 rep("786:",nrow(dd)),dd[786],
                 rep("787:",nrow(dd)),dd[787],
                 rep("788:",nrow(dd)),dd[788],
                 rep("789:",nrow(dd)),dd[789],
                 rep("790:",nrow(dd)),dd[790],
                 rep("791:",nrow(dd)),dd[791],
                 rep("792:",nrow(dd)),dd[792],
                 rep("793:",nrow(dd)),dd[793],
                 rep("794:",nrow(dd)),dd[794],
                 rep("795:",nrow(dd)),dd[795],
                 rep("796:",nrow(dd)),dd[796],
                 rep("797:",nrow(dd)),dd[797],
                 rep("798:",nrow(dd)),dd[798],
                 rep("799:",nrow(dd)),dd[799],
                 rep("800:",nrow(dd)),dd[800],
                 rep("801:",nrow(dd)),dd[801],
                 rep("802:",nrow(dd)),dd[802],
                 rep("803:",nrow(dd)),dd[803],
                 rep("804:",nrow(dd)),dd[804],
                 rep("805:",nrow(dd)),dd[805],
                 rep("806:",nrow(dd)),dd[806],
                 rep("807:",nrow(dd)),dd[807],
                 rep("808:",nrow(dd)),dd[808],
                 rep("809:",nrow(dd)),dd[809],
                 rep("810:",nrow(dd)),dd[810],
                 rep("811:",nrow(dd)),dd[811],
                 rep("812:",nrow(dd)),dd[812],
                 rep("813:",nrow(dd)),dd[813],
                 rep("814:",nrow(dd)),dd[814],
                 rep("815:",nrow(dd)),dd[815],
                 rep("816:",nrow(dd)),dd[816],
                 rep("817:",nrow(dd)),dd[817],
                 rep("818:",nrow(dd)),dd[818],
                 rep("819:",nrow(dd)),dd[819],
                 rep("820:",nrow(dd)),dd[820],
                 rep("821:",nrow(dd)),dd[821],
                 rep("822:",nrow(dd)),dd[822],
                 rep("823:",nrow(dd)),dd[823],
                 rep("824:",nrow(dd)),dd[824],
                 rep("825:",nrow(dd)),dd[825],
                 rep("826:",nrow(dd)),dd[826],
                 rep("827:",nrow(dd)),dd[827],
                 rep("828:",nrow(dd)),dd[828],
                 rep("829:",nrow(dd)),dd[829],
                 rep("830:",nrow(dd)),dd[830],
                 rep("831:",nrow(dd)),dd[831],
                 rep("832:",nrow(dd)),dd[832],
                 rep("833:",nrow(dd)),dd[833],
                 rep("834:",nrow(dd)),dd[834],
                 rep("835:",nrow(dd)),dd[835],
                 rep("836:",nrow(dd)),dd[836],
                 rep("837:",nrow(dd)),dd[837],
                 rep("838:",nrow(dd)),dd[838],
                 rep("839:",nrow(dd)),dd[839],
                 rep("840:",nrow(dd)),dd[840],
                 rep("841:",nrow(dd)),dd[841],
                 rep("842:",nrow(dd)),dd[842],
                 rep("843:",nrow(dd)),dd[843],
                 rep("844:",nrow(dd)),dd[844],
                 rep("845:",nrow(dd)),dd[845],
                 rep("846:",nrow(dd)),dd[846],
                 rep("847:",nrow(dd)),dd[847],
                 rep("848:",nrow(dd)),dd[848],
                 rep("849:",nrow(dd)),dd[849],
                 rep("850:",nrow(dd)),dd[850],
                 rep("851:",nrow(dd)),dd[851],
                 rep("852:",nrow(dd)),dd[852],
                 rep("853:",nrow(dd)),dd[853],
                 rep("854:",nrow(dd)),dd[854],
                 rep("855:",nrow(dd)),dd[855],
                 rep("856:",nrow(dd)),dd[856],
                 rep("857:",nrow(dd)),dd[857],
                 rep("858:",nrow(dd)),dd[858],
                 rep("859:",nrow(dd)),dd[859],
                 rep("860:",nrow(dd)),dd[860],
                 rep("861:",nrow(dd)),dd[861],
                 rep("862:",nrow(dd)),dd[862],
                 rep("863:",nrow(dd)),dd[863],
                 rep("864:",nrow(dd)),dd[864],
                 rep("865:",nrow(dd)),dd[865],
                 rep("866:",nrow(dd)),dd[866],
                 rep("867:",nrow(dd)),dd[867],
                 rep("868:",nrow(dd)),dd[868],
                 rep("869:",nrow(dd)),dd[869],
                 rep("870:",nrow(dd)),dd[870],
                 rep("871:",nrow(dd)),dd[871],
                 rep("872:",nrow(dd)),dd[872],
                 rep("873:",nrow(dd)),dd[873],
                 rep("874:",nrow(dd)),dd[874],
                 rep("875:",nrow(dd)),dd[875],
                 rep("876:",nrow(dd)),dd[876],
                 rep("877:",nrow(dd)),dd[877],
                 rep("878:",nrow(dd)),dd[878],
                 rep("879:",nrow(dd)),dd[879],
                 rep("880:",nrow(dd)),dd[880],
                 rep("881:",nrow(dd)),dd[881],
                 rep("882:",nrow(dd)),dd[882],
                 rep("883:",nrow(dd)),dd[883],
                 rep("884:",nrow(dd)),dd[884],
                 rep("885:",nrow(dd)),dd[885],
                 rep("886:",nrow(dd)),dd[886],
                 rep("887:",nrow(dd)),dd[887],
                 rep("888:",nrow(dd)),dd[888],
                 rep("889:",nrow(dd)),dd[889],
                 rep("890:",nrow(dd)),dd[890],
                 rep("891:",nrow(dd)),dd[891],
                 rep("892:",nrow(dd)),dd[892],
                 rep("893:",nrow(dd)),dd[893],
                 rep("894:",nrow(dd)),dd[894],
                 rep("895:",nrow(dd)),dd[895],
                 rep("896:",nrow(dd)),dd[896],
                 rep("897:",nrow(dd)),dd[897],
                 rep("898:",nrow(dd)),dd[898],
                 rep("899:",nrow(dd)),dd[899],
                 rep("900:",nrow(dd)),dd[900],
                 rep("901:",nrow(dd)),dd[901],
                 rep("902:",nrow(dd)),dd[902],
                 rep("903:",nrow(dd)),dd[903],
                 rep("904:",nrow(dd)),dd[904],
                 rep("905:",nrow(dd)),dd[905],
                 rep("906:",nrow(dd)),dd[906],
                 rep("907:",nrow(dd)),dd[907],
                 rep("908:",nrow(dd)),dd[908],
                 rep("909:",nrow(dd)),dd[909],
                 rep("910:",nrow(dd)),dd[910],
                 rep("911:",nrow(dd)),dd[911],
                 rep("912:",nrow(dd)),dd[912],
                 rep("913:",nrow(dd)),dd[913],
                 rep("914:",nrow(dd)),dd[914],
                 rep("915:",nrow(dd)),dd[915],
                 rep("916:",nrow(dd)),dd[916],
                 rep("917:",nrow(dd)),dd[917],
                 rep("918:",nrow(dd)),dd[918],
                 rep("919:",nrow(dd)),dd[919],
                 rep("920:",nrow(dd)),dd[920],
                 rep("921:",nrow(dd)),dd[921],
                 rep("922:",nrow(dd)),dd[922],
                 rep("923:",nrow(dd)),dd[923],
                 rep("924:",nrow(dd)),dd[924],
                 rep("925:",nrow(dd)),dd[925],
                 rep("926:",nrow(dd)),dd[926],
                 rep("927:",nrow(dd)),dd[927],
                 rep("928:",nrow(dd)),dd[928],
                 rep("929:",nrow(dd)),dd[929],
                 rep("930:",nrow(dd)),dd[930],
                 rep("931:",nrow(dd)),dd[931],
                 rep("932:",nrow(dd)),dd[932],
                 rep("933:",nrow(dd)),dd[933],
                 rep("934:",nrow(dd)),dd[934],
                 rep("935:",nrow(dd)),dd[935],
                 rep("936:",nrow(dd)),dd[936],
                 rep("937:",nrow(dd)),dd[937],
                 rep("938:",nrow(dd)),dd[938],
                 rep("939:",nrow(dd)),dd[939],
                 rep("940:",nrow(dd)),dd[940],
                 rep("941:",nrow(dd)),dd[941],
                 rep("942:",nrow(dd)),dd[942],
                 rep("943:",nrow(dd)),dd[943],
                 rep("944:",nrow(dd)),dd[944],
                 rep("945:",nrow(dd)),dd[945],
                 rep("946:",nrow(dd)),dd[946],
                 rep("947:",nrow(dd)),dd[947],
                 rep("948:",nrow(dd)),dd[948],
                 rep("949:",nrow(dd)),dd[949],
                 rep("950:",nrow(dd)),dd[950],
                 rep("951:",nrow(dd)),dd[951],
                 rep("952:",nrow(dd)),dd[952],
                 rep("953:",nrow(dd)),dd[953],
                 rep("954:",nrow(dd)),dd[954],
                 rep("955:",nrow(dd)),dd[955],
                 rep("956:",nrow(dd)),dd[956],
                 rep("957:",nrow(dd)),dd[957],
                 rep("958:",nrow(dd)),dd[958],
                 rep("959:",nrow(dd)),dd[959],
                 rep("960:",nrow(dd)),dd[960],
                 rep("961:",nrow(dd)),dd[961],
                 rep("962:",nrow(dd)),dd[962],
                 rep("963:",nrow(dd)),dd[963],
                 rep("964:",nrow(dd)),dd[964],
                 rep("965:",nrow(dd)),dd[965],
                 rep("966:",nrow(dd)),dd[966],
                 rep("967:",nrow(dd)),dd[967],
                 rep("968:",nrow(dd)),dd[968],
                 rep("969:",nrow(dd)),dd[969],
                 rep("970:",nrow(dd)),dd[970],
                 rep("971:",nrow(dd)),dd[971],
                 rep("972:",nrow(dd)),dd[972],
                 rep("973:",nrow(dd)),dd[973],
                 rep("974:",nrow(dd)),dd[974],
                 rep("975:",nrow(dd)),dd[975],
                 rep("976:",nrow(dd)),dd[976],
                 rep("977:",nrow(dd)),dd[977],
                 rep("978:",nrow(dd)),dd[978],
                 rep("979:",nrow(dd)),dd[979],
                 rep("980:",nrow(dd)),dd[980],
                 rep("981:",nrow(dd)),dd[981],
                 rep("982:",nrow(dd)),dd[982],
                 rep("983:",nrow(dd)),dd[983],
                 rep("984:",nrow(dd)),dd[984],
                 rep("985:",nrow(dd)),dd[985],
                 rep("986:",nrow(dd)),dd[986],
                 rep("987:",nrow(dd)),dd[987],
                 rep("988:",nrow(dd)),dd[988],
                 rep("989:",nrow(dd)),dd[989],
                 rep("990:",nrow(dd)),dd[990],
                 rep("991:",nrow(dd)),dd[991],
                 rep("992:",nrow(dd)),dd[992],
                 rep("993:",nrow(dd)),dd[993],
                 rep("994:",nrow(dd)),dd[994],
                 rep("995:",nrow(dd)),dd[995],
                 rep("996:",nrow(dd)),dd[996],
                 rep("997:",nrow(dd)),dd[997],
                 rep("998:",nrow(dd)),dd[998],
                 rep("999:",nrow(dd)),dd[999],
                 rep("1000:",nrow(dd)),dd[1000])
                 
  dd
}