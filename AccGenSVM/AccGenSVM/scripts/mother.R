mother<-function(class)
{
  #setwd("/home/dben652/HTL/scripts")
  setwd("C:/Users/dben652/workspace/AccGenSVM/HTL/scripts")
  #setwd("/gpfs1m/projects/uoa00440/AccGenSVM/scripts")

  library("xlsx")
  library("RCurl")
  library("plyr")
  library("ggplot2")
  library("sampling")
  library("caret")
  library("FNN")
  
  source("load.R")
  source("extractSamples.R")
  source("getSamples.R")
  source("getFinalDataCaltech.R")
  source("fixFiles.R")
  
  #1) Load data
  data <- loadOffice()
  #positive<-c(251,145,253,232,96,86,99,179,204,223,183,197,200,233,249)

   #  getSamplesPositives(data, class, 0.10)
   #  getSamplesPositives(data, class, 0.20)
   #  getSamplesPositives(data, class, 0.30)
   #  #fixFilesBase(positive[i], 0.10)
   #  fixFilesTransfer(class, 0.10)
   #  fixFilesTest(class, 0.10)
   # # fixFilesBase(positive[i], 0.20)
   #  fixFilesTransfer(class, 0.20)
   #  fixFilesTest(class, 0.20)
   #  #fixFilesBase(positive[i], 0.30)
   #  fixFilesTransfer(class, 0.30)
   #  fixFilesTest(class, 0.30)
  
  #10 most related, although I will select 5 of them
#  related251<-c(248, 255, 244, 249, 243, 242, 254, 238, 246, 247)
#  related145<-c(146, 137, 139, 141, 143, 157, 150, 156, 148, 155)
#  related253<-c(254, 251, 248, 255, 249, 243, 256, 242, 246, 244)
#  related232<-c(238, 227, 231, 226, 235, 229, 243, 224, 234, 221)
#  related96<-c(95, 92, 94, 90, 99, 105, 93, 104, 87, 103)
#  related86<-c(94, 78, 87, 79, 88, 95, 77, 83, 91, 81)
#  related99<-c(94, 95, 104, 96, 101, 97, 108, 105, 91, 103)
#  related179<-c(182, 174, 175, 186, 173, 183, 176, 177, 189, 178)
#  related204<-c(205, 206, 208, 196, 203, 211, 202, 213, 195, 193)
#  related223<-c(221, 216, 226, 227, 219, 224, 232, 225, 217, 213)
#  related183<-c(182, 174, 175, 177, 186, 189, 178, 185, 176, 193)
#  related197<-c(195, 196, 192, 193, 203, 189, 199, 191, 190, 202)
#  related200<-c(195, 196, 203, 197, 205, 208, 202, 193, 199, 192, 189)
#  related233<-c(238, 234, 231, 232, 227, 235, 243, 226, 242, 237)
#  related249<-c(248, 255, 251, 243, 244, 242, 238, 247, 254, 246)
#
#  alltop5related<-c(related251[1:5],related145[1:5],related253[1:5],related232[1:5],related96[1:5],related86[1:5],related99[1:5],related179[1:5],related204[1:5],related223[1:5],related183[1:5],related197[1:5],related200[1:5],related233[1:5],related249[1:5])
#  alltop5related<-unique(alltop5related)
#  
#  for(i in 1:length(alltop5related))
#  {
   getSamplesRelated(data, class, 0.10)
   getSamplesRelated(data, class, 0.20)
   getSamplesRelated(data, class, 0.30)
   fixFilesBase(class, 0.10)
   #fixFilesTransfer(positive[i], 0.10)
   #fixFilesTest(alltop5related[i], 0.10)
   fixFilesBase(class, 0.20)
   #fixFilesTransfer(positive[i], 0.20)
   #fixFilesTest(alltop5related[i], 0.20)
   fixFilesBase(class, 0.30)
   #fixFilesTransfer(positive[i], 0.30)
   #fixFilesTest(alltop5related[i], 0.30)
#  }
}

# counts[order(-counts$freq) , ] #251, 145, 253, 232, 96
# counts[order(counts$freq) , ] #86, 99, 179, 204, 223
# (counts[order(counts$freq), ])[c(126,127,128,129,130),] #183, 197, 200, 233, 249