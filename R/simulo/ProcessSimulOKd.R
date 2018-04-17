# 
source('D:/DocumentsPerso/Dropbox/SimulO/R_Tools/SimulO_RTools.R')
# 
# filelist<-list.files(pattern = "^SimulKdA")
# SimulO<-MergeSimulO(filelist)
# 
# SimulO<-readSimulO("SimulKdAb3.txt")
# 
# DetecList<-names(SimulO$objects)[-(1:3)]
# 
# boxrangeX<-c(100:110)
# boxrangeY<-c(98:102)
# image(SimulO$detectors$D0m_F1_Intensity[boxrangeX,boxrangeY])
# #variation relative
# sd(SimulO$detectors$D0m_F1_Intensity[boxrangeX,boxrangeY])/mean(SimulO$detectors$D0m_F1_Intensity[boxrangeX,boxrangeY])
# 
# 
# 
# 
# ##### Ed profile
# ZSimulO<-NULL
# EdMatrix<-array(NA, dim = c(length(boxrangeX),length(boxrangeY),length(DetecList)))
# EdMean<-NULL
# for (i in 1:length(DetecList)){
#   ZSimulO<-c(ZSimulO,as.numeric(substr(DetecList[i],start = 2,stop=(nchar(DetecList[i])-1))))
#   EdMatrix[,,i]<-SimulO$detectors[[paste(DetecList[i],"_F1_Intensity",sep="")]][boxrangeX,boxrangeY]
#   EdMean<-c(EdMean,mean(EdMatrix[,,i]))
# }
# 
# 
# #Normalisation à la moyenne
# EdMatrix<-0.5*EdMatrix/mean(EdMatrix[,,1])
# EdMean<-0.5*EdMean/EdMean[1]
# 
# plot(ZSimulO,EdMean,log="y",type="b",col=2,xlab="depth",ylab="Ed")
# 
# for (i in 1:length(boxrangeX)){
#   for (j in 1:length(boxrangeY)){
#     lines(ZSimulO,EdMatrix[i,j,],col="gray")
#   }
# }
# 
# legend("bottomleft",legend=c("SimulO_3D mean sur box","SimulO_3D"),lty=1,col=c("red","gray"))
# 
# ##### Lu profile
# 
# LuMatrix<-array(NA, dim = c(length(boxrangeX),length(boxrangeY),length(DetecList)))
# LuMean<-NULL
# for (i in 1:length(DetecList)){
#   LuMatrix[,,i]<-SimulO$detectors[[paste(DetecList[i],"_F2_Radiance",sep="")]][boxrangeX,boxrangeY]
#   LuMean<-c(LuMean,mean(LuMatrix[,,i]))
# }
# 
# ## Normalisation
# ## ATTENTION valable pour une ouverture angulaire de 5 degree dans les simuls !!!! A modifier dans le cosinus !!!
# 
# LuMatrix<-length(boxrangeX)*length(boxrangeY)*LuMatrix/(sum(SimulO$detectors$`D0m_F1_Intensity`[boxrangeX,boxrangeY])*2*pi*(1-(cos(pi*5/180))^2))
# LuMean<-length(boxrangeX)*length(boxrangeY)*LuMean/(sum(SimulO$detectors$`D0m_F1_Intensity`[boxrangeX,boxrangeY])*2*pi*(1-(cos(pi*5/180))^2))
# 
# plot(ZSimulO,LuMean,log="y",type="b",col=2,xlab="depth",ylab="Lu")
# 
# for (i in 1:length(boxrangeX)){
#   for (j in 1:length(boxrangeY)){
#     lines(ZSimulO,LuMatrix[i,j,],col="gray")
#   }
# }

###################################
DataS<-MergeSimulO(fileslist=list.files(pattern = "^SimulKd_NoMeltP.*.txt"),TestConfig = T)
save(DataS,file = "SimulKd_MeltP_C_150m.RData")
save(DataS,file = "SimulKd_NoMeltP_150m.RData")

load("SimulKd_MeltP_C_150m.RData")

plot(apply(DataS$detectors$D10m_F2_Radiance[,95:105],1,mean))
image(DataS$detectors$D1m_F1_Intensity)

DetectSize=250
distTab=matrix(1,ncol=DetectSize,nrow=DetectSize)
center=c(DetectSize/2,DetectSize/2)
for (i in 1:DetectSize){
  for (j in 1:DetectSize){
    distTab[i,j]=sqrt((i-center[1])^2+(j-center[2])^2)
  }
}


nbreaks=200
cutdist<-cut(as.vector(distTab),breaks=nbreaks)
distcoef<-max(distTab)/nbreaks

## Intensity
maxE=max(tapply(as.vector(DataS$detectors$D0.5m_F1_Intensity),cutdist,mean))
ResultEd<-NULL
plot(NULL,NULL,xlim=c(0,120),ylim=c(maxE/1E3,maxE),log="y",xlab="Radius (m)",ylab="Ed")
for (i in grep("Intensity",names(DataS$detectors))){
  Edr<-tapply(as.vector(DataS$detectors[[i]]),cutdist,mean)
  lines(distcoef*(1:nbreaks),Edr,type="l",col=i)
  ResultEd<-rbind(ResultEd,Edr)
}

lines(c(80,80),c(maxE/1E3,maxE),lty=2)

##Ed
plot(NULL,NULL,xlim=c(maxE/1E2,maxE),ylim=c(-25,0),log="x")
for (i in 1:20){
  lines(ResultEd[,i],-c(seq(0.5,15,0.5),20,25),col=i)
}

legend("topleft",legend=distcoef*(1:nbreaks)[1:15],col=1:30,lty=1)



## Radiance
ResultLu<-NULL
maxL=max(tapply(as.vector(DataS$detectors$D0.5m_F2_Radiance),cutdist,mean))
plot(NULL,NULL,xlim=c(0,120),ylim=c(maxL/1E4,maxL),log="y",xlab="Radius (AU)",ylab="Lu")
for (i in grep("Radiance",names(DataS$detectors))){
  Lur<-tapply(as.vector(DataS$detectors[[i]]),cutdist,mean)
  lines(distcoef*(1:nbreaks),Lur,type="l",col=i)
  ResultLu<-rbind(ResultLu,Lur)
}
lines(c(80,80),c(maxL/1E3,maxL),lty=2)

##Lu
plot(NULL,NULL,xlim=c(maxL/1E2,maxL),ylim=c(-25,0),log="x")
for (i in 1:20){
  lines(ResultLu[,i],-c(seq(0.5,15,0.5),20,25),col=i)
}

legend("topleft",legend=distcoef*(1:nbreaks)[1:20],col=1:20,lty=1)

##### Relatif

nbreaks=50
cutdist<-cut(as.vector(distTab),breaks=nbreaks)
distcoef<-max(distTab)/nbreaks

## Intensity
maxE=1
plot(NULL,NULL,xlim=c(0,120),ylim=c(maxE/10,maxE),xlab="Radius (m)",ylab="Ed")
for (i in grep("Intensity",names(DataS$detectors))){
  Edr<-tapply(as.vector(DataS$detectors[[i]]),cutdist,mean)
  lines(distcoef*(1:nbreaks),Edr/max(Edr),type="l",col=i)
}

lines(c(80,80),c(maxE/1E3,maxE),lty=2)

## Radiance
maxL=1
#plot(NULL,NULL,xlim=c(0,120),ylim=c(maxL/1E4,maxL),log="y",xlab="Radius (AU)",ylab="Lu")
for (i in grep("Radiance",names(DataS$detectors))){
  Lur<-tapply(as.vector(DataS$detectors[[i]]),cutdist,mean)
  lines(distcoef*(1:nbreaks),Lur/max(Lur),type="l",col=i)
}
lines(c(80,80),c(maxL/1E3,maxL),lty=2)


