


# Empirical validation_part 1 ####


requiredPackages <- c("mFilter","parallel")

for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}



#rm(list=ls()) 

#Before processing the code, load the dataset: multivariateanalysis.RData ###


b=250
lagg=20
#DATI REALI
aa=read.csv("C:/Users/lored/Desktop/consumption.csv",sep=",")
realc=aa[2]
realc.ts=as.ts(realc,frequency=1,start=2)
bb=read.csv("C:/Users/lored/Desktop/GDP.csv",sep=",")
realGDP=bb[2]
realGDP.ts=as.ts(realGDP,frequency=1,start=2)
c=read.csv("C:/Users/lored/Desktop/Investments.csv",sep=",")
realIn=c[2]
realIn.ts=as.ts(realIn,frequency=1,start=2)
#filtro dati reali
realc.hp=hpfilter(realc.ts,freq=1600,type="lambda")
realGDP.hp=hpfilter(realGDP.ts,freq=1600,type="lambda")
realIn.hp=hpfilter(realIn.ts,freq=1600,type="lambda")

#Autocorrelazione GDP USA
aGDPr=acf(realGDP.hp$cycle, lag.max=lagg, plot=FALSE)
aGDPr=as.numeric(unlist(aGDPr))[1:(lagg+1)]
#Autocorrelazione Consumo USA
aconsr=acf(realc.hp$cycle, lag.max=lagg, plot=FALSE)
aconsr=as.numeric(unlist(aconsr))[1:(lagg+1)]
#Autocorrelazione Investimenti USA
ainvr=acf(realIn.hp$cycle, lag.max=lagg, plot=FALSE)
ainvr=as.numeric(unlist(ainvr))[1:(lagg+1)]


lung=length(res[1,])/2
serial=seq(2, by = 2, len = lung)

hh=length((20+b):(T+20))
GDP.ts=matrix(data=0, nrow=hh,ncol=lung*2)
in.ts=matrix(data=0, nrow=hh,ncol=lung*2)
ch.ts=matrix(data=0, nrow=hh,ncol=lung*2)
GDP.hp=matrix(data=0, nrow=hh,ncol=lung*2)
in.hp=matrix(data=0, nrow=hh,ncol=lung*2)
ch.hp=matrix(data=0, nrow=hh,ncol=lung*2)

GDP.ts[,]=as.ts(res[(20+b):(T+20),],frequency=1,start=2)
in.ts[,]=as.ts(res[(2*T+b+20):(3*T+20),],frequency=1,start=2)
ch.ts[,]=as.ts(res[(T+b+20):(2*T+20),],frequency=1,start=2)

serial2=serial[1:20]
ff<-function(serial){
  lagg=20
  datoGDP=hpfilter(GDP.ts[,serial], freq=1600, type="lambda")
  datoGDP2=datoGDP$cycle
  datoinv=hpfilter(in.ts[,serial], freq=1600, type="lambda")
  datoinv2=datoinv$cycle
  datocons=hpfilter(ch.ts[,serial], freq=1600, type="lambda")
  datocons2=datocons$cycle
  
  
  aGDP=acf(datoGDP2, lag.max=lagg, plot=FALSE)
  aGDP=as.numeric(unlist(aGDP))[1:(lagg+1)]
  ach=acf(datocons2, lag.max=lagg, plot=FALSE)
  ach=as.numeric(unlist(ach))[1:(lagg+1)]
  ainv=acf(datoinv2, lag.max=lagg, plot=FALSE)
  ainv=as.numeric(unlist(ainv))[1:(lagg+1)]
  
  var=abs(aGDP-aGDPr)
  var2=var*100/abs(aGDPr)
  distGDP=mean(var2)
  
  var=abs(ach-aconsr)
  var2=var*100/abs(aconsr)
  distcons=mean(var2)
  
  var=abs(ainv-ainvr)
  var2=var*100/abs(ainvr)
  distinv=mean(var2)
  
  disttot=distinv+distcons+distGDP
  #dismean=(distinv+distcons+distGDP)/3
  
  
  c(disttot,ainv,aGDP,ach)
}
numCores <- detectCores()-1
cl <- makeCluster(numCores)
# # # # # # 
clusterEvalQ(cl, library(mFilter))
clusterExport(cl,c("GDP.ts","in.ts","ch.ts","aGDPr","aconsr","ainvr"))
resfilter=parSapply(cl,serial, ff)
stopCluster(cl)

#save.image("/Users/lored/Desktop/crossandautocorrelationsmultivariate.RData")     















