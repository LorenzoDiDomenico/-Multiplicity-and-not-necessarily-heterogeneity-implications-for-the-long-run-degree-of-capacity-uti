
###Empirical validation_part 2 #####
#rm(list=ls()) 

#Before lauching the code, load the dataset: crossandautocorrelationsmultivariate.RData
requiredPackages <- c("mFilter","parallel")

for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}




dis_sort=sort(resfilter[1,],decreasing = FALSE)
lagg=20

if(FALSE){
for(i in 1:lung){
  
  # conta=which(distanza==distanza2[i])
  #conta=which(ur==ursort[i])
  conta1=which(resfilter[1,]==dis_sort[i])*2
  conta=which(resfilter[1,]==dis_sort[i])
  if(length(conta)>1){
    #conta=which(ur==ordifsort[i])[1]
    # ur[conta]=1
    print("problemconta")
  }
  
  mypath <- file.path("C:","Users","lored","Desktop","grafici_validazione0.8",paste("myplot_", i, ".png", sep = ""))
  png(file=mypath)
  mytitle = paste("my title is", i)
  
  #m=rbind(c(1,1),c(2,2))
  #layout(m)
  # par(tcl=0,mgp=c(2,0.4,0))
  #par(mar=c(2,2,2,2))
  
  # 
  #GDP
  m=rbind(c(1),c(2),c(3,4,5))
  layout(m)
  par(tcl=0,mgp=c(2,0.4,0))
  par(mar=c(2,2,2,2))
  # plot(res[bb:(5*T+a),conta],type='l',col=1,ylim=range(res[bb:(5*T+a),conta]),main='grado di utilizzo',xaxt='n')
  mingdp=min(res[(20+b):(T+20),conta1])*0.97
  maxgdp=max(res[(20+b):(T+20),conta1])*1.03
  plot(res[(20+b):(T+20),conta1],type="l",main="GDP",ylim=range(mingdp,maxgdp))
  #legend("top",ncol=6,legend=c("phi",res[(3*T+1),conta],"al",res[(3*T+2),conta],"dk",res[(3*T+6),conta],"x",res[(3*T+17),conta],"ur",res[(3*T+21),conta],"dis",urdifsort[i]),cex = 0.6,box.lty=0)
  legend("top",ncol=8,legend=c("phi",res[1,conta1],"v",res[2,conta1],"z",res[5,conta1],"dk",res[6,conta1],"grado",round(res[20+1+5*T,conta1],3)),cex = 0.6,box.lty=0)
  
  
  plot(resfilter[23:43,conta],ylim=range(-1,1), type='l')
  par(new="TRUE")
  plot(aGDPr,ylim=range(-1,1), type='l',lty=2)
  #title("GDP",line=-1)
  title("aGDP",line=-1,font.main=1,cex.main=1.2)
  #areaGDP=sum(abs(aGDP-aGDPr))
  #legend("bottomright",c("Artificial","USA data"),  bty = "n", cex = 1, lty=c(1,2), lwd=c(2,2), col = c(1,1), box.lwd=0)
  #legend("topright",legend=c(areaGDP))
  plot(resfilter[44:64,conta],ylim=range(-1,1), type='l')
  par(new="TRUE")
  plot(aconsr,ylim=range(-1,1), type='l',lty=2)
  title("Consumption",line=-1,font.main=1,cex.main=1.2)
  #areach=sum(abs(ach-aconsr))
  plot(resfilter[2:22,conta],ylim=range(-1,1), type='l')
  par(new="TRUE")
  plot(ainvr,ylim=range(-1,1), type='l',lty=2)
  title("Investments",line=-1,font.main=1,cex.main=1.2)
  #areainv=sum(abs(ainv-ainvr))
  
  
  dev.off()
  
}
}

scelta=which(resfilter[1,]==dis_sort[19])*2
mingdp=min(res[(20+b):(T+20),scelta])*0.97
maxgdp=max(res[(20+b):(T+20),scelta])*1.03
#plot(res[(20+b):(T+20),scelta],type='l',ylim=range(mingdp,maxgdp))


# Grafici validazione empirica #####################
b=250
GDP.ts=as.ts(log(res[(20+b):(T+20),scelta]),frequency=1,start=2)
in.ts=as.ts(log(res[(2*T+b+20):(3*T+20),scelta]),frequency=1,start=2)
ch.ts=as.ts(log(res[(T+b+20):(2*T+20),scelta]),frequency=1,start=2)
GDP.hp=hpfilter(GDP.ts,freq=1600,type="lambda")
in.hp=hpfilter(in.ts,freq=1600,type="lambda")
ch.hp=hpfilter(ch.ts,freq=1600,type="lambda")



# Grafici componenti cicliche ####
m=rbind(c(1))
layout(m)
par(tcl=-0.2,mgp=c(0,0.4,0))
par(mar=c(1.5,2,2,1.5))

mini=min(GDP.hp$cycle,ch.hp$cycle,in.hp$cycle)*2
maxi=max(GDP.hp$cycle,ch.hp$cycle,in.hp$cycle)*2
plot(in.hp$cycle,type='l',ylim=range(mini,maxi),lty=2,col=8,lwd=2,ylab = NA,xlab = NA, main="Cyclical components")
par(new="TRUE")
plot(ch.hp$cycle,type='l',ylim=range(mini,maxi),lty=3,col=6,ylab = NA,xlab = NA)
par(new="TRUE")
plot(GDP.hp$cycle,type='l',ylim=range(mini,maxi),lty=1,ylab = NA,xlab = NA)
legend("bottomright",c("GDP","Investments","Consumption"),  bty = "n", cex = 1, lty=c(1,2,3), lwd=c(2,2,2), col = c(1,8,6), box.lwd=0)



GDP.ts=as.ts(res[(20+b):(T+20),scelta],frequency=1,start=2)
in.ts=as.ts(res[(2*T+b+20):(3*T+20),scelta],frequency=1,start=2)
ch.ts=as.ts(res[(T+b+20):(2*T+20),scelta],frequency=1,start=2)
GDP.hp=hpfilter(GDP.ts,freq=1600,type="lambda")
in.hp=hpfilter(in.ts,freq=1600,type="lambda")
ch.hp=hpfilter(ch.ts,freq=1600,type="lambda")



# Autocorrelazioni ####
m=rbind(c(1,1),c(2,3))
layout(m)
par(tcl=0,mgp=c(2,0.4,0))
par(mar=c(1.5,1.4,1.5,0.8))

#GDP
aGDP=acf(GDP.hp$cycle, lag.max=lagg, plot=FALSE)
aGDP=as.numeric(unlist(aGDP))[1:(lagg+1)]

plot(aGDP,ylim=range(-1,1), type='l')
par(new="TRUE")
plot(aGDPr,ylim=range(-1,1), type='l',lty=2)
#title("GDP",line=-1)
title("GDP",line=-1,font.main=1,cex.main=1.2)
areaGDP=sum(abs(aGDP-aGDPr))
legend("bottomright",c("Model","USA data"),  bty = "n", cex = 1, lty=c(1,2), lwd=c(2,2), col = c(1,1), box.lwd=0)
#legend("topright",legend=c(areaGDP))
#legend("topright",ncol=5,legend=c("z",z,"dk",dk,"perc",perc))

# #CONSUMO
# acons=acf(yc.hp$cycle, lag.max=lagg, plot=FALSE)
# acons=as.numeric(unlist(acons))[1:(lagg+1)]
# plot(acons,ylim=range(-1,1), type='l')
# par(new="TRUE")
# plot(aconsr,ylim=range(-1,1), type='l',lty=2)
# title("C",line=-1)
# areaC=sum(abs(acons-aconsr))
# legend("topright",legend=c(areaC))
# 
#Consumption hh
ach=acf(ch.hp$cycle, lag.max=lagg, plot=FALSE)
ach=as.numeric(unlist(ach))[1:(lagg+1)]
plot(ach,ylim=range(-1,1), type='l')
par(new="TRUE")
plot(aconsr,ylim=range(-1,1), type='l',lty=2)
title("Consumption",line=-1,font.main=1,cex.main=1.2)
areach=sum(abs(ach-aconsr))
#legend("topright",legend=c(areach))

# Investimenti
ainv=acf(in.hp$cycle, lag.max=lagg, plot=FALSE)
ainv=as.numeric(unlist(ainv))[1:(lagg+1)]
plot(ainv,ylim=range(-1,1), type='l')
par(new="TRUE")
plot(ainvr,ylim=range(-1,1), type='l',lty=2)
title("Investments",line=-1,font.main=1,cex.main=1.2)
areainv=sum(abs(ainv-ainvr))
#legend("topright",legend=c(areainv))

mtext("Artificial and real auto-correlations of Bandpass - filtered series up to 20th lag" , side = 3,line = -1.3,font=2,cex=0.8, outer = TRUE)


##### Cross-correlazioni ####

#Matrici crosscorrelazioni
#GDP

m=rbind(c(1,2),c(3,4))
layout(m)
par(tcl=0,mgp=c(2,0.4,0))
par(mar=c(2.2,2,2.5,1.8))
lag2=10
lagg=lag2*2
x=seq(-lag2,lag2)
crossGDP=ccf(GDP.hp$cycle,GDP.hp$cycle, lag.max=lag2, type=c("correlation"),plot=FALSE)
crossGDP=as.numeric(unlist(crossGDP))[1:(lagg+1)]
crossRealGDP=ccf(realGDP.hp$cycle,realGDP.hp$cycle, lag.max=lag2, type=c("correlation"),plot=FALSE)
crossRealGDP=as.numeric(unlist(crossRealGDP))[1:(lagg+1)]
plot(x,crossGDP,type='l',ylim=range(-0.3,1),lty=1)
par(new="TRUE")
plot(x,crossRealGDP,type='l',ylim=range(-0.3,1),lty=2)
title("GDP",line=0.4,font.main=1,cex.main=1.2)

#INVESTMENTS
crossIN=ccf(in.hp$cycle,in.hp$cycle, lag.max=10, type=c("correlation"),plot=FALSE)
crossIN=as.numeric(unlist(crossIN))[1:(lagg+1)]
crossRealIN=ccf(realIn.hp$cycle,realIn.hp$cycle, lag.max=10, type=c("correlation"),plot=FALSE)
crossRealIN=as.numeric(unlist(crossRealIN))[1:(lagg+1)]
plot(x,crossIN,type='l',ylim=range(-0.3,1),lty=1)
par(new="TRUE")
plot(x,crossRealIN,type='l',ylim=range(-0.3,1),lty=2)
title("Investments",line=0.4,font.main=1,cex.main=1.2)

#CONSUMPTION con G
crossCONS=ccf(ch.hp$cycle,ch.hp$cycle, lag.max=10, type=c("correlation"),plot=FALSE)
crossCONS=as.numeric(unlist(crossCONS))[1:(lagg+1)]
crossRealCons=ccf(realc.hp$cycle,realc.hp$cycle, lag.max=10, type=c("correlation"),plot=FALSE)
crossRealCons=as.numeric(unlist(crossRealCons))[1:(lagg+1)]
plot(x,crossCONS,type='l',ylim=range(-0.3,1),lty=1)
par(new="TRUE")
plot(x,crossRealCons,type='l',ylim=range(-0.3,1),lty=2)
title("Consumpition",line=0.4,font.main=1,cex.main=1.2)

#CONSUMPTION senza G
crossCONShh=ccf(ch.hp$cycle,ch.hp$cycle, lag.max=10, type=c("correlation"),plot=FALSE)
crossCONShh=as.numeric(unlist(crossCONShh))[1:(lagg+1)]
crossRealCons=ccf(realc.hp$cycle,realc.hp$cycle, lag.max=10, type=c("correlation"),plot=FALSE)
crossRealCons=as.numeric(unlist(crossRealCons))[1:(lagg+1)]
plot(x,crossCONShh,type='l',ylim=range(-0.3,1),lty=1)
par(new="TRUE")
plot(x,crossRealCons,type='l',ylim=range(-0.3,1),lty=2)
title("C + G",line=0.4,font.main=1,cex.main=1.2)

mtext("Artificial and real cross-correlations of Bandpass - filtered series up to 10th lag" , side = 3,line = -1,font=2,cex=0.9, outer = TRUE)

