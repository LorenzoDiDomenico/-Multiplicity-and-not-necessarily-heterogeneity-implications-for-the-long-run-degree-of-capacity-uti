
 requiredPackages <- c("ggplot2","cowplot","gridExtra")
 
 for(p in requiredPackages){
   if(!require(p,character.only = TRUE)) install.packages(p)
   library(p,character.only = TRUE)
 }







b=350
mediaGDP=matrix(data=0,nrow=T)
minimoGDP=matrix(data=0,nrow=T)
maximoGDP=matrix(data=0,nrow=T)
mediaur=matrix(data=0,nrow=T)
minimour=matrix(data=0,nrow=T)
maximour=matrix(data=0,nrow=T)
datiGDP=matrix(data=0,nrow=T,ncol=3)
for(i in 1:T){
mediaGDP[i]=mean(res[i,])
st=sd(res[i,])/2
minimoGDP[i]=mediaGDP[i]-st
maximoGDP[i]=mediaGDP[i]+st
}
for(i in 1:T){
  mediaur[i]=mean(res[i+T,])
  st1=sd(res[i+T,])/2
  minimour[i]=mediaur[i]-st1
  maximour[i]=mediaur[i]+st1
}



datiGDP[,1]=mediaGDP
datiGDP[,2]=minimoGDP
datiGDP[,3]=maximoGDP
V4=minimoGDP[b:T]
V5=maximoGDP[b:T]
min1=min(V4)
max1=max(V5)
vectorGDP=c(mediaGDP[b:T],GDPreal[b:T])
vectormin=c(V4,GDPreal[b:T])
vectormax=c(V5,GDPreal[b:T])
tempo=c((b-300):(T-300),(b-300):(T-300))
aa=rep("Multifirm model",(T-b+1))
bb=rep("Aggregate model",(T-b+1))
cc=c(aa,bb)

data1 <- data.frame(tempo,cc,vectorGDP,vectormin,vectormax)
colnames(data1) <- c('Time','Legend','GDP','MIN','MAX')

p1=ggplot(data1,aes(x=Time, y=GDP,group=Legend,fill=Legend))+
  
  #stat_summary(fun.y=mean, geom="line", size=.7, aes(color=V3))+
  geom_line(aes(color = Legend, linetype = Legend)) +
  #geom_line()+ #color="red", size=2)+
  labs(title="                                                                              Real GDP", x="", y="")+ theme_classic()+
  theme(plot.title=element_text(size=12))+
  #scale_fill_viridis(discrete = T)+
  scale_color_manual(values = c("steelblue","darkred"))+
  #theme(plot.title = element_text(hjust = 0.5,size=10),legend.position = c(0.2, 0.07),legend.direction = "horizontal",legend.title=element_blank())+
  #theme(legend.title = element_blank())+
  #theme(legend. position = "none")+
  theme(legend.position = "none")+
  geom_ribbon(aes(ymin=MIN, ymax=MAX), alpha = 0.4, fill = "grey90",colour=NA)+
  ylim(min1*0.999, max1*1.001)
#p1


V4b=minimour[b:T]
V5b=maximour[b:T]
min1=min(V4b)
max1=max(V5b,u_raggregate[b:T])
vectorur=c(mediaur[b:T],u_raggregate[b:T])
vectormin=c(V4b,u_raggregate[b:T])
vectormax=c(V5b,u_raggregate[b:T])
aa=rep("Multifirm model",(T-b+1))
bb=rep("Aggregate model",(T-b+1))
cc=c(aa,bb)

data1 <- data.frame(tempo,cc,vectorur,vectormin,vectormax)
colnames(data1) <- c('Time','Legend','ur','MIN','MAX')

p2=ggplot(data1,aes(x=Time, y=ur,group=Legend,fill=Legend))+
  
  #stat_summary(fun.y=mean, geom="line", size=.7, aes(color=V3))+
  geom_line(aes(color = Legend, linetype = Legend)) +
  #geom_line()+ #color="red", size=2)+
  labs(title="      Aggregate degree of capacity utilization", x="", y="")+ theme_classic()+
  theme(plot.title=element_text(size=11))+
  #scale_fill_viridis(discrete = T)+
  scale_color_manual(values = c("steelblue","darkred"))+
  #theme(plot.title = element_text(hjust = 0.5,size=10),legend.position = c(0.2, 0.07),legend.direction = "horizontal",legend.title=element_blank())+
  theme(legend.title = element_blank())+
  geom_ribbon(aes(ymin=MIN, ymax=MAX), alpha = 0.4, fill = "grey90",colour=NA)+
  ylim(min1*0.9999, max1*1.005)
#p2



mediaurbig=matrix(data=0,nrow=T)
minimourbig=matrix(data=0,nrow=T)
maximourbig=matrix(data=0,nrow=T)
for(i in 1:T){
  mediaurbig[i]=mean(res[(i+2*T),])
  st1=sd(res[(i+2*T),])/2
  minimourbig[i]=mediaurbig[i]-st1
  maximourbig[i]=mediaurbig[i]+st1
}
mediaursmall=matrix(data=0,nrow=T)
minimoursmall=matrix(data=0,nrow=T)
maximoursmall=matrix(data=0,nrow=T)
for(i in 1:T){
  mediaursmall[i]=mean(res[(i+3*T),])
  st1=sd(res[(i+3*T),])/2
  minimoursmall[i]=mediaursmall[i]-st1
  maximoursmall[i]=mediaursmall[i]+st1
}
mediaurmedium=matrix(data=0,nrow=T)
minimourmedium=matrix(data=0,nrow=T)
maximourmedium=matrix(data=0,nrow=T)
for(i in 1:T){
  mediaurmedium[i]=mean(res[(i+4*T),])
  st1=sd(res[(i+4*T),])/2
  minimourmedium[i]=mediaurmedium[i]-st1
  maximourmedium[i]=mediaurmedium[i]+st1
}

aa=rep("big",(T-b+1))
bb=rep("small",(T-b+1))
cc=rep("medium",(T-b+1))
dd=c(aa,bb,cc)
tempo=c((b-300):(T-300),(b-300):(T-300),(b-300):(T-300))


vectorur=c(mediaurbig[b:T],mediaursmall[b:T],mediaurmedium[b:T])
vectormin=c(minimourbig[b:T],minimoursmall[b:T],minimourmedium[b:T])
vectormax=c(maximourbig[b:T],maximoursmall[b:T],maximourmedium[b:T])
min1=min(vectormin)
max1=max(vectormax)


data2 <- data.frame(tempo,dd,vectorur,vectormin,vectormax)
colnames(data2) <- c('Time','Legend','ur','MIN','MAX')

p3=ggplot(data2,aes(x=Time, y=ur,colour=Legend,shape = Legend))+
  
  #stat_summary(fun.y=mean, geom="line", size=.7, aes(color=V3))+
  geom_line()+
 # geom_line(aes(color = Legend, linetype = Legend)) +
  #geom_line()+ #color="red", size=2)+
  labs(title="  Degree of capacity utilization by firms size percentile", x="", y="")+ theme_classic()+
  theme(plot.title=element_text(size=11))+
  #theme(legend.position = "none") +
  #scale_fill_viridis(discrete = T)+
  #scale_color_manual(values = c("darkorchid","darkblue","coral1"))+
  scale_colour_manual(name = "", values = c("darkorchid","darkblue","coral1"), 
                      labels = expression(x<P[10],paste(P[30]<"",x<P[60]), x>P[90])) + 
  #paste("Value is ", sigma,",", R^{2},'=0.6')
   # scale_shape_discrete(name = "", 
   #                    labels = expression(L[1], L[2], L[infinity]))+
   #theme(plot.title = element_text(hjust = 0.5,size=10),legend.position = c(0.2, 0.07),legend.direction = "horizontal",legend.title=element_blank())+
  #theme(legend.title = element_blank())+
  geom_ribbon(aes(ymin=MIN, ymax=MAX), alpha = 0.4, fill = "grey90",colour=NA)+
  ylim(min1*0.9999, max1*1.005)

#p3
bottom_row <- plot_grid( p2, p3,ncol=2, labels = c('', ''))
grafico<-plot_grid( p1,bottom_row,nrow=2, labels = c('', ''))
grafico

#library(patchwork)


# Figure 3 ############



m=rbind(c(1))
layout(m)
par(tcl=-0.2,mgp=c(2,0.4,0))
par(mar=c(1.4,1.8,3.6,2))

mini=min(res[(9*T+b):(10*T),1])*0.79
maxi=max(res[(9*T+b):(10*T),1])*1.04
plot(res[(9*T+b):(10*T),1],type='l',ylim=range(mini,maxi),col=c("darkred"),font.main=1,lwd=1,lty=2,main=expression(paste("Mean difference ", (zeta))),cex.main=1.2, xaxt='n',xlab = NA,ylab="")
axis(side = 2,col= "darkred",col.axis="darkred")
par(new=TRUE)
mini=min(meanratio2[b:T])*4
maxi=max(meanratio2[b:T])*15
plot(meanratio2[b:T],xaxt='n',yaxt='n',type='l',ylim=range(mini,maxi),col=c("steelblue"),lwd=1,lty=3,font.main=1,cex.main=0.75,main="",ylab = NA,xlab = NA)
axis(side = 4,col = "steelblue",col.axis="steelblue")
legend("bottom",ncol=2,c("Aggregate model","Multifirm model"),  bty = "n", cex =1.1, text.font =1,lty=c(3,3), lwd=c(2,2), col = c("steelblue","darkred"), box.lwd=0)



# Figure 4 ############


#iNVESTMENT SHARE #
mediainv=matrix(data=0,nrow=T)
minimoinv=matrix(data=0,nrow=T)
maximoinv=matrix(data=0,nrow=T)
multiplier=2
mediaG=matrix(data=0,nrow=T)
minimoG=matrix(data=0,nrow=T)
maximoG=matrix(data=0,nrow=T)
mediaC=matrix(data=0,nrow=T)
minimoC=matrix(data=0,nrow=T)
maximoC=matrix(data=0,nrow=T)
for(i in 1:T){
  mediaG[i]=mean(res[(i+7*T),])
  mediaC[i]=mean(res[(i+5*T),])
  mediainv[i]=mean(res[(i+6*T),])
}

multipliera=(1-Investmentwight[T]*multiplier)/(Gweigth[T]+Cweight[T])
multiplierd=(1-multiplier*(mean(mediainv[(T-100):T])))/(mean(mediaG[(T-100):T])+mean(mediaC[(T-100):T]))



for(i in 1:T){
  mediainv[i]=mean(res[(i+6*T),]*multiplier)
  st=sd(res[(i+6*T),]*multiplier)/2
  minimoinv[i]=mediainv[i]-st
  maximoinv[i]=mediainv[i]+st
}


Investmentwightb=Investmentwight*multiplier

min1=min(minimoinv[b:T],Investmentwightb[b:T])
max1=max(maximoinv[b:T],Investmentwightb[b:T])
vectorinv=c(mediainv[b:T],Investmentwightb[b:T])
vectormin=c(minimoinv[b:T],Investmentwightb[b:T])
vectormax=c(maximoinv[b:T],Investmentwightb[b:T])
tempo=c((b-300):(T-300),(b-300):(T-300))
aa=rep("Multifirm model",(T-b+1))
bb=rep("Aggregate model",(T-b+1))
cc=c(aa,bb)

data1 <- data.frame(tempo,cc,vectorinv,vectormin,vectormax)
colnames(data1) <- c('Time','Legend','GDP','MIN','MAX')

p4=ggplot(data1,aes(x=Time, y=GDP,group=Legend,fill=Legend))+
  
  #stat_summary(fun.y=mean, geom="line", size=.7, aes(color=V3))+
  geom_line(aes(color = Legend, linetype = Legend)) +
  #geom_line()+ #color="red", size=2)+
  labs(title="                                                                                   Investment share", x="", y="")+ theme_classic()+
  theme(plot.title=element_text(size=11))+#+ theme_classic()+
  #scale_fill_viridis(discrete = T)+
  scale_color_manual(values = c("steelblue","darkred"))+
  #theme(plot.title = element_text(hjust = 0.5,size=10),legend.position = c(0.2, 0.07),legend.direction = "horizontal",legend.title=element_blank())+
  #theme(legend.title = element_blank())+
  #theme(legend. position = "none")+
  #theme(legend.position = "none")+
  geom_ribbon(aes(ymin=MIN, ymax=MAX), alpha = 0.15, fill = "grey90",colour=NA)+
  theme(
    legend.direction = "horizontal",
    legend.position = c(0.95, 1.15),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(2, 2, 2, 2)
  )+  theme(legend.title = element_blank())

#p4




# Public expenditure share #

for(i in 1:T){
  mediaG[i]=mean(res[(i+7*T),]*multiplierd)
  st=sd(res[(i+7*T),]*multiplierd)/2
  minimoG[i]=mediaG[i]-st
  maximoG[i]=mediaG[i]+st
  
  mediaC[i]=mean(res[(i+5*T),]*multiplierd)
  st=sd(res[(i+5*T),]*multiplierd)/2
  minimoC[i]=mediaC[i]-st
  maximoC[i]=mediaC[i]+st
  
}
Gweigthb=Gweigth*multipliera
Cweightb=Cweight*multipliera


min1=min(minimoG[b:T],Gweigthb[b:T])
max1=max(maximoG[b:T],Gweigthb[b:T])
vectorG=c(mediaG[b:T],Gweigthb[b:T])
vectormin=c(minimoG[b:T],Gweigthb[b:T])
vectormax=c(maximoG[b:T],Gweigthb[b:T])

data1 <- data.frame(tempo,cc,vectorG,vectormin,vectormax)
colnames(data1) <- c('Time','Legend','GDP','MIN','MAX')

p5=ggplot(data1,aes(x=Time, y=GDP,group=Legend,fill=Legend))+
  
  #stat_summary(fun.y=mean, geom="line", size=.7, aes(color=V3))+
  geom_line(aes(color = Legend, linetype = Legend)) +
  #geom_line()+ #color="red", size=2)+          
  labs(title="                                                                            Public expenditure share", x="", y="")+ theme_classic()+
  theme(plot.title=element_text(size=11))+
  #scale_fill_viridis(discrete = T)+
  scale_color_manual(values = c("steelblue","darkred"))+
  #theme(plot.title = element_text(hjust = 0.5,size=10),legend.position = c(0.2, 0.07),legend.direction = "horizontal",legend.title=element_blank())+
  #theme(legend.title = element_blank())+
  #theme(legend. position = "none")+
  theme(legend.position = "none")+
  geom_ribbon(aes(ymin=MIN, ymax=MAX), alpha = 0.15, fill = "grey90",colour=NA)+
  ylim(min1*0.999, max1*1.001)
#p5



min1=min(minimoC[b:T],Cweightb[b:T])
max1=max(maximoC[b:T],Cweightb[b:T])
vectorC=c(mediaC[b:T],Cweightb[b:T])
vectormin=c(minimoC[b:T],Cweightb[b:T])
vectormax=c(maximoC[b:T],Cweightb[b:T])

data1 <- data.frame(tempo,cc,vectorC,vectormin,vectormax)
colnames(data1) <- c('Time','Legend','GDP','MIN','MAX')

p6=ggplot(data1,aes(x=Time, y=GDP,group=Legend,fill=Legend))+
  
  #stat_summary(fun.y=mean, geom="line", size=.7, aes(color=V3))+
  geom_line(aes(color = Legend, linetype = Legend)) +
  #geom_line()+ #color="red", size=2)+
  labs(title="                                                                                    Consumption share", x="", y="")+ theme_classic()+
  theme(plot.title=element_text(size=11))+
  #scale_fill_viridis(discrete = T)+
  scale_color_manual(values = c("steelblue","darkred"))+
  #theme(plot.title = element_text(hjust = 0.5,size=10),legend.position = c(0.2, 0.07),legend.direction = "horizontal",legend.title=element_blank())+
  #theme(legend.title = element_blank())+
  #theme(legend. position = "none")+
  theme(legend.position = "none")+
  geom_ribbon(aes(ymin=MIN, ymax=MAX), alpha = 0.2, fill = "grey90",colour=NA)+
  ylim(min1*0.999, max1*1.001)
#p6
grafico2<-plot_grid( p4,p5,p6,nrow=3, labels = c('', '',''))
grafico2
