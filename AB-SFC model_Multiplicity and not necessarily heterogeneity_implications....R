# AB-SFC Model-  Multiplicity and not necessarily heterogeneity: implications for the long-run degree of capacity utilization 



requiredPackages <- c("VGAM","parallel")

for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}




#library(tictoc)
#library(TTR)
#library(mFilter)
#library(beepr)



rm(list=ls()) 
RNGkind(kind = NULL, normal.kind = NULL, sample.kind = "Rejection")

eterogeneita=0#se 1 c'è ETEORGENITà NELLe aspettative
sceltastocastica=1#se 1 è stocasstico
sceltastocasticaK=1 # SE 1 LA SCLTA DI K è STOCASTICA
equaldistribution=0

T=900
MC=1
ygeneral=matrix(data=0, nrow=T, ncol=2)
stockG=matrix(data=0,nrow=T,ncol=2)
GDPreale=matrix(data=0,nrow=T,ncol=MC)
debpilgen=matrix(data=0,nrow=T,ncol=2)
deb_yc=matrix(data=0, nrow=T, ncol=MC)
yc=matrix(data=0, nrow=T, ncol=MC)


# Parametri per analisi multivariata #####
npar=20
Nv=1
Nphi=1
Nx=1
Nlim=1
Nz=1
Ndk=1
Nperc=1
Nqex=1
Nbeta=1
Nwgov=1
Nflex=1
Nint=1
Ngpub=1
Nparcons=1
Nswic=1
Numin=1
NNF=1
nhmese=1
nprobex=1
ncomb=Nv*Nphi*Nlim*Nz*Ndk*Nperc*Nqex*Nbeta*Nwgov*Nflex*Nint*Ngpub*Nparcons*Nswic*Numin*NNF*nhmese*nprobex*Nx



trial=seq(1,ncomb)
parametri=matrix(data=0, nrow=ncomb, ncol=npar)
dati=matrix(data=0,nrow=1,ncol=(npar-1))
datiIn=matrix(data=0,nrow=1,ncol=npar)
progval=c(data=0,ncol=npar)
dati[1,1]=Nphi
dati[1,2]=Nv
dati[1,3]=Nlim
dati[1,4]=Nz
dati[1,5]=Ndk
dati[1,6]=Nperc
dati[1,7]=Nqex
dati[1,8]=Nbeta
dati[1,9]=Nwgov
dati[1,10]=Nflex
dati[1,11]=Nint
dati[1,12]=Ngpub
dati[1,13]=Nparcons
dati[1,14]=Nswic
dati[1,15]=Numin
dati[1,16]=NNF
dati[1,17]=nhmese
dati[1,18]=nprobex
dati[1,19]=Nx


counter=1
phivalue=0.5 #0.25 #0.2
datiIn[1,1]=phivalue
for(i in 1:Nphi){
  progphi=0
  progval[1]=progphi
  phivalue=phivalue+progphi
  vvalue=1.05# 0.45 #rapporto capitale/lavoro
  datiIn[1,2]=vvalue
  for(j in 1:Nv){
    progv=0
    progval[2]=progv
    vvalue=vvalue+progv #  
    liminfvalue=2000# numero di periodi consecuiti massimo (in cui le scorte sono state sotto al desiderato) oltre il quale si varia il markup
    limsupvalue=2000# numero di periodi consecuiti massimo (in cui le scorte sono state sopra al desiderato) oltre il quale si varia il markup
    datiIn[1,3]=liminfvalue
    datiIn[1,4]=limsupvalue
    for(h in 1:Nlim){
      proglim=0
      progval[3]=proglim
      progval[4]=proglim
      liminfvalue=liminfvalue+proglim
      limsupvalue=limsupvalue+proglim
      zvalue=50 #50 #50#tempo di vita del capitale
      datiIn[1,5]=zvalue
      for(gg in 1:Nz){
        progz=0
        progval[5]=progz
        zvalue=zvalue+progz   
        dkvalue=6#9 #8  #numero periodi per la produzione del bene capitale
        datiIn[1,6]=dkvalue
        for(ll in 1:Ndk){
          progdk=0
          progval[6]=progdk
          dkvalue=dkvalue+progdk
          percvalue=0.1 #perso del settore pubblico
          datiIn[1,7]=percvalue 
          for(xx in 1:Nperc){
            progperc=0
            progval[7]=progperc
            percvalue=percvalue+progperc
            qexvalue=500  #quantità iniziale attesa dalle imrpese C
            datiIn[1,8]=qexvalue 
            for(oo in 1:Nqex){
              progqex=0
              progval[8]=progqex
              qexvalue=qexvalue+progqex
              betavalue=0.8 #95   #aspettativa adattiva
              datiIn[1,9]=betavalue 
              for(ww in 1:Nbeta){
                progbeta=0
                progval[9]=progbeta
                betavalue=betavalue+progbeta
                alfawgovvalue=0 #0.1  # % del salario di mercato pagato come sussidio disoccupazione dal governo
                datiIn[1,10]=alfawgovvalue 
                for(ppp in 1:Nwgov){
                  progwgov=0
                  progval[10]=progwgov
                  alfawgovvalue=alfawgovvalue+progwgov
                  flexvalue=0
                  datiIn[1,11]=flexvalue 
                  for(popi in 1:Nflex){
                    progflex=0
                    progval[11]=progflex
                    flexvalue=flexvalue+progflex
                    
                    intvalue=0 #0.00016  #tasso d'interesse sui titoli 
                    datiIn[1,12]=intvalue 
                    for(oio in 1:Nint){
                      progint=0
                      progval[12]=progint
                      intvalue=intvalue+progint
                      gpubvalue=0 #0.0001 #tasso di crescita della componente di spesa pubblica diretta
                      datiIn[1,13]=gpubvalue 
                      for(sos in 1:Ngpub){
                        proggpub=0 
                        progval[13]=proggpub
                        gpubvalue=gpubvalue+proggpub  
                        
                        parmatchingconsvalue=10  #numero di imprese visionate da ogni consumatore, per secegliere da chi comprare
                        datiIn[1,14]=parmatchingconsvalue
                        for(eia in 1:Nparcons){
                          progparcons=0
                          progval[14]=progparcons
                          parmatchingconsvalue=parmatchingconsvalue+progparcons  
                          
                          swicvalue=0.91#elasticità rispetto ai differenziali di prezzo 
                          datiIn[1,15]=swicvalue
                          for(eiu in 1:Nswic){
                            progswic=0
                            progval[15]=progswic
                            swicvalue=swicvalue+progswic          
                            
                            uminvalue=0.45 #0.45  # percentuale del grado di utilizzo normale, utilizzata per decidere se cambiare markup o meno
                            datiIn[1,16]=uminvalue 
                            for(kok in 1:Numin){
                              progumin=0
                              progval[16]=progumin
                              uminvalue=uminvalue+progumin
                              
                              NF=102#100 #2 #100 #695 # 350 #120 #-10
                              for(dop in 1:NNF){
                                progNF=0  #15
                                progval[17]=progNF
                                NF=NF+progNF
                                
                                hmese=0.6
                                for(uui in 1:nhmese){
                                  proghmese=0
                                  progval[18]=proghmese
                                  hmese=hmese+proghmese
                                  
                                  probex=0.5
                                  for(rtu in 1:nprobex){
                                    progprobex=0
                                    progval[19]=progprobex
                                    probex=probex+progprobex  
                                    
                                    xval=1/0.85
                                    for(uio in 1:Nx){
                                      progxval=0
                                      progval[20]=progxval
                                      xval=xval+progxval
                                      parametri[counter,1]=phivalue
                                      parametri[counter,2]=vvalue
                                      parametri[counter,3]=liminfvalue
                                      parametri[counter,4]=limsupvalue
                                      parametri[counter,5]=zvalue
                                      parametri[counter,6]=dkvalue
                                      parametri[counter,7]=percvalue
                                      parametri[counter,8]=qexvalue
                                      parametri[counter,9]=betavalue
                                      parametri[counter,10]=alfawgovvalue
                                      parametri[counter,11]=flexvalue
                                      parametri[counter,12]=intvalue
                                      parametri[counter,13]=gpubvalue
                                      parametri[counter,14]=parmatchingconsvalue
                                      parametri[counter,15]=swicvalue
                                      parametri[counter,16]=uminvalue
                                      parametri[counter,17]=NF
                                      parametri[counter,18]=hmese
                                      parametri[counter,19]=probex
                                      parametri[counter,20]=xval
                                      
                                      counter=counter+1
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }}
        }
      }
    }
  }
}
#parametri[1,17]=2


#ff<-function(trial){


#for(trial in 1:ncomb){
conditionbasket=0
vin="no"   #vincolo di bilancio settore pubblico
T=900
#MONTE CARLO 
result=matrix(data=0,nrow=T,ncol=1)


#NUMBER OF HOUSHOLDS


NF=parametri[trial,17]
#NUMBER OF Consumer FIRMS 
NFC=round(0.72*NF,0)  #0.72
#NUMBER OF CAPITAL GOODS FRIMS
NFK=NF-NFC

perc10=ceiling(NFC*0.1)
perc30=ceiling(NFC*0.2)
perc60=ceiling(NFC*0.6)


espe=0# se lo metto uguale a zero assumo che la quantita che mi attendo per domani è indipendente dal tasso di crescita atteso.. ma viene calcolato solo utilizzando direttametne le quantità registrate nei periodi precedente (non la variazione dei tassi di crescita)
conta=1
xvalue=parametri[trial,20] #produttività del bene capitale (sarebbe: 1/v dell  a tesi)
wg=c(0.1,0.2,0.3,0.4)    #valroi utilizzati per la media ponderata dei gradi di utilizzo passati
epsi=0.4   #parametro utilizzato dal Governo per stimare aspettative su GDP, Tasse ,ecc
sens=0  #0.003  # sensibilità del salario nominale rispetto al rapporto tra disoccupazione effettiva e full employment 
UN=0.9 #percentuale del numero totale dei lavoratori considerata come benchmark di full employment per la variaizone dei salari nominali


ualfa=matrix(data=0, nrow=NF)
alfa=matrix(data=0, nrow=NF)
beta=matrix(data=0, nrow=NF)
if(eterogeneita==1){
  stocasp=abs(rfoldnorm(NF,mean=parametri[trial,9],sd=0.02))
  ualfa=stocasp
  alfa=stocasp
  beta=stocasp
}else{
  ualfa[]=parametri[trial,9]
  alfa[]=parametri[trial,9]
  beta[]=parametri[trial,9]
}

#Taylor Rule (i valori dei parametri sono presi come in Assenza et al. )
inflazionetarget=0.002  #inflazione target del singolo periodo 
inttarget=0.01   #tasso d'interesse target della BC
t1=0.1 # sensibilità rispetto a interesse target
t2=0.9 # sensibilità rispetto al tasso d'interesse realizzato nel periodo precedente 
t3=0.01 #sens rispetto al gap dell'inflazione

vincolo=0.02
phi=parametri[trial,1]
v=parametri[trial,2]
qexogenousc=parametri[trial,8]/NFC


if(NF>2){
  limiteinvdec=parametri[trial,3]
  limiteinvinc=parametri[trial,4]
}else{
  limiteinvdec=3000
  limiteinvinc=3000
}
z=parametri[trial,5] 
dk=parametri[trial,6]
de=dk
N = max(6000,round((qexogenousc*NFC/(xvalue*v)+(((qexogenousc*NFC/xvalue)/z)/dk)/phi)*1.1),3)

flexsigma=parametri[trial,11]
sigmavalue=0.05 #0.05 #0.02 #& scorte desiderate
perc=parametri[trial,7]
cworker=0.75 #0.9 #0.85 #propensione al consumo dei lavoratori
#propensioe al consumo dei capitalisti
ccap=0.65

cupw=cworker
cdownw=cworker

cupcap=ccap
cdowncap=ccap


#propensione al consuo sui risparmi dei lavoratori
cworkercash=0 #0.05 #0.04#0.02    #0.06
#propensioneal consumo sui risparmi dei capitlasti 
ccapcash=0 #0.05  #0.04 #0.02
#produttività del singolo bene capitale
xexogenous=xvalue
#LABOR PRODUCTIVITY uguale per tutte le imprese
#quota di capitale ordinata su quello teoricamente necessario per soddisfare la domanda attesa al livello normale
gamma=1 
#aspettativa adattiva
bas=0.8 #0.7 #0.7 #se è uguale ad uno non considero il paniere s


#RATIO DELLE INVENTORIES
sigma=sigmavalue
# grado di utilizzo normale della capacità produttiva esogeno
u_n=0.8
umin=parametri[trial,16]
# paarmetri per la scelta di portafoglio (titoli pubblici o depositi)
l_o= 0#0.35    #0.15
l_uno=0#0.5    #0.3
l_due=0#0.4
# markup iniziale nei due settori
markupc=0.2
markupk=0.2
#probabilità di cambiare fornitore di beni di consumo-> al diminuire di questo numero la probabilitò aumenta
#swic=parametri[trial,15]
e=2.71828
#durata del bene capitale e dei prestiti long term
#durata short term loand
zs=10
#durata titoli pubblici
tpub=1
#durata prestito overdraft
zp=15

#h lavoro massime che ogni lavoratore può lavorare in ogni periodo 
hmese=parametri[trial,18]
# tasso d'interesse sui prestiti lungo termine
exintrate=parametri[trial,12] #0.0008
# tasso d'interesse sui prestiti breve termine (prestiti per finanziare i salari)
exintrateshort=parametri[trial,12]
# tassi d'interesse sui depositi
exintdep=parametri[trial,12]*0.5
exintpub=parametri[trial,12]
# tasso d'interesse overdraft
exintrateoverdraft=0
# numero massimo di periodi consecutivi dove si è insolventi e si chiede un prestito per rimbordare i prestiti e pagare gli interessi precedenti
limponzi=5
startingponzi=1
#leverage desiderato
lev=0 #se è 0, vuol dire che l'impresa finchè può si autofinanzia tutto il possibile
eta=0  # in realtà non serve 
teta=1 #percentuale distribuita dei profitti realizzati 
percG=perc
gpub=parametri[trial,13]
tauwork=0.2#15 #tassa suiìl reddito dei lavoratori
taucap=0.2 #35 #tassa sul reddito dei  capitalisti

limrestr=1
creditlim=1
tcredito=3000
tcrunch=10
zcredit=5
spamcredit=0
for (i in 1:z){
  spamcredit=spamcredit+i/zcredit
}
intcredit=exintdep
xintcredit=intcredit*spamcredit/(1+intcredit*spamcredit)

# fattore moltiplicativo per il calcolo dell'ammortamento (sarebbe (a) nella tesi)
spam=0
for (i in 1:z){
  spam=spam+i/z
}

# fattore moltiplicativo per calcolo ammortamento comprensivo del debito e oneri finanziari (sarebbe (b) nellta tesi)
spam2=0
g=1
cr=1
for(i in 1:z){
  spam2=spam2+cr/(z*spam)
  g=g+1
  cr=cr+g
}

# fattore moltiplicativo per il prestito a breve periodo
spamshort=0
for (i in 1:zs){
  spamshort=spamshort+i/zs
}

# fattore moltiplicatotivo per overdraft
spamoverdraft=0
for (i in 1:zp){
  spamoverdraft=spamoverdraft+i/zp
}


limlabor=10
interazionilim=150  #numero massimo di volte che il consumatore effettua il processo di scelta dell'impresa quando quella precedentemente scelta non ha scorte a sufficienza per soddisfare il suo consumo desiderato 
parmatchingconsumption=parametri[trial,14]
parmatchingcapital=10  #numero di imprese K visistate dalle imprese C per scgliere il fornitore

if(NF==2){
  parmatchingconsumption=1
  parmatchingcapital=1
}

#salario per unità di lavoro 
wageExogenous=10  # il salario effettivo diventa wageExogenous/hmese
alfawgovvalue=parametri[trial,10]


#NFCmax=round(parametri[ncomb,17]*0.72,0)

#domanda esogna attesa per il primo periodo imprese consumer -> per avere che la quantità aggregata inizialmetne prodotta non varia al variare del numero delle imprese e con essa la domanda dello stato
#domanda esogena attesa primo periodo imprese capital
quin=qexogenousc
# dotazione iniziale di capitale delle imprese C (uguale per tutte, tutte si attendono la stessa quantità nel primo periodo)-> questo valroe vale solo se il periodo di produzioen del bene capitale è uguale ad uno
kin=(quin*(1+sigma))/(xexogenous*u_n)
#pc=wageExogenous/xexogenous*(1/v+(1+markupk)/(u_n*spam*phi))*(1+markupc)

#prezzo inizale dei due beni

pk=wageExogenous/phi*(1+markupk)
pc=wageExogenous/xexogenous*((1+exintrateshort*spamshort*zs)/v+(1+markupk)*(1+exintrate*spam2)/(u_n*spam*phi))*(1+markupc) #questo e' il prezzo in caso di tasso di deprezzamento decrescente (deprezzamento assoluto a paritá di capitale costante).. comq va aggiunto il tasso d interesse
unitcostc=wageExogenous/xexogenous*((1+exintrateshort*spamshort*zs)/v+(1+markupk)*(1+exintrate*spam2)/(u_n*spam*phi))
unitcostk=wageExogenous/phi


#determinazione swic
probex=parametri[trial,19]
mediaran=1.0000001
sdran=0.000005
pop=matrix(data=0, nrow=100)
for(i in 1:100){
  pop[i]=abs(rfoldnorm(1,mean=mediaran,sd=sdran)-1)
}
val=mean(pop)

mmin=markupc*(1-val)
swic=log(probex)*unitcostc*(1+mmin)/(unitcostc*(1+mmin)-pc)
mmin=markupk*(1-val)
swicK=log(probex)*unitcostk*(1+mmin)/(unitcostk*(1+mmin)-pk)




# livello della spesa iniziale del governo
Gin=percG*qexogenousc*NFC*pc

# contatore dei fallimenti
nbancarottac=0
nbancarottak=0

#delta=0.05
#MARKUP MINMO
markupmin=0.01
# numero di imprese scelte casualmente per fare il matching

#codice settore
cod=matrix(data=0, ncol=NF)
for(i in 1:NF){
  if(i<=NFK){
    cod[i]=0
  }else{
    cod[i]=1
  }}
#numero capitalisti
Ncap=NF
#numero lavoratori
Nwork=N-Ncap
#codice classe: lavoratore o capitalista
codclass=matrix(data=0, ncol=N)
for(i in 1:N){
  if(i<=Nwork){
    codclass[i]=0
  }else{
    codclass[i]=1
  }}
#assegnazione capitalisti a impresa:questo vale se Ncap è maggiore di NF (in caso contrario bisogna fare formula inversa: assegnare i capitalisti alle imrpese)
#distribuzione uniform
capitalistindexF=matrix(data=0, ncol=N)
capperfirm=Ncap/NF
contatore=0
firm=1
for(i in (Nwork+1):N){
  contatore=contatore+1
  capitalistindexF[i]=firm
  if(contatore==capperfirm){
    firm=firm+1
    contatore=0
  }
  
}




#WAGES

#tasso d interesse esogeno

#DATAFRAME WHERE STORING MC SYNTHETIC RESULTS
MC=1
#THE MONTE CARLO LOOP
for (mc in 1:MC) {
  #SET THE SEED
  mc=12
  set.seed(mc)
  
  
  if(TRUE){
    #DEFINE THE DATAFRAME OF RESULTS
    res_macro = as.data.frame(matrix(0,ncol=49,nrow=T+1,dimnames = list(NULL,c("Y","Realconsumptiondemand","Consumptiondemand","Residualconsumptiondemand","Invtot","InvFC","Demandcapital","Nominaldemandcapital","RevenuesK","RevenuesC","RevenuesTot","Currentcosttot","CurrentcostC","CurrentcostK","Profittot","ProfitC","ProfitK","Dividendtot","DividendC","DividendK","WB","Yhworkers","YHcapitalist","CashFC","CashFK","CashH","Kinstalledintheperiod","kdemanded","k","Laborrequired","domandalavoro","Employed","Stockdebt","Loandemand","Debtservice","Ponzi","CashF","wageshare","profitshare","NCC","NCCF","NCCI","priceindC","priceindK","markupindC","markupindK","realconsumptiongrowthrate","capacitygrowthrate","ratiocapconsgrowthrate"))))
    
    
    realvariables=c("Iinstalled","Kd_t_dk","K","y_dc","yc","kd","invc","realsalisc","realconsumptiondemand","y_dk","yk","ynewk","invk","realsailsk","lrequired","employees")
    RS<-as.data.frame(matrix(0,ncol=length(realvariables),nrow=T))
    colnames(RS)=realvariables
    
    nominalvariables=c("kdnominal","WBC","revenuesc","debtserviceC","StockdebtFC","loandemandC","overdraftdemandC","cashC","profitC","divC","revenuesk","WBK","loandemandk","cashK","debtserviceK","StockdebtFK","profitK","divK","WB","yhw","nominalconsdemandw","nomianlconsxpw","yhcapitalisti","nominalconsdemandCap","nomconsexpcap","cashHw","cashHcap","totrealconsumption","taxpaymentw","taxpaymentcap","Tax")
    NS<-as.data.frame(matrix(0,ncol=length(nominalvariables),nrow=(T+1)))     
    colnames(NS)=nominalvariables
    #ciao=as.data.frame(tabella_micro)
    
    
    namesSectors=c("WORKERS","CAPITALISTS","FIRMSK","FIRMSC","BANKS","GOVERNMENT","CB","SUM")
    namesStocks=c("Deposits","Advances","Loans","Credit_Cons","Nonperforming_Cons","Inventories","Fixed Capital","Net Worth","Bond","HPM","SUM")
    data<-data.frame(matrix(0,ncol=length(namesSectors),nrow=length(namesStocks)))
    BS<-array(data=0,dim=c(length(namesStocks),length(namesSectors),T),dimnames=list(namesStocks,namesSectors))
    colnames(BS)=namesSectors
    
    namesSectors=c("WORKERS","CAPITALISTS","FIRMSK","FIRMSC","BANKS","GOVERNMENT","CB","SUM")
    namesAssets=c("Consumption","Investments","GovExp","UnBenefit","Tax","Wages","Profits","Recapital","IntOnLoans&ov","IntCreditConsumption","InterestDeposit","InterestHb","InterestAdv","ProfitCB","InterestBond","deltadepositi","deltaloan","deltacreditCons","deltaBond","deltanonperforming","deltaAdvances","deltaHb","SUM")
    data<-data.frame(matrix(0,ncol=length(namesSectors),nrow=length(namesAssets)))
    TM<-array(data=0,dim=c(length(namesAssets),length(namesSectors),T),dimnames=list(namesAssets,namesSectors))
    colnames(TM)=namesSectors
    rownames(TM)=namesAssets
    
    
    Cweight=matrix(data=0, nrow=T)
    Investmentwight=matrix(data=0, nrow=T)
    investmentwight2=matrix(data=0, nrow=T)
    Gweigth=matrix(data=0, nrow=T)
    totrealconsdemand=matrix(data=0, nrow=T)
    totmissing=matrix(data=0, nrow=T)
    meanratio=matrix(data=0, nrow=T)
    Greal=matrix(data=0, nrow=T)
    Greal1=matrix(data=0, nrow=T)
    unsatisfieddemand=matrix(data=0, nrow=T)
    ratio=matrix(data=0, nrow=T, ncol=NF)
    percY=matrix(data=0, nrow=T, ncol=N)
    percV=matrix(data=0, nrow=T, ncol=N)
    Vreal=matrix(data=0, nrow=T, ncol=N)
    Gweigth=matrix(data=0, nrow=T)
    perc_Ub=matrix(data=0, nrow=T)
    perc_G=matrix(data=0, nrow=T)
    Gtot=matrix(data=0, nrow=T)
    SpesaconsentitaGU=matrix(data=0, nrow=T)
    Deficitconsentito=matrix(data=0, nrow=T)
    DeficitProg=matrix(data=0, nrow=T)
    control=matrix(data=0, nrow=T)
    AdvB=matrix(data=0, nrow=T)
    deltaRes=matrix(data=0, nrow=T)
    #calcolo rembursement credit
    deltanonperformingcredit=matrix(data=0, nrow=T)
    nintcredit=matrix(data=0, ncol=N)
    creditconsumptionB=matrix(data=0, nrow=T)
    Bankcreditinterest=matrix(data=0, nrow=T)
    nonperformingloanbank=matrix(data=0, nrow=T)
    intratecredit=matrix(data=intcredit, nrow=T)
    ratacredit=matrix(data=0, nrow=T+zcredit, ncol=N)
    ratacredittot=matrix(data=0, nrow=T+zcredit, ncol=N)
    hhdebt=matrix(data=0, nrow=T, ncol=N)
    capitalhh=matrix(data=0, nrow=T, ncol=N)
    effectivepaymentB=matrix(data=0, nrow=T, ncol=N)
    creditinterestpayment=matrix(data=0, nrow=T, ncol=N)
    crunch=matrix(data=0, nrow=T, ncol=N)
    restr=matrix(data=0,ncol=N)
    clim=matrix(data=0, ncol=N)
    inthh=matrix(data=0, nrow=T, ncol=N)
    creditdemand=matrix(data=0, nrow=T, ncol=N)
    newcreditdemandshock=matrix(data=0, nrow=T, ncol=N)
    newcreditdemand=matrix(data=0, nrow=T, ncol=N)
    nonperformingloanhh=matrix(data=0, nrow=T, ncol=N)
    deltaD=matrix(data=0, nrow=T)
    cashB=matrix(data=0, nrow=T, ncol=NF)
    uefuturo=matrix(data=0, nrow=T, ncol=NF)
    ue_pesati_aggregatebigs=matrix(data=0, nrow=T, ncol=NF)
    uefuturo_aggregatebigs=matrix(data=0, nrow=T)
    realconsumptionmissing=matrix(data=0, nrow=T, ncol=N)
    u_med=matrix(data=0,nrow=T,ncol=NF)
    totrealconsumption_des=matrix(data=0, nrow=T, ncol=N)
    ccappost=matrix(data=0,nrow=T)
    cwpost=matrix(data=0,nrow=T)
    consumptionemployed=matrix(data=0,nrow=T)
    yhdemployed=matrix(data=0,nrow=T)
    pastbaskettotworkers=matrix(data=0,nrow=T)
    pastbaskettotcapitalists=matrix(data=0,nrow=T)
    def_yc=matrix(data=0,nrow=T)
    #deb_yc=matrix(data=0,nrow=T)
    gex=matrix(data=0,nrow=T,ncol=NF)
    gr=matrix(data=0,nrow=T,ncol=NF)
    u_medio=matrix(data=0, nrow=T, ncol=NF)
    g_yc=matrix(data=0,nrow=T)
    realvalueyhd=matrix(data=0, nrow=T, ncol=N)
    pastbasket=matrix(data=0, nrow=T, ncol=N)
    invtotC=matrix(data=0,nrow=T)
    privatedebt_pil=matrix(data=0,nrow=T)
    percbondcap=matrix(data=0,nrow=T)
    percbondCB=matrix(data=0,nrow=T)
    TotdebtF=matrix(data=0,nrow=T)
    alfak=matrix(data=0, nrow=T, ncol=NF)
    #INITIAL CONDITIONS
    invlimsup=matrix(data=0, nrow=T, ncol=NF)
    invliminf=matrix(data=0, nrow=T, ncol=NF)
    #MARKUP
    markup=matrix(data = 0, nrow=T, ncol = NF)
    #wages
    w = matrix(data = 0, ncol = Nwork)
    wage=matrix(data=0,nrow=T,ncol=Nwork)
    #costi unitari senza overhead?
    #costi unitari con hoverhead (calcolato al pieno utilizzo del singolo macchinario--> poi posso calcolarlo anche in base al grado di utilizzo normale)
    #come calcolo il costo medio del capitale dal momento che ogni volta che ne compro di nuovo il prezzo è diverso?
    inter=matrix(data=0, nrow=T, ncol=N)
    GDPreal=matrix(data=0, nrow=T)
    #yc=matrix(data=0, nrow=T)
    u_raggregate=matrix(data=0, nrow=T)
    u_raggregatebigs=matrix(data=0, nrow=T)
    u_raggregatesmall=matrix(data=0, nrow=T)
    u_raggregatemedium=matrix(data=0, nrow=T)
    
    unitcost=matrix(data=0, nrow=T, ncol=NF)
    p=matrix(data=0, nrow=T, ncol=NF)
    pfloor=matrix(data=0, nrow=T, ncol=NF)
    pcup=matrix(data=0, nrow=T, ncol=NF)
    k_used=matrix(data=0, nrow=T, ncol=NF)
    #interestrate
    intratedeposit=matrix(data=exintdep, nrow=T)
    intrate=matrix(data=exintrate, nrow=T)
    intrateshort=matrix(data=exintrateshort, nrow=T)
    intrateoverdraft=matrix(data=exintrateoverdraft, nrow=T)
    #produttività della singola unità di capitale 
    x=matrix(data=0, ncol= NF)
    #dotazione di capiale delle imprese consumer
    k=matrix(data=0,nrow=T,ncol=NF)
    #capitale utilizzato nei periodo passati, media ponderata mobile
    k_sign=matrix(data=0,nrow=T,ncol=NF)
    #domanda attesa
    q_e=matrix(data=0, nrow=T, ncol=NF)
    #quantità desiderata in corrispondenza del grado di utilizzo normDDDale
    q_dn=matrix(data=0, nrow=T,ncol=NF)
    #produzione desiderata
    y_d=matrix(data=0,nrow=T, ncol=NF)
    #produzione in corrsipondenza del grado di utilizzo normale
    y_n=matrix(data=0, nrow=T, ncol=NF)
    #numero di lavoratori necessari in corrsipondenza del grado di utilizzo normale
    l_n=matrix(data=0,nrow=T, ncol=NF)
    #grado di utilizzo vincolato dalla manczanza di forza lavoro (non succede mai, ma bisogna mettercelo)
    u_l=matrix(data=0, nrow=T, ncol=NF)
    #grado di utilizzo realizzato
    u_r=matrix(data=0,nrow=T, ncol=NF)
    #produzione (potrebbero esserci dei colli di bottiglia: lavoro o capitale)
    y=matrix(data=0,nrow=T,ncol=NF)
    
    realyhworkers=matrix(data=0,nrow=T)
    realyhcapitalist=matrix(data=0,nrow=T)
    
    profitshare=matrix(data=0, nrow=T)
    wageshare=matrix(data=0, nrow=T)
    consumptioncapitalist=matrix(data=0, nrow=T)
    consumptionworkers=matrix(data=0, nrow=T)
    
    nint=matrix(data=0,ncol=N)
    deltareal=matrix(data=0, nrow=T,ncol=N)
    #capitale desiderato totale
    k_de=matrix(data=0,nrow=T,ncol=NF)
    #domanda di capitali, ossia di investimenti
    kd=matrix(data=0, nrow=T, ncol=NF)
    #
    kdfeasible=matrix(data=0, nrow=T, ncol=NF)
    #
    kdfeasiblenominal=matrix(data=0, nrow=T, ncol=NF)
    #labor necessario
    lrequired=matrix(data=0,nrow=T, ncol=NF)
    
    lhrequired=matrix(data=0,nrow=T, ncol=NF)
    
    lperwork=matrix(data=0,nrow=T, ncol=NF)
    #labor demand
    labordemand=matrix(data=0,nrow=T,ncol=NF)
    #
    labordemandproblem=matrix(data=0, ncol=NF)
    #
    consumptiondemand=matrix(data=0, ncol=N)
    #
    realconsumptiondemand=matrix(data=0, nrow=T,ncol =N)
    #
    nominalconsumptiondemand=matrix(data=0, nrow=T,ncol =N)
    nominalconsumptionexpenditure=matrix(data=0, nrow=T, ncol=N)
    #
    residualconsumpiondemand=matrix(data=0, ncol=N)
    #
    residualnominalconsumptiondemand=matrix(data=0, ncol=N)
    bancarotta=matrix(data=0,nrow=T, ncol=NF)
    
    nominalresidualconsumptiondemand=matrix(data=0, ncol=N)
    #nominalconsumptiondemand=matrix(data=0, ncol=N)
    
    #
    residualcapitaldemand=matrix(data=0,ncol=NF)
    indexC=matrix(data=NA,nrow=T, ncol=N)
    controllo=matrix(data=0, nrow=T)
    
    #domanda di beni capitali per ogni impresa che li producie
    demandperfirmcapital=matrix(data=0,nrow=T, ncol=NF)
    #domanda di beni per imprese consumer
    demandperfirmconsumer=matrix(data=0, nrow=T, ncol=NF)
    demandperfirmconsumer_sign=matrix(data=0, nrow=T, ncol=NF)
    #revenues
    revenues=matrix(data=0,nrow=T, ncol=NF)
    # gradi di utilizzo atteso in base alla proframmazione fissata
    ue=matrix(data=0, nrow=T,ncol=NF)
    #cinvolo capacità massima: grado di utizzo minimo tra il massimo e quello derivante dalla programmazione
    ue_f=matrix(data=0,nrow=T, ncol=NF)
    #vendite reali
    realsails=matrix(data=0,nrow=T, ncol=NF)
    #costi correnti
    currentcost=matrix(data=0, ncol=NF)
    #verifica supply
    esaurimento=matrix(data=2, ncol=NF)
    esaurimentoK=matrix(data=2, ncol=NF)
    situaindexF=matrix(data=0, ncol=N)
    situaindexFK=matrix(data=2, ncol=NF)
    Iinstalled=matrix(data=0, nrow=T, ncol=NF)
    Iinstallednominal=matrix(data=0, nrow=T,ncol=NF)
    indexCapital=matrix(data=0, ncol=NF)
    ponzi=matrix(data=0,nrow=T, ncol=NF)
    indexFK=matrix(data=NA, nrow=T, ncol=NF)
    interestdepositpayB=matrix(data=0, nrow=T)
    
    #storicoordini=matrix(data=0, nrow=T+dk-1, ncol=NF)
    storicoprezzo=matrix(data=0, nrow=T+dk-1, ncol=NF)
    #kstoricoordinatonominal=matrix(data=0, nrow=T+dk-1, ncol=NF)
    #kstoricoordinato=matrix(data=0, nrow=T+dk-1, ncol=NF)
    #ordinitot=matrix(data=0,nrow=T,ncol=NF)
    kfutinstorico=matrix(data=0, nrow=T+dk+z,ncol=NF)
    
    y_old=matrix(data=0, nrow=T, ncol=NF)
    y_new=matrix(data=0, nrow=T, ncol=NF)
    kdordinatofeasible=matrix(data=0, nrow=T, ncol=NF)
    kdordinatofeasiblestorico=matrix(data=0, nrow=T+dk, ncol=NF)
    kdordinatofeasiblenominalstorico=matrix(data=0, nrow=T+dk, ncol=NF)
    storicoordinifeasible=matrix(data=0, nrow=T+dk-1, ncol=NF)
    sails=matrix(data=0, nrow=T, ncol=NF)
    capfuturo=matrix(data=0,nrow=T,ncol=NF)
    nponzi=matrix(data=0, ncol=NF)
    ammresiduo=matrix(data=0, nrow=T, ncol=NF)
    Residualvalue=matrix(data=0, nrow=T, ncol=NF)
    NW=matrix(data=0, nrow=T, ncol=NF)
    #FIRM'S INVENTORIES
    inv=matrix(data = 0, nrow=T,ncol=NF)
    invt=matrix(data = 0, nrow=T,ncol=NF)
    valueinv=matrix(data = 0, nrow=T,ncol=NF)
    #HOUSHOLDS' CASH
    cashH = matrix(data = 0, ncol = N)
    #FIRMS' CASH
    cashF= matrix(data = 0, ncol = NF)
    #HOUSEHOLDS EMPLOYMENT CONDITION: 1=UNEMPLOYED 0=EMPLOYED
    unemp = matrix(data = 1, ncol = Nwork)
    #FIRMS' EMPLOYEES NUMBER
    employees = matrix(data = 0,nrow=T, ncol = NF)
    
    #HOUSHOLDS' EMPLOYER INDEXES
    employer = matrix (data = 0, ncol = N)
    #FIRMS' WAGE BILL
    wb = matrix(data = 0,nrow=T, ncol = NF)
    
    #HOUSHOLDS' GROSS INCOME
    #yH = matrix(data = 0, nrow=T, ncol = N)
    # FIRMS LOAND DEMAND
    servicedebtFtot=matrix(data=0, nrow=T, ncol=NF)
    interestpaymentF=matrix(data=0, nrow=T, ncol=NF)
    profitB=matrix(data=0, nrow=T)
    divB=matrix(data=0, nrow=T)
    intratestorico=matrix(data=exintrate, nrow=z+T+1)
    storicorata=matrix(data=0, nrow=z+T+1, ncol=NF)
    loandemandF=matrix(data = 0,nrow=T+1, ncol=NF)
    ponzis=matrix(data=0, nrow=T, ncol=NF)
    stockdebtF=matrix(data=0,nrow=T, ncol=NF)
    servicedebtF=matrix(data=0,nrow=T, ncol=NF)
    div=matrix(data=0, nrow=T,ncol=NF)
    profit=matrix(data=0,nrow=T, ncol=NF)
    profit_fe=matrix(data=0,nrow=T, ncol=NF)
    loanstoricoF=matrix(data=0, nrow=z+T+dk, ncol=NF)
    storicoI=matrix(data=0, nrow=z+T, ncol=NF)
    storicoIN=matrix(data=0, nrow=z+T, ncol=NF)
    ammortamento=matrix(data=0,nrow=T, ncol=NF)
    storicoammortamentinominal=matrix(data=0, nrow=2*z+T, ncol=NF)
    #prezziario storico dei capitali installati -> serve per il calcolo dell'ammortamento
    pstorico=matrix(data=0, nrow=T+z, ncol=NF)
    #dividendi distribuiti da ogni impresa su ogni singolo capitalista
    divpercap=matrix(data=0, ncol=NF)
    divpercapB=matrix(data=0, nrow=T)
    #STOCKLOAN FIRMS'
    #stockdebtF = matrix(data = 0, ncol = NF)
    #market share
    marketshare=matrix(data=0, nrow=T,ncol=NF)
    ratiomarketshare=matrix(data=0, nrow=T, ncol=NF)
    indexF=matrix(data=NA, ncol=N)
    unitcostlabor=matrix(data=0, nrow = T, ncol=NF)
    priceweightedC=matrix(data=0, ncol=NF)
    priceweightedK=matrix(data=0, ncol=NF)
    markupweightedC=matrix(data=0, ncol=NF)
    markupweightedK=matrix(data=0, ncol=NF)
    
    interestdepositworker=matrix(data=0, nrow=T)
    capacitygrowthrate=matrix(data=0, nrow=T)
    realconsumptiongrowthrate=matrix(data=0, nrow=T)
    ratiocapconsgrowthrate=matrix(data=0, nrow=T)
    totrealconsumption=matrix(data=0, nrow=T,ncol=N)
    totrealconsumption_sign=matrix(data=0, nrow=T,ncol=N)
    G=matrix(data=0, nrow=T)
    C=matrix(data=0, nrow=T)
    Gr=matrix(data=0, nrow=T)
    Ubenefit=matrix(data=0, nrow=T)
    Ubenefitantes=matrix(data=0, nrow=T)
    Tax=matrix(data=0, nrow=T)
    bond=matrix(data=0, nrow=T)
    pubdebt=matrix(data=0, nrow=T)
    debpil=matrix(data=0, nrow=T)
    debpilreal=matrix(data=0, nrow=T)
    deficitG=matrix(data=0, nrow=T)
    Def_Pil2=matrix(data=0, nrow=T)
    Def_Pil=matrix(data=0, nrow=T)
    
    storicoratashort=matrix(data=0, nrow=T+zs+1,ncol=NF)
    storicoshortloandemandF=matrix(data=0, nrow=T+zs+1,ncol=NF)
    interestBond_cap=matrix(data=0, nrow=T)
    pubintratestorico=matrix(data=0, nrow=tpub+T+1)
    taxpayment=matrix(data=0, nrow=T, ncol=N)
    VG=matrix(data=0, nrow=T)
    dissavingcap=matrix(data=0, nrow=T, ncol=N)
    Gef=matrix(data=0,nrow=T)
    Gcons=matrix(data=0,nrow=T)
    Gfinito=matrix(data=0, nrow=T)
    #GDPrealC=matrix(data=0, nrow=T)
    limalzare=matrix(data=0,nrow=T, ncol=NF)
    limabbassare=matrix(data=0,nrow=T, ncol=NF)
    riservedes=matrix(data=0,nrow=T, ncol=NF)
    risreali=matrix(data=0,nrow=T, ncol=NF)
    cashavailablefunding=matrix(data=0,nrow=T, ncol=NF)
    loandes=matrix(data=0,nrow=T, ncol=NF)
    StockLoanB=matrix(data=0,nrow=T)
    LoanBDemand=matrix(data=0,nrow=T)
    CFB=matrix(data=0,nrow=T)
    Advances=matrix(data=0,nrow=T)
    Advances2=matrix(data=0,nrow=T)
    deltaDeposit=matrix(data=0,nrow=T)
    deltaDeposit2=matrix(data=0,nrow=T)
    Deposit=matrix(data=0,nrow=T)
    overdraftdemand=matrix(data=0,nrow=T, ncol=NF)    
    storicooverdtraft=matrix(data=0,nrow=T+zp+1, ncol=NF)
    storicorataoverdraft=matrix(data=0,nrow=T+zp+1, ncol=NF)
    storicobondoverdraft=matrix(data=0, nrow=T+tpub)
    bondoverdraft=matrix(data=0,nrow=T)
    NWB=matrix(data=0, nrow=T)
    ChG=matrix(data=0, nrow=T)
    stockdebtG=matrix(data=0, nrow=T)
    HPM=matrix(data=0, nrow=T)
    deltaAssetatCB=matrix(data=0, nrow=T)
    servicericapital=matrix(data=0,nrow=T, ncol=NF) 
    #THE SIMULATION LOOP
    restituzioneparziale=matrix(data=0,nrow=T, ncol=NF) 
    PerditaB=matrix(data=0, nrow=T)
    deltaAdvances=matrix(data=0, nrow=T)
    NWG=matrix(data=0,nrow=T)
    NWCB=matrix(data=0, nrow=T)
    ReservesCB=matrix(data=0, nrow=T)
    
    leveragew=matrix(data=0, nrow=T,ncol=NF)
    leverage=matrix(data=0, nrow=T,ncol=NF)
    
    
    loanstoricoFtot=matrix(data=0, nrow=T+z+dk, ncol=NF)
    storicoINfull=matrix(data=0, nrow=z+T, ncol=NF)
    ammortamentofull=matrix(data=0, nrow=T, ncol=NF)
    Iinstallednominalfull=matrix(data=0, nrow=T, ncol=NF)
    kdordinatofeasiblenominalstoricofull=matrix(data=0, nrow=T+dk, ncol=NF)
    shortloandemandF=matrix(data=0, nrow=T, ncol=NF)
    y_cap=matrix(data=0, nrow=T)
    y_work=matrix(data=0, nrow=T)
    y_hw=matrix(data=0, nrow=T)
    y_hcap=matrix(data=0, nrow=T)
    C_w=matrix(data=0, nrow=T)
    C_cap=matrix(data=0, nrow=T)
    Moneydeposit_worker=matrix(data=0, nrow=T)
    Moneydeposit_cap=matrix(data=0, nrow=T)
    V_cap=matrix(data=0, nrow=T)
    V_worker=matrix(data=0, nrow=T)
    Liquidityend=matrix(data=0, nrow=T, ncol=N)
    #Liquidityend_worker=matrix(data=0, nrow=T)
    qd=matrix(data=0, nrow=T)
    realconsumptiondemandwork=matrix(data=0, nrow=T)
    realconsumptiondemandcap=matrix(data=0, nrow=T)
    Cef_w=matrix(data=0, nrow=T)
    Cef_cap=matrix(data=0, nrow=T)
    
    bondinterest=matrix(data=0, nrow=T)
    BondDemand=matrix(data=0, nrow=T, ncol=N)
    BondDemand_cap=matrix(data=0, nrow=T)
    BondRepayed=matrix(data=0, nrow=T, ncol=N)
    bondrepayment=matrix(data=0, nrow=T)
    bond=matrix(data=0, nrow=T)
    bondstorico=matrix(data=0, nrow=T+tpub)
    bondscadenza=matrix(data=0, nrow=T+tpub)
    interestpaymentG=matrix(data=0, nrow=T)
    ProfitCB=matrix(data=0, nrow=T)
    saldoprimario=matrix(data=0, nrow=T)
    saldosecondario=matrix(data=0, nrow=T)
    interestBond_CB=matrix(data=0, nrow=T)
    advanceinterest=matrix(data=0, nrow=T)
    
    dinero=matrix(data=0, nrow=T,ncol=N)
    redundant=matrix(data=0, nrow=T)
    NWCB=matrix(data=0, nrow=T)
    NWG=matrix(data=0, nrow=T)
    
    NW_cap=matrix(data=0, nrow=T)
    NW_workers=matrix(data=0, nrow=T)
    deltaH=matrix(data=0, nrow=T)
    BondCB=matrix(data=0, nrow=T)
    BuyBond_CB=matrix(data=0, nrow=T)
    BondRepayed_CB=matrix(data=0, nrow=T)
    bondscadenzaCB=matrix(data=0, nrow=T+tpub)
    Hb=matrix(data=0, nrow=T)
    Hb2=matrix(data=0, nrow=T)
    Deposit2=matrix(data=0, nrow=T)
    cashHcapmoment=matrix(data=0, nrow=T)
    cashHwmoment=matrix(data=0, nrow=T)
    cashG=matrix(data=0, nrow=T)
    shortloandemandF=matrix(data=0, nrow=T, ncol=NF)
    taxpaymentcap=matrix(data=0, nrow=T)
    taxpaymentworker=matrix(data=0, nrow=T)
    moneydeposit=matrix(data=0, nrow=T, ncol=N)
    V=matrix(data=0, nrow=T, ncol=N)
    yh=matrix(data=0, nrow=T, ncol=N)
    yhd=matrix(data=0, nrow=T, ncol=N)
    bondtoprivate=matrix(data=0, nrow=T)
    
    storicovaluesails=matrix(data=0, nrow=T+dk, ncol=NF)
    contrevenues=matrix(data=0, nrow=T, ncol=NF)
    contsails=matrix(data=0, nrow=T, ncol=NF)
    storicosales=matrix(data=0, nrow=T+dk, ncol=NF)
    
    lwork=matrix(data=0,nrow=T,ncol=Nwork)
    
    priceindexC=matrix(data=0, nrow=T)
    markupindexC=matrix(data=0, nrow=T)
    markupindexK=matrix(data=0, nrow=T)
    priceindexK=matrix(data=0, nrow=T)
    
    GDP=matrix(data=0,nrow=T)
    InvestmentR=matrix(data=0,nrow=T)
    #Consumption=matrix(data=0,nrow=T)
    Unemployment=matrix(data=0,nrow=T)
    Employeestot=matrix(data=0,nrow=T)
    Investmentvalue=matrix(data=0,nrow=T)
    Consumptionhh=matrix(data=0,nrow=T)
    prov=matrix(data=0,nrow=dk,ncol=NF)
    provfull=matrix(data=0,nrow=dk,ncol=NF)
    #DOTAZIONI INIZIALI
    storicofuturo=matrix(data=0, nrow=dk+z)
    Installare=matrix(data=0, nrow=dk)
    Installarenominale=matrix(data=0, nrow=dk)
    
    y_valorek=matrix(data=0, nrow=T, ncol=NF)
    y_oldvalore=matrix(data=0, nrow=T, ncol=NF)
    
    Nonemp=matrix(data=0, nrow=T)
    U=matrix(data=0, nrow=T)
    Une=matrix(data=0, nrow=T)
    wageaggregate=matrix(data=0, nrow=T)
    wgov=matrix(data=0, nrow=T)
    inflazione=matrix(data=0, nrow=T)
    
    gexGDP=matrix(data=0, nrow=T)
    gexTax=matrix(data=0, nrow=T)
    gexU=matrix(data=0, nrow=T)
    g_GDP=matrix(data=0, nrow=T)
    g_GDPreal=matrix(data=0, nrow=T)
    g_Tax=matrix(data=0, nrow=T)
    g_U=matrix(data=0, nrow=T)
    controllo2=matrix(data=0, nrow=T)
    produzionek=matrix(data=0, nrow=T)
  }
  #inzializzazione della quantità di capitale installata in ogni impresa per i primi dk periodi (dk è il numero di periodi necessari per produrre un bene capitale)
  for(i in 1:dk){
    if(i==1){
      Installare[i]=kin
      storicofuturo[i+z]=kin
    }else{
      kresiduo=0
      s=1/z
      for(j in (i+1):(i+z-1)){
        kresiduo=kresiduo+storicofuturo[j]*s
        s=s+1/z
      }
      
      Installare[i]=kin-kresiduo
      storicofuturo[i+z]=Installare[i]
    }      
    Installarenominale[i]=Installare[i]*pk  #non ci metto gli interessi perche non mi sono indebitato per questo capitale, é una dotazione iniziale
  }
  
  
  for(i in 1:dk){
    for(j in (NFK+1):NF){
      Iinstalled[i,j]=Installare[i]
      Iinstallednominal[i,j]=Installarenominale[i]
      Iinstallednominalfull[i,j]=Installarenominale[i]*(1+spam2*exintrate)
      kfutinstorico[i+z,j]=Installare[i]
    }
  }
  
  
  # Start #####
  for (t in 1:T){
    #t=
    nint[]=0
    priceweightedC[]=0
    markupweightedC[]=0
    markupweightedK[]=0
    priceweightedK[]=0
    nintcredit[]=0
    #totrealconsumption[]=0
    #w[]=wageExogenous
    x[]=xexogenous
    # y[]=0
    #  y_d[]=0
    # realsails[]=0
    # servicedebtF[]=0
    # revenues[]=0
    consumptiondemand[]=0
    # realconsumptiondemand[]=0
    residualconsumpiondemand[]=0
    residualcapitaldemand[]=0
    #  lrequired[]=0
    #  labordemand[]=0
    #demandperfirmcapital[]=0
    esaurimentoK[]=2
    esaurimento[]=2
    # k_de[]=0
    # profit[]=0
    #  profit_fe[]=0
    currentcost[]=0
    indexF[]=NA
    #  wb[]=0
    #  div[]=0
    divpercap[]=0
    situaindexF[]=0
    situaindexFK[]=2
    #nominalconsumptionexpenditure[]=0
    residualnominalconsumptiondemand[]=0
    #   ammortamento[]=0
    nominalresidualconsumptiondemand[]=0
    #realconsumptiondemand[]=0
    #nominalconsumptiondemand[]=0
    #indexC[]=NA
    #indexFK[]=NA
    
    #dinero[]=0
    
    #Taylor rule
    # if(t>20){
    # intrate[t]=t1*inttarget+t2*intrate[t-1]+t3*(inflazione[t-1]-inflazionetarget)
    # intrateshort[t]=intrate[t]
    # }
    
    
    # fissazione del salari nominale aggregato 
    if(t>1){
      wageaggregate[t]=wageaggregate[t-1]*(1+sens*(U[t-1]/UN))
      w[]=wageaggregate[t]
    }else{
      w[]=wageExogenous
      wageaggregate[t]=wageExogenous
    }
    
    
    #sussidio pagato dal governo
    wgov[t]=wageaggregate[t]*alfawgovvalue*hmese
    
    
    # Interessi pagati sul debito pubblico
    bondrepayment[t]=bondscadenza[t]
    for(j in t:(t+tpub-1)){
      if(bondstorico[j]>0){
        interestpaymentG[t]=interestpaymentG[t]+bondstorico[j]*pubintratestorico[j]
      }
    }
    
    
    # Profitti della BC
    if(t>1){
      interestBond_CB[t]=BondCB[t-1]*bondinterest[t-1] ##cosi solo perche ho messo che dura un periodo il bond
      ProfitCB[t]=interestBond_CB[t]+Advances[t-1]*advanceinterest[t-1]-Hb2[t-1]*intratedeposit[t-1]
    }
    if(round(ProfitCB[t],4)<0){
      #print("problemProfitCB")
      #print(t)
    }
    
    
    # Aspettativa adattiva del Governo, per il calcolo della spesa in caso di vincolo di bilancio da rispettare
    if(t>4){
      gexGDP[t]=gexGDP[t-2]+epsi*(g_GDP[t-1]-gexGDP[t-2])
      gexTax[t]=gexTax[t-2]+epsi*(g_Tax[t-1]-gexTax[t-2])
      gexU[t]=gexU[t-2]+epsi*(g_U[t-1]-gexU[t-2])
    }else if(t==3 | t==4){
      
      gexGDP[t]=g_GDP[t-1]
      gexTax[t]=g_Tax[t-1]
      gexU[t]=g_U[t-1]
    } 
    
    
    
    #Fissazione spesa del settore pubblico
    
    
    #bondstorico[t+tpub]=bond[t]
    #bondscadenza[t+tpub-1]= bond[t] 
    
    #tasso d'interesse fisso sui titoli pubblici
    bondinterest[t]=exintpub
    pubintratestorico[t+tpub]=bondinterest[t]
    
    
    
    
    
    #Aggiornamento capitale installato: il capitale odierno corrisponde alla composizione delle diverse quantità di capitale installate nei periodi precedenti e deprezzati in ogni periodo, in più arriva il capitale ordinato nel periodo precedente
    
    if(t<=dk){
      # Iinstalled[t,i]=kin
      #Iinstallednominal[t,i]=Iinstalled[t,i]*p[t,1]*(1+intrate[t]*spam)
      # kfutinstorico[t+z,i]=kin
      storicoI[t+z,(NFK+1):NF]=Iinstalled[t,(NFK+1):NF]
      storicoINfull[t+z,(NFK+1):NF]=Iinstallednominalfull[t,(NFK+1):NF]
      
    }else{
      Iinstalled[t,(NFK+1):NF]=kdordinatofeasiblestorico[t,(NFK+1):NF]  
      Iinstallednominal[t,(NFK+1):NF]=kdordinatofeasiblenominalstorico[t,(NFK+1):NF] 
      Iinstallednominalfull[t,(NFK+1):NF]=kdordinatofeasiblenominalstoricofull[t,(NFK+1):NF] 
      storicoI[t+z,(NFK+1):NF]=Iinstalled[t,(NFK+1):NF]
      storicoIN[t+z,(NFK+1):NF]=Iinstallednominal[t,(NFK+1):NF]
      storicoINfull[t+z,(NFK+1):NF]=Iinstallednominalfull[t,(NFK+1):NF]
    }
    
    
    #aggiornamento capitale:devo calcolare il deprezzamento per ogni singolo capitale istallato precedentemente che ad oggi compone tutto il capitale che dispongo
    #potrei anche farlo utilizzando lo storico ammortamento fisico
    s=1/z
    for(j in (t+1):(t+z)){
      k[t,(NFK+1):NF]=k[t,(NFK+1):NF]+storicoI[j,(NFK+1):NF]*s
      s=s+1/z
    }
    
    #ammortamento effettivo
    #prov[1:dk,(NFK+1):NF]=storicoIN[(z+1):(z+dk),(NFK+1):NF]
    #provfull[1:dk,(NFK+1):NF]=storicoINfull[(z+1):(z+dk),(NFK+1):NF]
    # storicoIN[(z+1):(z+dk),(NFK+1):NF]=0
    #storicoINfull[z+1,i]=0
    
    g=1/z
    for(j in (t+1):(t+z)){
      ammortamento[t,(NFK+1):NF]=ammortamento[t,(NFK+1):NF]+storicoIN[j,(NFK+1):NF]*g/spam
      ammortamentofull[t,(NFK+1):NF]=ammortamentofull[t,(NFK+1):NF]+storicoINfull[j,(NFK+1):NF]*g/spam
      g=g+1/z
    }
    
    
    #ammortamento con full leverage  --> serve per i prezzi
    
    # g=1/z
    # for(j in (t+1):(t+z)){
    #   ammortamentofull[t,(NFK+1):NF]=ammortamentofull[t,(NFK+1):NF]+storicoINfull[j,(NFK+1):NF]*g/spam
    #   g=g+1/z
    #   
    # }
    #storicoIN[(z+1):(z+dk),(NFK+1):NF]=prov[1:dk,(NFK+1):NF]
    #storicoINfull[z+1,i]=provfull
    
    
    
    
    
    
    #totaldemandcapital=sum(kd[t,])
    #aFK= ceiling(runif(NFC,min=0,max=NFK))
    #bFK=matrix(data=0, ncol=NFK)
    #indexFK=c(bFK,aFK)
    
    #ammortamento composito DECRESCENTE: nel primo periodo posso utilizzare maggiore capaictà produttiva quindi sostengo un ammortamento maggiore, nel secondo di meno e cosi via....GRANDE IDEAA che risolve il problema!!!!
    
    
    
    #for(i in (NFK+1):NF){
    # for(j in (t+1):(t+z)){
    #  if (storicoIN[j,i]>0){
    #   ammortamento[i]=ammortamento[i]+(storicoIN[j,i]/z)
    #  }
    #}
    #}
    ktot=sum(k[t,])
    
    
    #la spesa pubblica é distribuita in maniera proporzionale rispetto alla dimensione dell'impresa
    alfak[t,(NFK+1):NF]=k[t,(NFK+1):NF]/ktot
    
    alfasort=sort(alfak[t,(NFK+1):NF],decreasing=FALSE)
    stocnum=min(abs(alfasort[1]-rfoldnorm(1,mean=alfasort[1],sd=alfasort[1]*0.3)),alfasort[1]*0.9)
    shufflefirm=sample((NFK+1):NF)
    if((NFC %% 2)==0){
      for(i in 1:NFC){
        if((i %% 2)==0){
          alfak[t,shufflefirm[i]]=alfak[t,shufflefirm[i]]+stocnum
        }else{
          alfak[t,shufflefirm[i]]=alfak[t,shufflefirm[i]]-stocnum
        }
        alfak[t,(NFK+1):NF]=ifelse(is.nan(alfak[t,(NFK+1):NF])==TRUE,0,alfak[t,(NFK+1):NF])
        alfak[t,(NFK+1):NF]=ifelse(is.na(alfak[t,(NFK+1):NF])==TRUE,0,alfak[t,(NFK+1):NF])
        
      }
    }else{
      
      for(i in 1:NFC){ 
        
        if(i<NFC){
          if((i %% 2)==0){
            alfak[t,shufflefirm[i]]=alfak[t,shufflefirm[i]]+stocnum
          }else{
            alfak[t,shufflefirm[i]]=alfak[t,shufflefirm[i]]-stocnum
          }
          
          
        }
        alfak[t,(NFK+1):NF]=ifelse(is.nan(alfak[t,(NFK+1):NF])==TRUE,0,alfak[t,(NFK+1):NF])
        alfak[t,(NFK+1):NF]=ifelse(is.na(alfak[t,(NFK+1):NF])==TRUE,0,alfak[t,(NFK+1):NF])
        
      }
      
    }
    
    if(length(which(alfak[t,]<0))>0){
      print("alfak negativo")
      errortrial=numtrial
      save.image("/nobackup/bnldd/erroralfak.RData")     
      stop()
    }
    
    #COMPUTING UNIT COSTS & PRICE ... bisogna chiarire sto fatto se va bene che i costi unitari aumentano perchè l 'ammortamento è fisso, mentre il potenziale produttivo diminuisce a causa del deprezzamento--> vuol dire che anche l'ammortamento deve diminuire
    
    
    
    unitcost[t,1:NFK]=wageaggregate[t]/phi
    
    l_n[t,(NFK+1):NF]=u_n*k[t,(NFK+1):NF]/v 
    y_n[t,(NFK+1):NF]=u_n*k[t,(NFK+1):NF]*x[(NFK+1):NF]
    #costi unitari al leverage full
    unitcost[t,(NFK+1):NF]=round(wageaggregate[t]/(xexogenous*v)*(1+intrateshort[t]*spamshort)+ammortamentofull[t,(NFK+1):NF]/y_n[t,(NFK+1):NF],3)   
    #costi unitari al leverage effettivo
    #unitcost[t,(NFK+1):NF]=round(wageExogenous/(xexogenous*v)*(1+leveragew[t,(NFK+1):NF]*intrate[t])+ammortamento[t,(NFK+1):NF]/y_n[t,(NFK+1):NF],3)   #da rivedere: ci va la composizione storica del costo del capitale quando inserisco prezzi variabili
    
    
    unitcost[t,(NFK+1):NF]=ifelse(is.nan(unitcost[t,(NFK+1):NF])==TRUE,unitcost[t-1,(NFK+1):NF],unitcost[t,(NFK+1):NF])
    
    # if(t==1){
    #p[t,(NFK+1):NF]=unitcost[t,(NFK+1):NF]*(1+markupc)    
    #  }
    
    # ###costo unitario con full leverage###
    # for(i in 1:NF){
    #   if(i<=NFK){
    #     unitcost[t,i]=wageExogenous/phi
    #     #p[i]=unitcost[i]*(1+markup[i])
    #   }else{  
    #     l_n[t,i]=u_n*k[t,i]/v  #ci andrebbe il ceiling ma poi c'è il problema dei costi unitari che aumentano a caso: devo inserire il lavoro ad ore o aumetare i coefficienti di produzione
    #     y_n[t,i]=u_n*k[t,i]*x[i]
    #     #unitcost[t,i]=round(wageExogenous*l_n[t,i]/y_n[t,i]*(1+spam*intrate[t])+ammortamento[t,i]/y_n[t,i],3)   #da rivedere: ci va la composizione storica del costo del capitale quando inserisco prezzi variabili
    #     
    #     #costi unitari al leverage effettivo
    #    # unitcost[t,i]=round(wageExogenous/(xexogenous*v)*(1+leveragew[t,i]*intrate[t])+ammortamento[t,i]/y_n[t,i],3)   #da rivedere: ci va la composizione storica del costo del capitale quando inserisco prezzi variabili
    #     #costi unitari al leverage full
    #     unitcost[t,i]=round(wageExogenous/(xexogenous*v)*(1+intrateshort[t]*spamshort)+ammortamentofull[t,i]/y_n[t,i],3)   
    #     
    #     if(is.nan(unitcost[t,i])==TRUE){
    #       unitcost[t,i]=unitcost[t-1,i]
    #     }
    #     #  p[i]=unitcost[i]*(1+markup[i])
    #   }
    # }  
    
    
    # Fissazione prezzi #####
    
    markup[t,1:NF]=markupc      # rimane valido solo nel caso in cui i prezzi sono fissi
    
    
    
    if(FALSE){
      if(t>1){
        markup[t,1:NF]=markup[t-1,1:NF]
        ss=which(limabbassare[t-1,1:NFK]>limiteinvdec)
        if(length(ss>0)){
          stocvec=abs(rfoldnorm(length(ss),mean=mediaran,sd=sdran)-1)
          markup[t,ss]=pmax(markupmin,markup[t-1,ss]*(1-stocvec))
        }
        #p[t,ss]=unitcost[t,ss]*(1+markup[t,ss])
        dd=which(labordemandproblem[1:NFK]>=limlabor || limalzare[t-1,1:NFK]>limiteinvinc)
        if(length(dd)>0){
          stocvec=abs(rfoldnorm(length(dd),mean=mediaran,sd=sdran)-1)
          markup[t,dd]=pmax(markupmin,markup[t-1,dd]*(1+stocvec))
          #p[t,dd]=unitcost[t,dd]*(1+markup[t,dd])
        }
        ff=which(invliminf[t-1,1:NF]>=limiteinvdec)
        if(length(ff)>0){
          stocvec=abs(rfoldnorm(length(ff),mean=mediaran,sd=sdran)-1)
          markup[t,ff]=pmax(markupmin,markup[t-1,ff]*(1-stocvec))
        }
        tt=which(invlimsup[t-1,1:NF]>=limiteinvinc && u_med[t-1,1:NF]>umin)
        if(length(tt)>0){
          stocvec=abs(rfoldnorm(length(tt),mean=mediaran,sd=sdran)-1)
          markup[t,tt]=pmax(markupmin,markup[t-1,tt]*(1+stocvec))
        }
        p[t,1:NF]=unitcost[t,1:NF]*(1+markup[t,1:NF])
        
        
      }else{
        p[t,1:NF]=unitcost[t,1:NF]*(1+markup[t,1:NF])
      }
    }
    
    
    if(TRUE){  
      if(t>1){
        # imprese K
        for(i in 1:NFK){
          
          if(limabbassare[t-1,i]>limiteinvdec){ #| inv[t-1,i]>invt[t-1,i]){   #forse sarebbe meglio con le variazioni perchè magari si può accontentare di avere un market sahre molto basso: anzi no, uno si può accontentare un marketashare molto basso solo se è consapevole di valere di meno per il consumatore, ma qui per il momento le imrpese sono uguali agli occhi dei consumatori eccetto per il prezzo 
            
            #stocnum=sample(seq(0.001,0.005,0.001),1)
            stocnum=abs(rfoldnorm(1,mean=mediaran,sd=sdran)-1)
            markup[t,i]=max(markupmin,markup[t-1,i]*(1-stocnum))
            p[t,i]=unitcost[t,i]*(1+markup[t,i])
            #pcup[t,i]=pmax(p[t-1,i],unitcost[t,i]+markupmin)    #la seconda opzione dopo OR sembrerebbe ridoondante in un mercato on spot, serve invece solo nel secondo periodo:non lo è per il fatto che ho messo il primo periodo non on spot
            # pfloor[t,i]=p[t-1,i]*(1-delta)
            # pfloor[t,i]=pmax(unitcost[t,i]+markupmin,pfloor[t,i])
            #p[t,i]=round(runif(1,pfloor[t,i],pcup[t,i]),1)
          }else if(limalzare[t-1,i]>limiteinvinc){      #(inv[t-1,i]<invt[t-1,i]  | demandperfirmcapital[t-1,i]>realsails[t-1,i]){
            #stocnum=sample(seq(0.001,0.005,0.001),1)
            stocnum=abs(rfoldnorm(1,mean=mediaran,sd=sdran)-1)
            markup[t,i]=max(markupmin,markup[t-1,i]*(1+stocnum))  
            p[t,i]=unitcost[t,i]*(1+markup[t,i])
            # pfloor[t,i]=p[t-1,i]       #prezzo a cui è sicurissimo di vendere tutta la produzione (sotto il prezzo di mercato)
            #pcup[t,i]=p[t-1,i]*(1+delta)
            # p[t,i]=round(runif(1,pfloor[t,i],pcup[t,i]),1)
          }else{  #ossia se il markeshare è uguale ad uno:
            markup[t,i]=markup[t-1,i]
            p[t,i]=unitcost[t,i]*(1+markup[t,i])
            #p[t,i]=p[t-1,i]   #quando le cose vanno bene, comunque sia l 'mipresa amplia il raggio di azione
            #pfloor[t,i]=pfloor[t-1,i]*(1-delta)
            #pfloor[t,i]=pmax(unitcost[t,i]+markupmin,pfloor[t,i])
            #pcup[t,i]=pcup[t-1,i]*(1+delta)
          }}
        
        #imprese consumer:
        for(i in (NFK+1):NF){
          if(invliminf[t-1,i]>=limiteinvdec & t>200){ #dovrei smorzare questo effetto oppure se vendire realizzate sono maggiori di autteso
            #stocnum=sample(seq(0.001,0.005,0.001),1)
            #markup[t,i]=max(markupmin,markup[t-1,i]-stocnum)
            stocnum=abs(rfoldnorm(1,mean=mediaran,sd=sdran)-1)
            markup[t,i]=max(markupmin,markup[t-1,i]*(1-stocnum))
            p[t,i]=unitcost[t,i]*(1+markup[t,i])
            invliminf[t,i]=0
            invlimsup[t,i]=0
          }else if(invlimsup[t-1,i]>=limiteinvinc & u_med[t-1,i]>umin & t>200){     #(u_l[t-1,i]==u_r[t-1,i] & inv[t-1,i]<invt[t-1,i]) ){# | (u_r[t-1,i]==1 & inv[t-1,i]<invt[t-1,i])){    #oppure quando non sono riuscito a trovare lavoratori liberi o sufficiente capitale
            #stocnum=sample(seq(0.001,0.005,0.001),1)
            #markup[t,i]=markup[t-1,i]+stocnum
            stocnum=abs(rfoldnorm(1,mean=mediaran,sd=sdran)-1)
            markup[t,i]=max(markupmin,markup[t-1,i]*(1+stocnum))
            p[t,i]=unitcost[t,i]*(1+markup[t,i])
            #pfloor[t,i]=p[t-1,i]       #prezzo a cui è sicurissimo di vendere tutta la produzione (sotto il prezzo di mercato)
            #pcup[t,i]=round(p[t-1,i]*(1+delta),2)
            #p[t,i]=round(runif(1,pfloor[t,i],pcup[t,i]),2)
            # }
            invliminf[t,i]=0
            invlimsup[t,i]=0
          }else{
            
            markup[t,i]=markup[t-1,i]
            p[t,i]=unitcost[t,i]*(1+markup[t,i])
            invliminf[t,i]=invliminf[t-1,i]
            invlimsup[t,i]=invlimsup[t-1,i]
            # p[t,i]=p[t-1,i]   #quando le cose vanno bene, comunque sia l 'mipresa ampli il raggio di azione
            # pfloor[t,i]=round(pfloor[t-1,i]*(1-delta),2)
            # pfloor[t,i]=round(pmax(unitcost[t,i]+markupmin,pfloor[t,i]),2)
            # pcup[t,i]=pcup[t-1,i]*(1+delta)
          }
        }
      }else{
        for(i in 1:NF){
          p[t,i]=unitcost[t,i]*(1+markup[t,i])
        }
      }
      
    }   
    
    
    #Produzione e domanda capitale settore C ####
    if(TRUE){
      if(t>2){
        #stocnum
        
        q_e[t,which(cod==1 & bancarotta[t-1,]==0)]=ifelse(round(inv[t-1,which(cod==1 & bancarotta[t-1,]==0)],5)>0,beta[which(cod==1 & bancarotta[t-1,]==0)]*realsails[t-1,which(cod==1 & bancarotta[t-1,]==0)]+(1-beta[which(cod==1 & bancarotta[t-1,]==0)])*q_e[t-1,which(cod==1 & bancarotta[t-1,]==0)],(beta[which(cod==1 & bancarotta[t-1,]==0)]*realsails[t-1,which(cod==1 & bancarotta[t-1,]==0)]+(1-beta[which(cod==1 & bancarotta[t-1,]==0)])*q_e[t-1,which(cod==1 & bancarotta[t-1,]==0)])*1)  #nel caso di memoria storica con pesi decrescenti 
        #lung=length(which(bancarotta[t-1,]))
        if(length(which(bancarotta[t-1,]==1))>0){
          stocvec=abs(rfoldnorm(length(which(bancarotta[t-1,]==1)),mean=0.1,sd=0.0096))
          q_e[t,which(cod==1 & bancarotta[t-1,]==1)]=stocvec*(beta[which(cod==1 & bancarotta[t-1,]==1)]*realsails[t-1,which(cod==1 & bancarotta[t-1,]==1)]+(1-beta[which(cod==1 & bancarotta[t-1,]==1)])*q_e[t-1,which(cod==1 & bancarotta[t-1,]==1)])
          gex[t-1,which(cod==1 & bancarotta[t-1,]==1)]=0
          gex[t-2,which(cod==1 & bancarotta[t-1,]==1)]=0
          gr[t-1,which(cod==1 & bancarotta[t-1,]==1)]=0
        }
        if(t>3){
          gex[t,(NFK+1):NF]=gex[t-1,(NFK+1):NF]+alfa[(NFK+1):NF]*(gr[t-1,(NFK+1):NF]-gex[t-1,(NFK+1):NF])
          
        }else{
          gex[t,(NFK+1):NF]=gr[t-1,(NFK+1):NF]
        }
        gex[t,(NFK+1):NF]=ifelse(is.infinite(gex[t,(NFK+1):NF]==TRUE),0.5,gex[t,(NFK+1):NF])
        q_dn[t,(NFK+1):NF]=q_e[t,(NFK+1):NF]*(1+espe*gex[t,(NFK+1):NF])   #*(1+sigma)
        k_de[t,(NFK+1):NF]=q_dn[t,(NFK+1):NF]/(u_n*x[(NFK+1):NF])
        s=1/z
        # calcolo del quantitativo di capitale che l'impresa avrebbe nel periodo t+dkm nel caso in cui non ordinasse niente
        for(j in (t+dk+1):(t+z+dk-1)){
          capfuturo[t,(NFK+1):NF]=capfuturo[t,(NFK+1):NF]+kfutinstorico[j,(NFK+1):NF]*s
          s=s+1/z
        }
        
        
        # 
        kd[t,(NFK+1):NF]=pmax(0,gamma*(k_de[t,(NFK+1):NF]-capfuturo[t,(NFK+1):NF]))
        #kd[t,(NFK+1):NF]=ifelse((kd[t,(NFK+1):NF]+capfuturo[t,(NFK+1):NF])*x[(NFK+1):NF]<q_dn[t,(NFK+1):NF],round(q_dn[t,(NFK+1):NF]/x[(NFK+1):NF]-capfuturo[t,(NFK+1):NF],4), kd[t,(NFK+1):NF])
        # 
        uefuturo[t,(NFK+1):NF]=ifelse(q_dn[t,(NFK+1):NF]==0,0,q_dn[t,(NFK+1):NF]/(capfuturo[t,(NFK+1):NF]*x[(NFK+1):NF]))
        uefuturo[t,(NFK+1):NF]=ifelse(is.infinite(uefuturo[t,(NFK+1):NF])==TRUE,1,uefuturo[t,(NFK+1):NF])
        
        invt[t,(NFK+1):NF]=sigma*q_e[t,(NFK+1):NF]
        y_d[t,(NFK+1):NF]=pmax(0,q_e[t,(NFK+1):NF]-inv[t-1,(NFK+1):NF]+invt[t,(NFK+1):NF])
        ue[t,(NFK+1):NF]=ifelse(y_d[t,(NFK+1):NF]==0 | k[t,(NFK+1):NF]==0,0,y_d[t,(NFK+1):NF]/(k[t,(NFK+1):NF]*x[(NFK+1):NF]))
        ue_f[t,(NFK+1):NF]=ifelse(y_d[t,(NFK+1):NF]==0 | k[t,(NFK+1):NF]==0,0,pmin(1,ue[t,(NFK+1):NF]))
        #unità di lavoro richieste (ore)
        lhrequired[t,(NFK+1):NF]=ifelse(y_d[t,(NFK+1):NF]==0 | k[t,(NFK+1):NF]==0,0,ue_f[t,(NFK+1):NF]*k[t,(NFK+1):NF]/v )
        #numero di lavoratori richiesti
        lrequired[t,(NFK+1):NF]=ceiling(lhrequired[t,(NFK+1):NF]/hmese)
        
        
        u_med[t,(NFK+1):NF]=ualfa[(NFK+1):NF]*ue_f[t,(NFK+1):NF]+(1-ualfa[(NFK+1):NF])*u_med[t-1,(NFK+1):NF]
        # kd[t,(NFK+1):NF]=ifelse(u_med[t,(NFK+1):NF]>u_n & uefuturo[t,(NFK+1):NF]>u_n,round(pmax(0,k_de[t,(NFK+1):NF]-capfuturo[t,(NFK+1):NF]),4),(ifelse(capfuturo[t,(NFK+1):NF]*x[(NFK+1):NF]<q_dn[t,(NFK+1):NF],round(q_dn[t,(NFK+1):NF]/x[(NFK+1):NF]-capfuturo[t,(NFK+1):NF],4),0)))
        
      }else{
        
        if(t==1){
          q_e[t,(NFK+1):NF]=qexogenousc
        }else{
          q_e[t,(NFK+1):NF]=realsails[t-1,(NFK+1):NF] #*b+demandperfirmconsumer[t-2,i]*(1-b): dal terzo periodo#posso cambiarla e metterci quella in toward a benchmark model stigliz ecc
        }
        q_dn[t,(NFK+1):NF]=q_e[t,(NFK+1):NF]*(1+espe*gex[t,(NFK+1):NF])#*(1+sigma)
        k_de[t,(NFK+1):NF]=q_dn[t,(NFK+1):NF]/(u_n*x[(NFK+1):NF])
        s=1/z
        for(j in (t+dk+1):(t+z+dk-1)){
          capfuturo[t,(NFK+1):NF]=capfuturo[t,(NFK+1):NF]+kfutinstorico[j,(NFK+1):NF]*s
          s=s+1/z
        }
        invt[t,(NFK+1):NF]=sigma*q_e[t,(NFK+1):NF]
        if(t==1){
          y_d[t,(NFK+1):NF]=max(0,q_e[t,(NFK+1):NF]+invt[t,(NFK+1):NF])
        }else{
          y_d[t,(NFK+1):NF]=max(0,q_e[t,(NFK+1):NF]-inv[t-1,(NFK+1):NF]+invt[t,(NFK+1):NF])
        }
        ue[t,(NFK+1):NF]=ifelse(y_d[t,(NFK+1):NF]==0 | k[t,(NFK+1):NF]==0,0,y_d[t,(NFK+1):NF]/(k[t,(NFK+1):NF]*x[(NFK+1):NF]))
        ue_f[t,(NFK+1):NF]=ifelse(y_d[t,(NFK+1):NF]==0 | k[t,(NFK+1):NF]==0,0,pmin(1,ue[t,(NFK+1):NF]))
        lhrequired[t,(NFK+1):NF]=ifelse(y_d[t,(NFK+1):NF]==0 | k[t,(NFK+1):NF]==0,0,ue_f[t,(NFK+1):NF]*k[t,(NFK+1):NF]/v )
        lrequired[t,(NFK+1):NF]=ceiling(lhrequired[t,(NFK+1):NF]/hmese)
        uefuturo[t,(NFK+1):NF]=ifelse(q_dn[t,(NFK+1):NF]==0,0,q_dn[t,(NFK+1):NF]/(capfuturo[t,(NFK+1):NF]*x[(NFK+1):NF]))
        uefuturo[t,(NFK+1):NF]=ifelse(is.infinite(uefuturo[t,(NFK+1):NF])==TRUE,1,uefuturo[t,(NFK+1):NF])
        
        if(t==1){
          u_med[t,(NFK+1):NF]=ue_f[t,(NFK+1):NF]
        }else{
          u_med[t,(NFK+1):NF]=ualfa[(NFK+1):NF]*ue_f[t,(NFK+1):NF]+(1-ualfa[(NFK+1):NF])*ue_f[t-1,(NFK+1):NF]
        }
        
        kd[t,(NFK+1):NF]=pmax(0,gamma*(k_de[t,(NFK+1):NF]-capfuturo[t,(NFK+1):NF]))
        kd[t,(NFK+1):NF]=ifelse((kd[t,(NFK+1):NF]+capfuturo[t,(NFK+1):NF])*x[(NFK+1):NF]<q_dn[t,(NFK+1):NF],round(q_dn[t,(NFK+1):NF]/x[(NFK+1):NF]-capfuturo[t,(NFK+1):NF],4), kd[t,(NFK+1):NF])
        #kd[t,(NFK+1):NF]=ifelse(u_med[t,(NFK+1):NF]>u_n && uefuturo[t,(NFK+1):NF]>u_n,round(pmax(0,k_de[t,(NFK+1):NF]-capfuturo[t,(NFK+1):NF]),4),(ifelse(capfuturo[t,(NFK+1):NF]*x[(NFK+1):NF]<q_dn[t,(NFK+1):NF],round(q_dn[t,(NFK+1):NF]/x[(NFK+1):NF]-capfuturo[t,(NFK+1):NF],4),0)))
        
        
      }
    }
    
    #Produzione e domanda capitale settore C
    
    
    if(FALSE){
      for(i in (NFK+1):NF){
        if(t<=2){
          if(t==1){
            q_e[t,i]=qexogenousc
          }else{
            q_e[t,i]=realsails[t-1,i] #*b+demandperfirmconsumer[t-2,i]*(1-b): dal terzo periodo#posso cambiarla e metterci quella in toward a benchmark model stigliz ecc
          }
          #q_dn[t,i]=q_e[t,i]*(1+sigma)
          q_dn[t,i]=q_e[t,i]*(1+gex[t,i]*espe)*(1+sigma)
          k_de[t,i]=q_dn[t,i]/(u_n*x[i])
          
          
          s=1/z
          for(j in (t+dk+1):(t+z+dk-1)){
            if(kfutinstorico[j,i]>0){
              capfuturo[t,i]=capfuturo[t,i]+kfutinstorico[j,i]*s
            }
            s=s+1/z
          }
          
          #capitale desiderato
          kd[t,i]=round(max(0,gamma*(k_de[t,i]-capfuturo[t,i])),4)
          if((kd[t,i]+capfuturo[t,i])*x[i]<q_dn[t,i]){
            kd[t,i]=round(q_dn[t,i]/x[i]-capfuturo[t,i],4)
          }
          
          #fissazione produzione e domanda di lavoro
          invt[t,i]=sigma*q_e[t,i]
          if(t==1){
            y_d[t,i]=max(0,q_e[t,i]+invt[t,i])
          }else{
            y_d[t,i]=max(0,q_e[t,i]-inv[t-1,i]+invt[t,i])
          }
          ue[t,i]=y_d[t,i]/(k[t,i]*x[i])
          #  tabella_micro[i,7,1,t]=ue[i]
          ue_f[t,i]=min(1,ue[t,i])
          # tabella_micro[i,8,1,t]=ue_f[i]
          lhrequired[t,i]=ue_f[t,i]*k[t,i]/v   #ceiling(ue_f[t,i]*k[t,i]/v)
          # tabella_micro[i,6,1,t]=lrequired[i]
        }else{
          #q_e[i]=beta*demandperfirmconsumer[t-1,i]+(1-beta)*demandperfirmconsumer[t-2,i]
          if(bancarotta[t-1,i]==0){
            q_e[t,i]=beta[i]*realsails[t-1,i]+(1-beta[i])*q_e[t-1,i]  #nel caso di memoria storica con pesi decrescenti 
            #q_e[t,i]=demandperfirmconsumer[t-1,i]*beta+(1-beta)*demandperfirmconsumer[t-2,i]
            #tabella_micro[i,9,1,t]=q_e[i]
          }else{
            stocnum=abs(rfoldnorm(1,mean=0.4,sd=0.0095))
            q_e[t,i]=stocnum*(beta[i]*realsails[t-1,i]+(1-beta[i])*q_e[t-1,i])
            gex[t-1,i]=0
            gex[t-2,i]=0
            gr[t-1,i]=0
            if(is.infinite(gex[t,2])==TRUE){
              gex[t,2]=0
            }
            
          }
          #q_dn[t,i]=q_e[t,i]*(1+sigma)
          gex[t,i]=gex[t-2,i]+alfa[(NFK+1):NF]*(gr[t-1,i]-gex[t-2,i])
          if(is.infinite(gex[t,i])==TRUE){
            gex[t,i]=0
          }
          q_dn[t,i]=q_e[t,i]*(1+espe*gex[t,i])*(1+sigma)
          k_de[t,i]=q_dn[t,i]/(u_n*x[i])
          s=1/z
          for(j in (t+dk+1):(t+z+dk-1)){
            if(kfutinstorico[j,i]>0){
              capfuturo[t,i]=capfuturo[t,i]+kfutinstorico[j,i]*s
            }
            s=s+1/z
          }
          kd[t,i]=round(max(0,gamma*(k_de[t,i]-capfuturo[t,i])),4)
          if((kd[t,i]+capfuturo[t,i])*x[i]<q_dn[t,i]){
            kd[t,i]=round(q_dn[t,i]/x[i]-capfuturo[t,i],4)
          }
          
          
          
          invt[t,i]=sigma*q_e[t,i]
          
          y_d[t,i]=max(0,q_e[t,i]-inv[t-1,i]+invt[t,i])
          
          
          
          
          if(y_d[t,i]==0){
            ue[t,i]=0
            ue_f[t,i]=0
            lhrequired[t,i]=0
            u_r[t,i]=0
            
          }else if(k[t,i]==0){
            ue[t,i]=0
            ue_f[t,i]=0
            lhrequired[t,i]=0
            u_r[t,i]=0
          }else{
            
            ue[t,i]=y_d[t,i]/(k[t,i]*x[i])
            ue_f[t,i]=min(1,ue[t,i])
            lhrequired[t,i]=ue_f[t,i]*k[t,i]/v   #ceiling(ue_f[t,i]*k[t,i]/v)
            
          }
          
        }
        
        lrequired[t,i]=ceiling(lhrequired[t,i]/hmese)
        #lperwork[t,i]=lhrequired[t,i]/lrequired[t,i]
        # wperwork[t,i]=wageExogenous*lperwork[t,i]
        
        #employees[t,i]=lrequired[t,i]
        #wb[t,i]=employees[t,i]*wageExogenous
      }
      
    }  
    
    
    totaldemandcapital=sum(kd[t,])
    
    
    if(FALSE){
      #prima assegnazione delle imprese che vendono capitali alle imrpese consumer
      for (i in (NFK+1):NF){
        potentialSuppliers=sample(NFK)[1:parmatchingcapital]
        minPrice=min(p[t,potentialSuppliers])
        indexFK[t,i]=potentialSuppliers[p[t,potentialSuppliers]==minPrice][1]  
        #kstoricoordinatonominal[t+dk-1,i]=kd[t,i]*p[t,indexFK[t,i]]
        #kdfeasible[t,i]=kstoricoordinato[t,i]
        #kdfeasiblenominal[t,i]=kstoricoordinatonominal[t,i]
      }
    }
    
    
    
    
    
    
    ###Matching mercato capitale ####
    
    if(TRUE){
      
      
      if(sceltastocasticaK==1){
        
        fun <- function(x) {
          # function to print x raised to the power y
          return(sample(1:NFK)[1])
        }
        indexFK[t,(NFK+1):NF]=sapply((NFK+1):NF,fun)
      }else{
        #prima assegnazione delle imprese che vendono capitali alle imrpese consumer
        for (i in (NFK+1):NF){
          if(NFK>=parmatchingcapital){
            potentialSuppliers=sample(NFK)[1:parmatchingcapital]
          }else{
            potentialSuppliers=sample(NFK)
          }
          minPrice=min(p[t,potentialSuppliers])
          #indexFK[t,i]=potentialSuppliers[p[t,potentialSuppliers]==minPrice][1]
          if(t>1 && is.na(indexFK[t-1,i])==FALSE){
            
            if(minPrice<p[t,indexFK[t-1,i]]){
              proba=1-e^(swicK*((minPrice-p[t,indexFK[t-1,i]])/minPrice))  #probabilità di cambiare fornitore
              prob=c(proba,1-proba)
              sol=rbinom(1, size=1, prob=prob)
              if(sol==1){
                indexFK[t,i]=potentialSuppliers[p[t,potentialSuppliers]==minPrice][1]
              }else{
                indexFK[t,i]=indexFK[t-1,i]
              }
            }else{
              indexFK[t,i]=indexFK[t-1,i]
            }
          }else{
            indexFK[t,i]=potentialSuppliers[p[t,potentialSuppliers]==minPrice][1]
          }
          
          #kstoricoordinatonominal[t+dk-1,i]=kd[t,i]*p[t,indexFK[t,i]]
          #kdfeasible[t,i]=kstoricoordinato[t,i]
          #kdfeasiblenominal[t,i]=kstoricoordinatonominal[t,i]
        }
      }
    }
    
    
    if(t==1 & equaldistribution==1){
      indexFK[t,(NFK+1):NF]=indexcapitale
    }
    
    
    if(FALSE){
      for(i in 1:NFK){
        
        demandperfirmcapital[t,i]=sum(kd[t,which(indexFK[t,]==i)])
        # storicoordini[t+dk-1,i]=demandperfirmcapital[t,i]/dk
        storicoprezzo[t+dk-1,i]=p[t,i]
        #calcoliamo la produzione necessaria sugli ordini passati
        if(dk>1){
          #quantità di semilavorati ancora da produrre
          for(j in 2:dk){
            y_old[t,i]=y_old[t,i]+storicoordinifeasible[t+dk-j,i]
            #valore storico della % di semilavorato che andrò a produrre in questo periodo (serve per il calcolo del GDP)
            y_oldvalore[t,i]=y_oldvalore[t,i]+storicoordinifeasible[t+dk-j,i]*storicoprezzo[t+dk-j,i]
          }
          #quantitativo che l'impresa desidera produre nel periodo
          y_d[t,i]=y_old[t,i]+demandperfirmcapital[t,i]/dk
        }else{
          y_d[t,i]=demandperfirmcapital[t,i]
        }
        
        if(t>1){
          y_d[t,i]=max(0, y_d[t,i]-inv[t-1,i]) #in realtà questo settore non ha scorte (le avrebbe solo se, a causa del ceiling, ha più ore lavoro a dosposizione del necessario, cmq questa cosa non c'è pù ora)
          
        }
        
        lhrequired[t,i]=y_d[t,i]/phi     
        lrequired[t,i]=ceiling(lhrequired[t,i]/hmese)
        
      }
    }
    
    if(TRUE){
      fun <- function(x) {
        # function to print x raised to the power y
        return(sum(kd[t,which(indexFK[t,]==x)]))
      }
      demandperfirmcapital[t,1:NFK]=sapply(1:NFK,fun)
      storicoprezzo[t+dk-1,1:NFK]=p[t,1:NFK]
      if(dk>1){
        #quantità di semilavorati ancora da produrre
        for(j in 2:dk){
          y_old[t,1:NFK]=y_old[t,1:NFK]+storicoordinifeasible[t+dk-j,1:NFK]
          #valore storico della % di semilavorato che andrò a produrre in questo periodo (serve per il calcolo del GDP)
          y_oldvalore[t,1:NFK]=y_oldvalore[t,1:NFK]+storicoordinifeasible[t+dk-j,1:NFK]*storicoprezzo[t+dk-j,1:NFK]
        }
        #quantitativo che l'impresa desidera produre nel periodo
        y_d[t,1:NFK]=y_old[t,1:NFK]+demandperfirmcapital[t,1:NFK]/dk
      }else{
        y_d[t,1:NFK]=demandperfirmcapital[t,1:NFK]
      }
      
      if(t>1){
        y_d[t,1:NFK]=pmax(0, y_d[t,1:NFK]-inv[t-1,1:NFK]) #in realtà questo settore non ha scorte (le avrebbe solo se, a causa del ceiling, ha più ore lavoro a dosposizione del necessario, cmq questa cosa non c'è pù ora)
        
      }
      
      lhrequired[t,1:NFK]=y_d[t,1:NFK]/phi     
      lrequired[t,1:NFK]=ceiling(lhrequired[t,1:NFK]/hmese)
      
    }
    
    if(t>1){
      employees[t,]=employees[t-1,]
    }
    
    labordemand[t,]=lrequired[t,]-employees[t,]
    
    
    # MERCATO DEL LAVORO #####
    
    for (i in 1:NF){
      if(labordemand[t,i]<0){
        employeesShuffled=sample(length(which(employer==i))) 
        toBeFired=which(employer==i)[employeesShuffled[1:-labordemand[t,i]]]
        employees[t,i]=employees[t,i]+labordemand[t,i]
        unemp[toBeFired]=1
        employer[toBeFired]=NA
        labordemand[t,i]=0
      }
    }
    
    
    
    
    
    
    
    #activeFirms = which(labordemand[t,]>0)
    if(FALSE){
      while(length(which(labordemand[t,]>0))>0 & length(which(unemp[]==1))>0){
        activeFirms = which(labordemand[t,]>0)
        if(length(which(unemp[]==1))>1){
          workershuffle=sample(which(unemp[]==1))
          indexW=workershuffle[1]
        }else if(length(which(unemp[]==1))==1){
          indexW=which(unemp[]==1) 
        }
        indexEmployerTemp = ceiling(length(activeFirms) * runif(1))
        indexEmployer = activeFirms[indexEmployerTemp]
        employees[t,indexEmployer] = employees[t,indexEmployer] + 1
        labordemand[t,indexEmployer]=labordemand[t,indexEmployer]-1
        unemp[indexW] = 0
        employer[indexW] = indexEmployer
      }
    }
    
    
    
    
    
    
    if(TRUE){
      householdsShuffled=sample(Nwork) #mercato del lavoro base come in Caiani
      for (j in 1:Nwork) {  #i capitalisti non lavorano
        indexW = householdsShuffled[j]
        if (unemp[indexW] == 1) {
          activeFirms = which(labordemand[t,]> 0)
          if (length(activeFirms) > 0) {
            indexEmployerTemp = ceiling(length(activeFirms) * runif(1))
            indexEmployer = activeFirms[indexEmployerTemp]
            employees[t,indexEmployer] = employees[t,indexEmployer] + 1
            labordemand[t,indexEmployer]=labordemand[t,indexEmployer]-1
            unemp[indexW] = 0
            employer[indexW] = indexEmployer
            
          }else{
            employer[indexW] = NA
            #wage[t,indexW] = 0
          }
        }else{
          
          #wage[t,indexW] = w[indexW]*lperwork[t,employer[indexW]]
          #taxpayment[t,indexW]=w[indexW]*tauwork
          #wb[t,employer[indexW]] = wb[t,employer[indexW]] + w[indexW]*lperwork[t,employer[indexW]]
        }
      } 
    }
    # 
    # for(i in 1:NF){
    #   if(labordemand[t,i]>0){
    #     labordemandproblem[i]=labordemandproblem[i]+1
    #   }else{
    #     labordemandproblem[i]=0
    #   }
    # }
    # 
    
    # for(i in 1:NF){
    #   if(labordemand[t,i]<0){
    #     print("labordemandnegative")
    #   }
    #   }
    #  
    if(length(which(labordemand[t,]<0))>0){
      print("labordemandnegative")
    }
    
    #Produzione settore K ####
    if(TRUE){
      for(i in 1:NFK){
        if(lhrequired[t,i]>0){
          if(lrequired[t,i]==employees[t,i]){
            y[t,i]=y_d[t,i]
            lperwork[t,i]=lhrequired[t,i]/lrequired[t,i]
            if(is.nan(lperwork[t,i])==TRUE){
              lperwork[t,i]=0
            }
            #wperwork[t,i]=wageExogenous*lperwork[t,i]
            wb[t,i]=sum(w[which(employer==i)])*lperwork[t,i]
          }else{
            y[t,i]=employees[t,i]*hmese*phi
            lperwork[t,i]=hmese
            #wperwork[t,i]=wageExogenous*lperwork[t,i]
            wb[t,i]=sum(w[which(employer==i)])*lperwork[t,i]
          }
          lwork[t,which(employer==i)]=lperwork[t,i]
          y_new[t,i]=y[t,i]-y_old[t,i]
          y_valorek[t,i]=y_oldvalore[t,i]+y_new[t,i]*p[t,i]
        }
        if(t==1){
          inv[t,i]=y_new[t,i]
        }else{# a causa del fatto che i lavoratori sono unitari (non posso avere mezzo lavoratore), la produzione può non corrisopndere a quella desiderata, per questo inserisco sta roba
          inv[t,i]=y_new[t,i]+inv[t-1,i]
        }
        #realsails[t,i]=storicoordini[t,i]*dk
        #inv[t,i]= inv[t,i]-realsails[t,i]
        # revenues[t,i]=realsails[t,i]*storicoprezzo[t,i]
        
      }
    }
    
    if(FALSE){
      
      s=which(lhrequired[t,1:NFK]>0)
      y[t,s]=ifelse(lrequired[t,s]==employees[t,s], y_d[t,s], employees[t,s]*hmese*phi)
      lperwork[t,s]=ifelse(lrequired[t,s]==employees[t,s],lhrequired[t,s]/lrequired[t,s],hmese)
      
      fun2<-function(x){
        return(sum(w[which(employer[]==x)])*lperwork[t,x])
      }
      wb[t,s]=sapply(s,fun2)
      
      fun3<-function(x){
        return(lperwork[t,employer[x]])
      }
      lwork[t,1:Nwork]=sapply(1:Nwork,fun3)
      
      y_new[t,s]=y[t,s]-y_old[t,s]
      y_valorek[t,s]=y_oldvalore[t,s]+y_new[t,s]*p[t,s]
      if(t==1){
        inv[t,1:NFK]=y_new[t,1:NFK]
      }else{# a causa del fatto che i lavoratori sono unitari (non posso avere mezzo lavoratore), la produzione può non corrisopndere a quella desiderata, per questo inserisco sta roba
        inv[t,1:NFK]=y_new[t,1:NFK]+inv[t-1,1:NFK]
      }
    }
    
    
    #valroe della produzione del settore K, al prezzo storico ##
    
    
    
    if(FALSE){  
      if((round(kd[t,2]/dk,1))<=(round(inv[t,1],1))){
        kdordinatofeasible[t,2]=kd[t,2]
        kdordinatofeasiblestorico[t+dk,2]=kd[t,2]
        #kdordinatofeasiblenominalstorico[t+dk,2]=kdordinatofeasible[t,2]*p[t,1]*(1+spam2*intrate[t]*leverage[t,2])
        kdordinatofeasiblenominalstoricofull[t+dk,2]=kd[t,2]*p[t,1]*(1+spam2*intrate[t])
        kdfeasiblenominal[t,2]=kd[t,2]*p[t,1]
        kfutinstorico[t+dk+z,2]=kd[t,2]
        sails[t,1]=kd[t,2]/dk
        inv[t,1]=inv[t,1]-kd[t,2]/dk
        storicoordinifeasible[t+dk-1,2]=kd[t,2]
        realsails[t,1]=kd[t,2]
        #realsails[t,i]=storicoordinifeasible[t,i]*dk
        revenues[t,1]=realsails[t,1]*p[t,1]
        
      }else{
        
        print("problemk")
        print(t)
        kdordinatofeasible[t,2]=kd[t,2]
        kdordinatofeasiblestorico[t+dk,2]=kd[t,2]
        #kdordinatofeasiblenominalstorico[t+dk,2]=kdordinatofeasible[t,2]*p[t,1]*(1+spam2*intrate[t]*leverage[t,2])
        kdordinatofeasiblenominalstoricofull[t+dk,2]=kd[t,2]*p[t,1]*(1+spam2*intrate[t])
        kdfeasiblenominal[t,2]=kd[t,2]*p[t,1]
        kfutinstorico[t+dk+z,2]=kd[t,2]
        sails[t,1]=kd[t,2]/dk
        inv[t,1]=inv[t,1]-kd[t,2]/dk
        storicoordinifeasible[t+dk-1,2]=kd[t,2]
        realsails[t,1]=kd[t,2]
        #realsails[t,i]=storicoordinifeasible[t,i]*dk
        revenues[t,1]=realsails[t,1]*p[t,1]
        
        
      }
    }   
    
    
    
    # Vendite effettive dal settore K a C ####### 
    #dopo il matching nel mercato del lavoro si verifica se effettivamente le imprese K riescono a produrre quanto ordinato (la produzione derivante dagli ordini dei periodi precedenti è sicuramente soddisfatta perchè stiamo ipotizzando che ilavoratori non se ne vanno autonomamenete)
    if(TRUE){
      for(i in 1:NFK){
        if(inv[t,i]>0){
          customC=which(indexFK[t,]==i)[kd[t,which(indexFK[t,]==i)]>0]          
          if(length(customC>0)){
            shufflecustomC=sample(length(customC))
            for(j in 1:length(customC)){
              indexCK=customC[shufflecustomC[j]]
              if((kd[t,indexCK]/dk)<=inv[t,i]){
                sails[t,i]=sails[t,i]+kd[t,indexCK]/dk
                inv[t,i]=inv[t,i]-kd[t,indexCK]/dk
                kdordinatofeasible[t,indexCK]=kd[t,indexCK]
                kdordinatofeasiblestorico[t+dk,indexCK]=kd[t,indexCK]
                #questo sotto lo utilizzo solo per calcolare il valore residuo del capitale quando fallisco
                #kdordinatofeasiblenominalstorico[t+dk,indexCK]=kd[t,indexCK]*p[t,i]*(1+spam2*intrate[t]*leverage[t,i])
                kdordinatofeasiblenominalstoricofull[t+dk,indexCK]=kd[t,indexCK]*p[t,i]*(1+spam2*intrate[t])
                kdfeasiblenominal[t,indexCK]=kd[t,indexCK]*p[t,i]
                kfutinstorico[t+dk+z,indexCK]=kd[t,indexCK]
                storicoordinifeasible[t+dk-1,i]=sails[t,i]
              }else{
                sails[t,i]=sails[t,i]+inv[t,i]
                kdordinatofeasible[t,indexCK]=inv[t,i]*dk
                kdordinatofeasiblestorico[t+dk,indexCK]=inv[t,i]*dk
                #kdordinatofeasiblenominalstorico[t+dk,indexCK]=inv[t,i]*p[t,i]*dk*(1+spam2*intrate[t]*leverage[t,i])
                kdordinatofeasiblenominalstoricofull[t+dk,indexCK]=inv[t,i]*p[t,i]*dk*(1+spam2*intrate[t])
                kfutinstorico[t+dk+z,indexCK]=inv[t,i]*dk
                kdfeasiblenominal[t,indexCK]=inv[t,i]*p[t,i]*dk
                storicoordinifeasible[t+dk-1,i]=sails[t,i]
                inv[t,i]=0
                #print("aa")
                #print(t)
                #print(i)
                #print(indexCK)
                
              }
            }
          }
        }
      }
      
      
      realsails[t, 1:NFK]=sails[t, 1:NFK]*dk
      #realsails[t,i]=storicoordinifeasible[t,i]*dk
      revenues[t, 1:NFK]=realsails[t, 1:NFK]*p[t, 1:NFK] #  storicoprezzo[t,i]
      storicovaluesails[t+dk, 1:NFK]=revenues[t, 1:NFK]/dk
      storicosales[t+dk, 1:NFK]=sails[t, 1:NFK]
      
      
      # for(i in 1:NFK){
      #  #  storicoordinifeasible[t+dk-1,i]=sails[t,i]
      #    realsails[t,i]=sails[t,i]*dk
      #    #realsails[t,i]=storicoordinifeasible[t,i]*dk
      #    revenues[t,i]=realsails[t,i]*p[t,i] #  storicoprezzo[t,i]
      #    storicovaluesails[t+dk,i]=revenues[t,i]/dk
      #    storicosales[t+dk,i]=sails[t,i]
      #  }
    }
    
    if(FALSE){ 
      u_l[t,(NFK+1):NF]=employees[t,(NFK+1):NF]*hmese*v/k[t,(NFK+1):NF]
      u_l[t,(NFK+1):NF]=ifelse(is.nan(u_l[t,(NFK+1):NF])==TRUE,0,u_l[t,(NFK+1):NF])
      
      y[t,(NFK+1):NF]=ifelse(lrequired[t,(NFK+1):NF]==employees[t,(NFK+1):NF],pmin(y_d[t,(NFK+1):NF],k[t,(NFK+1):NF]*x[(NFK+1):NF]),pmin(u_l[t,(NFK+1):NF]*k[t,(NFK+1):NF]*x[(NFK+1):NF],k[t,(NFK+1):NF]*x[(NFK+1):NF]))
      
      lperwork[t,(NFK+1):NF]=ifelse(lrequired[t,(NFK+1):NF]==employees[t,(NFK+1):NF],lhrequired[t,(NFK+1):NF]/lrequired[t,(NFK+1):NF],hmese)
      lperwork[t,(NFK+1):NF]=ifelse(is.nan(lperwork[t,(NFK+1):NF])==TRUE,0,lperwork[t,(NFK+1):NF])
      
      wb[t,(NFK+1):NF]=sum(w[which(employer==(NFK+1):NF)])*lperwork[t,(NFK+1):NF]    
      
      u_r[t,(NFK+1):NF]=y[t,(NFK+1):NF]/(k[t,(NFK+1):NF]*x[(NFK+1):NF]) 
      u_r[t,(NFK+1):NF]=ifelse(is.nan(u_r[t,(NFK+1):NF])==TRUE,0,u_r[t,(NFK+1):NF])
      
      if(t==1){
        inv[t,(NFK+1):NF]=y[t,(NFK+1):NF]
      }else{
        inv[t,(NFK+1):NF]=inv[t-1,(NFK+1):NF]+y[t,(NFK+1):NF]
      }
      
    }
    
    
    
    
    
    #Produzione settore C ###
    for(i in (NFK+1):NF){
      if(lhrequired[t,i]>0){
        if(lrequired[t,i]==employees[t,i]){
          y[t,i]=min(y_d[t,i],k[t,i]*x[i])
          lperwork[t,i]=lhrequired[t,i]/lrequired[t,i]
          if(is.nan(lperwork[t,i])==TRUE){
            lperwork[t,i]=0
          }
          # wperwork[t,i]=wageExogenous*lperwork[t,i]
          wb[t,i]=sum(w[which(employer==i)])*lperwork[t,i]
          u_l[t,i]=employees[t,i]*lperwork[t,i]*v/k[t,i]
        }else{
          u_l[t,i]=employees[t,i]*hmese*v/k[t,i]
          if(is.nan(u_l[t,i])==TRUE){
            u_l[t,i]=0
          }
          y[t,i]=min(u_l[t,i]*k[t,i]*x[i],k[t,i]*x[i])
          lperwork[t,i]=hmese
          #wperwork[t,i]=wageExogenous*lperwork[t,i]
          wb[t,i]=sum(w[which(employer==i)])*lperwork[t,i]
        }
        #y[t,i]=min(u_l[t,i]*k[t,i]*x[i],ue_f[t,i]*k[t,i]*x[i])
        
        u_r[t,i]=y[t,i]/(k[t,i]*x[i])  #oppure min(u_l[i],ue[i])
        k_used[t,i]=k[t,i]*u_r[t,i]
        if(is.nan(u_r[t,i])==TRUE){
          u_r[t,i]=0
        }
        if(t>=4){
          u_medio[t,i]=weighted.mean(u_r[(t-3):t,i],wg,na.rm=FALSE)
        }else{
          u_medio[t,i]=u_r[t,i]
        }
        lwork[t,which(employer==i)]=lperwork[t,i]
      }
      if(t==1){
        inv[t,i]=y[t,i]
      }else{
        inv[t,i]=inv[t-1,i]+y[t,i]
      }
      # k_sign[t,i]=alfa*u_r[t,i]*k[t,i]+(1-alfa)*k_sign[t-1,i]
    }
    
    
    #Calcolo ammontare sussidi alla disoccupazione    
    Ubenefitantes[t]=length(which(unemp==1))*wgov[t]
    #Ubenefit[t]=Ubenefitantes[t]
    # verifica e aggiustamento della spesa pubblica (tramite variazione del valore del sussidio), nel caso in cui non si srispetta il vincolo di bilancio
    if(FALSE){
      controllo[t]=(G[t]+Ubenefitantes[t]+interestpaymentG[t]-ProfitCB[t]-Tax[t-1]*(1+gexTax[t]))/(GDP[t-1]*(1+gexGDP[t]))
      if(controllo[t]>vincolo & vin=="si"){
        #raschia il fondo del barile
        Ubenefit[t]=max(0,GDP[t-1]*vincolo*(1+gexGDP[t])+Tax[t-1]*(1+gexTax[t])-interestpaymentG[t]+ProfitCB[t]-G[t]) 
        wgov[t]= Ubenefit[t]/(length(which(unemp==1)))
      }
    }
    
    series=seq(100,800,40)
    # Vincolo di bilancio pubblico #####  
    if(t==1){
      G[t]=Gin
      Ubenefit[t]=Ubenefitantes[t]
      
    }else{
      Ubenefit[t]=Ubenefitantes[t]
      #G[t]=G[t-1]*(1+gpub)*(1+inflazione[t-1])
      #Ubenefit[t]=Ubenefitantes[t]
      
      if(t>2){
        G[t]=G[t-1]/priceindexC[t-2]*priceindexC[t-1]*(1+gpub)
        
        
        # if(t %in% series){
        #   G[t]=G[1]*1.05
        # }else{
        #   G[t]=G[1]
        # }
        # 
        # if(t %in% series){
        #   G[t]=G[t-1]*1.05
        # }
        
        Greal1[t]=G[t]/priceindexC[t-1]
      }else{
        G[t]=Gin    #G[t-1]/pc*priceindexC[t-1]*(1+gpub)
      }
      
      #Ubenefit[t]=Ubenefitantes[t]
      # G[t]=Gin/pc*priceindexC[t-1]
      
      
      if(FALSE){
        G[t]=Gflat[t]-Ubenefitantes[t]-interestpaymentG[t]
        if(G[t]<0){
          print("problemG")
        }
      }
      perc_Ub[t]=Ubenefitantes[t]/(G[t]+Ubenefitantes[t])
      if(G[t]==0){
        perc_G[t]=perc_G[t-1]
      }else{
        perc_G[t]=G[t]/(G[t]+Ubenefitantes[t])
        
      }
      control[t]=(G[t]+Ubenefitantes[t]+interestpaymentG[t]-ProfitCB[t]-Tax[t-1]*(1+gexTax[t]))/(GDP[t-1]*(1+gexGDP[t]))
      # nel caso di vincolo di bilancio
      #if(control[t]>vincolo & vin=="si" & t>200){
      #   Deficitconsentito[t]=vincolo*GDP[t-1]*(1+gexGDP[t])
      #   DeficitProg[t]=G[t]+Ubenefitantes[t]+interestpaymentG[t]-ProfitCB[t]-Tax[t-1]*(1+gexTax[t])
      #   deltaD[t]=DeficitProg[t]-Deficitconsentito[t]
      #   perc_G=G[t]/(G[t]+Ubenefitantes[t])
      #   perc_Ub=Ubenefitantes[t]/(G[t]+Ubenefitantes[t])
      #   G[t]=max(0,G[t]-perc_G*deltaD[t])
      #   Ubenefit[t]=max(0,Ubenefitantes[t]-perc_Ub*deltaD[t])
      #   wgov[t]=Ubenefit[t]/(length(which(unemp==1)))
      #   #G[t]=max(0,GDP[t-1]*vincolo*(1+gexGDP[t])+Tax[t-1]*(1+gexTax[t])-interestpaymentG[t]+ProfitCB[t]-Nonemp[t-1]*(1+gexU[t])*wgov[t])
      # #  }else if(t>10){
      #  # SpesaconsentitaGU[t]=GDP[t-1]*(1+gexGDP[t])*vincolo-interestpaymentG[t]+ProfitCB[t]+Tax[t-1]*(1+gexTax[t])
      #  # Ubenefit[t]=max(min(Ubenefitantes[t],SpesaconsentitaGU[t]),0)
      #  # if(Ubenefit[t]>0){
      #  # wgov[t]=Ubenefit[t]/(length(which(unemp==1)))
      #  # G[t]=max(SpesaconsentitaGU[t]-Ubenefit[t],0)
      #  # }else{
      #   #  wgov[t]=0 
      #   #  G[t]=0
      #  # }
      #  }
      
      if(vin=="si" & t>50){
        SpesaconsentitaGU[t]=GDP[t-1]*(1+gexGDP[t])*vincolo-interestpaymentG[t]+ProfitCB[t]+Tax[t-1]*(1+gexTax[t])
        if(G[t]+Ubenefitantes[t]>SpesaconsentitaGU[t]){
          # perc_G[t]=G[t]/(G[t]+Ubenefitantes[t])
          # perc_Ub[t]=Ubenefitantes[t]/(G[t]+Ubenefitantes[t])
          G[t]=max(0,SpesaconsentitaGU[t]*perc_G[t])
          Ubenefit[t]=max(0,SpesaconsentitaGU[t]*perc_Ub[t])
          wgov[t]=Ubenefit[t]/(length(which(unemp==1)))
        }else{
          Ubenefit[t]=Ubenefitantes[t] #il sussidio rimane quello esogeno
          
          # G[t]=SpesaconsentitaGU[t]-Ubenefit[t]
          
        }
      }
    }
    
    
    
    
    
    
    
    #Reddito  dei lavoratori
    wage[t,1:Nwork]=ifelse(unemp[1:Nwork]==0,w[1:Nwork]*lwork[t,1:Nwork],wgov[t])
    
    
    
    
    if(t>1){
      yh[t,1:Nwork]=wage[t,1:Nwork]+moneydeposit[t-1,1:Nwork]*intratedeposit[t-1]
    }else{
      yh[t,1:Nwork]=wage[t,1:Nwork]
    }
    yhd[t,1:Nwork]=yh[t,1:Nwork]*(1-tauwork)
    
    taxpaymentworker[t]=sum(yh[t,which(codclass==0)])*tauwork
    
    
    
    if(t>1){
      StockLoanB[t]=StockLoanB[t-1]    
    }
    
    # Domanda di credito da parte delle imprese C ####
    #Il liverage desiderato (quando positivo) vale solo per il bene capitale.. per il pagamento dei salari si assume sempre pari a zero
    for(i in (NFK+1):NF){
      riservedes[t,i]=eta*stockdebtF[t,i]
      risreali[t,i]=min(riservedes[t,i],cashF[i])
      cashavailablefunding[t,i]=cashF[i]-risreali[t,i]
      loandes[t,i]=kdfeasiblenominal[t,i]*lev
      
      if(cashavailablefunding[t,i]>=wb[t,i]+kdfeasiblenominal[t,i]*(1-lev)){   
        loandemandF[t,i]=loandemandF[t,i]+loandes[t,i]
        #LoanB[t]=LoanB[t]+loandemandF[t,i]
        leveragew[t,i]=0
        leverage[t,i]=lev
      }else if(cashavailablefunding[t,i]>=wb[t,i]){
        leveragew[t,i]=0
        leverage[t,i]=(kdfeasiblenominal[t,i]-cashavailablefunding[t,i]+wb[t,i])/kdfeasiblenominal[t,i]
        loandemandF[t,i]=loandemandF[t,i]+leverage[t,i]*kdfeasiblenominal[t,i]
      }else{
        leveragew[t,i]=(wb[t,i]-cashavailablefunding[t,i])/wb[t,i]
        leverage[t,i]=1
        shortloandemandF[t,i]=leveragew[t,i]*wb[t,i]
        loandemandF[t,i]=loandemandF[t,i]+leverage[t,i]*kdfeasiblenominal[t,i]
        
      }
      loanstoricoF[t+z+de,i]=loandemandF[t,i]
      loanstoricoFtot[t+z+de,i]=loandemandF[t,i]*(1+spam2*intrate[t])
      storicoshortloandemandF[t+zs+1,i]=shortloandemandF[t,i]
      
      
      if(t==1){
        stockdebtF[t,i]=loandemandF[t,i]+shortloandemandF[t,i]
        StockLoanB[t]= StockLoanB[t]+loandemandF[t,i]+shortloandemandF[t,i]
      }else{
        stockdebtF[t,i]=stockdebtF[t-1,i]+loandemandF[t,i]+shortloandemandF[t,i]
        StockLoanB[t]=StockLoanB[t]+loandemandF[t,i]+shortloandemandF[t,i]
      }
      kdordinatofeasiblenominalstorico[t+dk,i]=kdordinatofeasible[t,i]*p[t,indexFK[t,i]]*(1+spam2*intrate[t]*leverage[t,i])
      
      
      
      #RATA FRANCESE per lo short loan (anticipo salari)
      if(intrate[t]==0){
        storicoratashort[t+zs+1,i]=shortloandemandF[t,i]/zs
      }else{
        storicoratashort[t+zs+1,i]=(shortloandemandF[t,i]*(1+intrateshort[t]*spamshort))/zs       #(shortloandemandF[t,i]*intrate[t])/(1-1/((1+intrate[t])^zs))
        if(is.nan(storicoratashort[t+zs+1,i])==TRUE){
          storicoratashort[t+zs+1,i]=0
        }
      }
    }
    
    
    # Contabilità & credito/debito consumatori ####
    
    if(t>1){
      dinero[t,(Nwork+1):N]=yhd[t-1,(Nwork+1):N]+moneydeposit[t-1,(Nwork+1):N]
      dinero[t,1:Nwork]=yhd[t,1:Nwork]+moneydeposit[t-1,1:Nwork]-ratacredittot[t,1:Nwork]  #rata credit complessiva dei tassi d'interesse
      
      effectivepaymentB[t,1:Nwork]=ratacredittot[t,1:Nwork]
      hhdebt[t,1:Nwork]=hhdebt[t-1,1:Nwork]-ratacredit[t,1:Nwork]
      creditconsumptionB[t]=creditconsumptionB[t-1]-sum(ratacredit[t,1:Nwork])
      creditinterestpayment[t,1:Nwork]=ratacredittot[t,1:Nwork]-ratacredit[t,1:Nwork]
      capitalhh[t,1:Nwork]=ratacredit[t,1:Nwork]
      crunch[t,1:Nwork]=ifelse(crunch[t-1,1:Nwork]==1 & ratacredit[t,1:Nwork]==0,0,crunch[t-1,1:Nwork])  # se finisco a pagare il debito ristritturato dopo il crunch allora torno a essere finanziato (dal periodo successivo ripspetto all'ultima rata) 
      restr[1:Nwork]=ifelse(crunch[t,1:Nwork]==1 & ratacredit[t,1:Nwork]==0,0,restr[1:Nwork])  # azzero il numero di ristrutturazioni 
      clim[1:Nwork]=ifelse((crunch[t,1:Nwork]==1 & ratacredit[t,1:Nwork]==0) | (clim[1:Nwork]>0 & ratacredit[t,1:Nwork]==0),0,clim[1:Nwork])  # azzero il numero di ristrutturazioni 
      nonperformingloanbank[t]=nonperformingloanbank[t-1]
      
    }else{
      dinero[t,1:Nwork]=yhd[t,1:Nwork]
    }
    
    # chi ha fatto il revolver il periodo precedente ma non questo, il suo clim diviene automaticamente zero
    if(t>tcredito){
      for(indexH in 1:Nwork){
        #se entra in questo loop sicuramente alla fine il suo consumo sarà zero perchè la banca non gli presta i soldi per consumare
        if(dinero[t,indexH]<0 & clim[indexH]<creditlim & crunch[t,indexH]==0){ #se può fare revolving
          newcreditdemand[t,indexH]=abs(dinero[t,indexH])
          stampa=sprintf("revolving t=%i index=%i",t,indexH)
          print(stampa)
          
          # print(t)
          # print(indexH)
          #Double entry:
          hhdebt[t,indexH]=hhdebt[t,indexH]+newcreditdemand[t,indexH]
          creditconsumptionB[t]=creditconsumptionB[t]+newcreditdemand[t,indexH]
          creditdemand[t,indexH]=creditdemand[t,indexH]+newcreditdemand[t,indexH]
          #effectivepaymentB[t,indexH]=ratacredittot[t,indexH]
          capitalhh[t,indexH]=ratacredit[t,indexH]
          ratacredittot[(t+1):(t+zcredit),indexH]=ratacredittot[(t+1):(t+zcredit),indexH]+newcreditdemand[t,indexH]/zcredit*(1+intratecredit[t]*spamcredit)
          ratacredit[(t+1):(t+zcredit),indexH]=ratacredit[(t+1):(t+zcredit),indexH]+newcreditdemand[t,indexH]/zcredit
          dinero[t,indexH]=0
          clim[indexH]=clim[indexH]+1
        }else if(dinero[t,indexH]<0 & (crunch[t,indexH]==0 | restr[indexH]<limrestr)){  #se non può fare revolving e/ deve restrutturale e/o deve diventare non performinglona
          # print("crunch1")
          # print(t)
          # print(indexH)
          stampa=sprintf("crunch1 t=%i index=%i",t,indexH)
          print(stampa)
          crunch[t,indexH]=1
          restr[indexH]=restr[indexH]+1
          effectivepaymentB[t,indexH]=ratacredittot[t,indexH]+dinero[t,indexH]
          inthh[t,indexH]=effectivepaymentB[t,indexH]*xintcredit
          capitalhh[t,indexH]=effectivepaymentB[t,indexH]-inthh[t,indexH]
          hhdebt[t,indexH]=hhdebt[t,indexH]+ratacredit[t,indexH]-capitalhh[t,indexH]
          creditconsumptionB[t]=creditconsumptionB[t]+ratacredit[t,indexH]-capitalhh[t,indexH]
          
          creditinterestpayment[t,indexH]=inthh[t,indexH]
          #ristrutturazione + ogni periodo 
          ratacredittot[(t+1):(t+zcredit),indexH]=ratacredittot[(t+1):(t+zcredit),indexH]-dinero[t,indexH]/zcredit
          ratacredit[(t+1):(t+zcredit),indexH]=ratacredit[(t+1):(t+zcredit),indexH]+(dinero[t,indexH]*(1-xintcredit))/zcredit        
          dinero[t,indexH]=0
          
        }else if(dinero[t,indexH]<0 & crunch[t,indexH]==1 & restr[indexH]>=limrestr){
          #salta tutto-> non performingloanuufficiale
          print("funghiilconsumatore")
          effectivepaymentB[t,indexH]=ratacredittot[t,indexH]+dinero[t,indexH]
          inthh[t,indexH]=effectivepaymentB[t,indexH]*xintcredit
          capitalhh[t,indexH]=effectivepaymentB[t,indexH]-inthh[t,indexH]
          hhdebt[t,indexH]=hhdebt[t,indexH]+ratacredit[t,indexH]-capitalhh[t,indexH]
          creditconsumptionB[t]=creditconsumptionB[t]+ratacredit[t,indexH]-capitalhh[t,indexH]
          
          creditinterestpayment[t,indexH]=inthh[t,indexH]
          nonperformingloanhh[t:T,indexH]=nonperformingloanhh[t:T,indexH]+hhdebt[t,indexH]
          nonperformingloanbank[t]=nonperformingloanbank[t]+hhdebt[t,indexH]
          creditconsumptionB[t]=creditconsumptionB[t]-nonperformingloanbank[t]
          hhdebt[t,indexH]=0
          dinero[t,indexH]=0
          restr[indexH]=0
          crunch[t:(t+tcrunch),indexH]=1 #in teoria da ora fino alla fine non potrà avere mai un cash inferiore a zero, perchè non paga rate e quindi non entra mani nei loop precedenti
          ratacredittot[(t+1):(t+zcredit),indexH]=0
          ratacredit[(t+1):(t+zcredit),indexH]=0
          
        }
      }
    }
    
    Bankcreditinterest[t]=sum(creditinterestpayment[t,])
    
    # #fissazione paniere path-dependenth dei lavoratori
    # if(t>2){
    #   #pastbasket[t,1:Nwork]=bas*totrealconsumption[t-1,1:Nwork]+(1-bas)*totrealconsumption_sign[t-2,1:Nwork]
    #   pastbasket[t,1:Nwork]=bas*totrealconsumption_des[t-1,1:Nwork]+(1-bas)*pastbasket[t-1,1:Nwork]
    #   
    # }else if(t==2){
    #   pastbasket[t,1:Nwork]=pastbasket[t-1,1:Nwork]
    # }else{
    #   pastbasket[t,1:Nwork]=(yhd[t,1:Nwork]*cworker)/p[1,NF]
    # }
    # 
    # # fissazione paniere path-dependenth dei capitalisti
    # if(t>3){
    #   #pastbasket[t,(Nwork+1):N]=bas*totrealconsumption[t-1,(Nwork+1):N]+(1-bas)*totrealconsumption_sign[t-2,(Nwork+1):N]
    #   pastbasket[t,(Nwork+1):N]=bas*totrealconsumption_des[t-1,(Nwork+1):N]+(1-bas)*pastbasket[t-1,(Nwork+1):N]
    #   
    # }else if(t==3){
    #   pastbasket[t,(Nwork+1):N]=totrealconsumption_des[t-1,(Nwork+1):N]
    # }else if(t==2){
    #   pastbasket[t,(Nwork+1):N]=(yhd[t-1,(Nwork+1):N]*ccap)/p[1,NF]
    # }
    # 
    
    
    # Consumption demand ######
    if(t>1){
      nominalconsumptiondemand[t,1:Nwork]=(pmin(yhd[t,1:Nwork]*cworker+V[t-1,1:Nwork]*cworkercash,yhd[t,1:Nwork]+moneydeposit[t-1,1:Nwork]))
      nominalconsumptiondemand[t,(Nwork+1):N]=pmin(yhd[t-1,(Nwork+1):N]*ccap+V[t-1,(Nwork+1):N]*ccapcash,yhd[t-1,(Nwork+1):N]+moneydeposit[t-1,(Nwork+1):N])
    }else{
      nominalconsumptiondemand[t,1:Nwork]=yhd[t,1:Nwork]*cworker
    }
    
    totrealconsdemand[t]=sum(nominalconsumptiondemand[t,])/p[t,NF]
    
    
    # spesa effettivamente realizzazta dal settore pubblico 
    
    if(TRUE){  
      for(i in (NFK+1):NF){
        realsails[t,i]=min(inv[t,i],(G[t]*alfak[t,i])/p[t,i])
        inv[t,i]=inv[t,i]-realsails[t,i]
        if(inv[t,i]==0){
          Gef[t]=Gef[t]+realsails[t,i]*p[t,i]
          Gfinito[t]=Gfinito[t]+1
        }else{
          Gef[t]=Gef[t]+realsails[t,i]*p[t,i]
        }
      }
      
    }
    Gcons[t]=Gef[t]
    Greal[t]=sum(realsails[t,which(cod==1)])
    
    
    if(t==1 & equaldistribution==1){
      Ncons=length(which(nominalconsumptiondemand[t,]>0))+Ncap
      cel=ceiling(Ncons/NFC)
      flo=floor(Ncons/NFC)
      yy=(Ncons-NFC*cel)/(flo-cel)
      xx=NFC-yy
      indexconsumo=c(rep((NFK+1):(NFK+xx),each=cel),rep((xx+NFK+1):(NFK+xx+yy),each=flo))
      
      contar=1
      for(i in 1:N){
        
        if(nominalconsumptiondemand[t,i]>0 | codclass[t,i]==1){
          indexC[t,i]=indexconsumo[contar]
          contar=contar+1
        }
        
      }
      
      
    }
    
    
    
    
    
    # Mathing mercato del consumo ####
    if(sceltastocastica==1){
      customer=which(nominalconsumptiondemand[t,]>0)
      if(length(customer)>0){
        if(length(customer)>1){
          customershuffle=sample(customer)
        }else{
          customershuffle=customer
        }
        
        for(i in 1:length(customer)){
          indexH=customershuffle[i]
          
          q=as.integer(0)
          while(q!=1){
            availablesuppliers=which(cod==1 & inv[t,]>0)
            if(length(availablesuppliers)>0){
              
              if(length(availablesuppliers)>1){
                indexC[t,indexH]=sample(availablesuppliers)[1]
              }else{
                indexC[t,indexH]=availablesuppliers
              }
              
              
              if(is.na(indexC[t,indexH])==TRUE){
                print(indexH)
                print(t)
                print("problemNA")
                stop()
              }
              realconsumptiondemand[t,indexH]=nominalconsumptiondemand[t,indexH]/p[t,indexC[t,indexH]]
              if(realconsumptiondemand[t,indexH]<=inv[t,indexC[t,indexH]]){
                realsails[t,indexC[t,indexH]]=realsails[t,indexC[t,indexH]]+realconsumptiondemand[t,indexH]
                inv[t,indexC[t,indexH]]=inv[t,indexC[t,indexH]]-realconsumptiondemand[t,indexH]
                #nominalresidualconsumptiondemand[indexH]=0
                nominalconsumptionexpenditure[t,indexH]=nominalconsumptionexpenditure[t,indexH]+realconsumptiondemand[t,indexH]*p[t,indexC[t,indexH]]
                totrealconsumption[t,indexH]=totrealconsumption[t,indexH]+realconsumptiondemand[t,indexH]
                realconsumptionmissing[t,indexH]=0
                nint[indexH]=nint[indexH]+1
                nominalconsumptiondemand[t,indexH]=0 #nominalconsumptiondemand[t,indexH]-nominalconsumptionexpenditure[t,indexH]
                #totrealconsumption_des[t,indexH]=totrealconsumption[t,indexH]
                q=1
              }else{
                realsails[t,indexC[t,indexH]]=realsails[t,indexC[t,indexH]]+inv[t,indexC[t,indexH]]
                nominalconsumptionexpenditure[t,indexH]=nominalconsumptionexpenditure[t,indexH]+inv[t,indexC[t,indexH]]*p[t,indexC[t,indexH]]
                #nominalresidualconsumptiondemand[indexH]=nominalresidualconsumptiondemand[indexH]-inv[t,indexC[t,indexH]]*p[t,indexC[t,indexH]]
                totrealconsumption[t,indexH]=totrealconsumption[t,indexH]+inv[t,indexC[t,indexH]]
                realconsumptionmissing[t,indexH]=realconsumptiondemand[t,indexH]-inv[t,indexC[t,indexH]]
                nominalconsumptiondemand[t,indexH]=nominalconsumptiondemand[t,indexH]-inv[t,indexC[t,indexH]]*p[t,indexC[t,indexH]] #nominalconsumptionexpenditure[t,indexH]
                inv[t,indexC[t,indexH]]=0 
                # inter[t,indexH]=inter[t,indexH]+1
                nint[indexH]=nint[indexH]+1
                # if(inter[t,indexH]==interazionilim){
                #   q=1
                # }
              }   
            }else{
              q=1
              
            }
          } 
        }
        
      }
      
    }else{
      customer=which(nominalconsumptiondemand[t,]>0)
      if(length(customer)>0){
        if(length(customer)>1){
          customershuffle=sample(customer)
        }else{
          customershuffle=customer
        }
        
        for(i in 1:length(customer)){
          indexH=customershuffle[i]
          
          q=as.integer(0)
          while(q!=1){
            availablesuppliers=which(cod==1 & inv[t,]>0)
            if(length(availablesuppliers)>0){
              
              if(length(availablesuppliers)>=parmatchingconsumption){
                potentialsuppliers=sample(availablesuppliers)[1:parmatchingconsumption]
                minprice=min(p[t,potentialsuppliers])
                
              }else if(length(availablesuppliers)<parmatchingconsumption & length(availablesuppliers)>1){
                potentialsuppliers=sample(availablesuppliers)
                minprice=min(p[t,potentialsuppliers])
                
              }else{
                potentialsuppliers=availablesuppliers
                minprice=p[t,potentialsuppliers[1]]
                #print(potentialsuppliers)
              }
              
              
              if(t>1 && is.na(indexC[t-1,indexH])==FALSE && inv[t,indexC[t-1,indexH]]>0 && nint[indexH]==0){
                
                if(minprice<p[t,indexC[t-1,indexH]]){
                  proba=1-e^(swic*((minprice-p[t,indexC[t-1,indexH]])/minprice))  #probabilità di cambiare fornitore
                  prob=c(proba,1-proba)
                  sol=rbinom(1, size=1, prob=prob)
                  if(sol==1){
                    indexC[t,indexH]=potentialsuppliers[p[t,potentialsuppliers]==minprice][1]
                  }else{
                    indexC[t,indexH]=indexC[t-1,indexH]
                  }
                }else{
                  indexC[t,indexH]=indexC[t-1,indexH]
                }
              }else if(t==1 && equaldistribution==1 && nint[indexH]==0){
                
                
              }else{
                indexC[t,indexH]=potentialsuppliers[p[t,potentialsuppliers]==minprice][1]
              }
              
              if(is.na(indexC[t,indexH])==TRUE){
                print(indexH)
                print(t)
                print("problemNA")
                stop()
              }
              realconsumptiondemand[t,indexH]=nominalconsumptiondemand[t,indexH]/p[t,indexC[t,indexH]]
              if(realconsumptiondemand[t,indexH]<=inv[t,indexC[t,indexH]]){
                realsails[t,indexC[t,indexH]]=realsails[t,indexC[t,indexH]]+realconsumptiondemand[t,indexH]
                inv[t,indexC[t,indexH]]=inv[t,indexC[t,indexH]]-realconsumptiondemand[t,indexH]
                #nominalresidualconsumptiondemand[indexH]=0
                nominalconsumptionexpenditure[t,indexH]=nominalconsumptionexpenditure[t,indexH]+realconsumptiondemand[t,indexH]*p[t,indexC[t,indexH]]
                totrealconsumption[t,indexH]=totrealconsumption[t,indexH]+realconsumptiondemand[t,indexH]
                realconsumptionmissing[t,indexH]=0
                nint[indexH]=nint[indexH]+1
                nominalconsumptiondemand[t,indexH]=0 #nominalconsumptiondemand[t,indexH]-nominalconsumptionexpenditure[t,indexH]
                #totrealconsumption_des[t,indexH]=totrealconsumption[t,indexH]
                q=1
              }else{
                realsails[t,indexC[t,indexH]]=realsails[t,indexC[t,indexH]]+inv[t,indexC[t,indexH]]
                nominalconsumptionexpenditure[t,indexH]=nominalconsumptionexpenditure[t,indexH]+inv[t,indexC[t,indexH]]*p[t,indexC[t,indexH]]
                #nominalresidualconsumptiondemand[indexH]=nominalresidualconsumptiondemand[indexH]-inv[t,indexC[t,indexH]]*p[t,indexC[t,indexH]]
                totrealconsumption[t,indexH]=totrealconsumption[t,indexH]+inv[t,indexC[t,indexH]]
                realconsumptionmissing[t,indexH]=realconsumptiondemand[t,indexH]-inv[t,indexC[t,indexH]]
                nominalconsumptiondemand[t,indexH]=nominalconsumptiondemand[t,indexH]-inv[t,indexC[t,indexH]]*p[t,indexC[t,indexH]] #nominalconsumptionexpenditure[t,indexH]
                inv[t,indexC[t,indexH]]=0 
                # inter[t,indexH]=inter[t,indexH]+1
                nint[indexH]=nint[indexH]+1
                # if(inter[t,indexH]==interazionilim){
                #   q=1
                # }
              }   
            }else{
              q=1
              
            }
          } 
        }
        
      }
    }
    
    #caloclo degli introiti delle imprese C
    revenues[t,(NFK+1):NF]=realsails[t,(NFK+1):NF]*p[t,(NFK+1):NF]
    
    unsatisfieddemand[t]=sum(realconsumptionmissing[t,]) #Greal[t]+sum(realconsumptiondemand[t,])-sum(realsails[t,which(cod==1)])
    #totmissing[t]=sum(realconsumptionmissing[t,])
    
    
    # Controllo matching
    if(round(sum(realconsumptionmissing[t,]),4)>0 & sum(inv[t,which(cod==1)])>0){
      print("problem matching consumption")
      errortrial=numtrial
      save.image("/nobackup/bnldd/errormathccons.RData")    
      stop()
    }
    
    #check
    if(isFALSE(all.equal(creditconsumptionB[t],sum(hhdebt[t,])))){
      print("SFC problem credit")
      stop()
    }
    if(nonperformingloanbank[t]!=sum(nonperformingloanhh[t,])){
      print("SFC proble credit2")
    }
    
    # andamento storico delle vendite con pesi decrescenti + consumo storico
    # if(t==2){
    #   demandperfirmconsumer_sign[t,(NFK+1):NF]=realsails[t,(NFK+1):NF]*beta+(1-beta)*realsails[t-1,(NFK+1):NF]
    #   #totrealconsumption_sign[t,1:Nwork]=totrealconsumption[t,1:Nwork]*bas+(1-bas)*totrealconsumption[t-1,1:Nwork]
    #   # totrealconsumption_sign[t,1:Nwork]=totrealconsumption_des[t,1:Nwork]*bas+(1-bas)*totrealconsumption_des[t-1,1:Nwork]
    #   
    # }else if(t>2){
    #   demandperfirmconsumer_sign[t,(NFK+1):NF]=realsails[t,(NFK+1):NF]*beta+(1-beta)*demandperfirmconsumer_sign[t-1,(NFK+1):NF]
    #   #totrealconsumption_sign[t,1:Nwork]=totrealconsumption[t,1:Nwork]*bas+(1-bas)*totrealconsumption_sign[t-1,1:Nwork]
    #   #totrealconsumption_sign[t,1:Nwork]=totrealconsumption_des[t,1:Nwork]*bas+(1-bas)*totrealconsumption_sign[t-1,1:Nwork]
    # }else{
    #   demandperfirmconsumer_sign[t,(NFK+1):NF]=realsails[t,(NFK+1):NF]
    #   #totrealconsumption_sign[t,1:Nwork]=totrealconsumption[t,1:Nwork]
    #   #totrealconsumption_sign[t,1:Nwork]=totrealconsumption_des[t,1:Nwork]
    #   
    # }
    
    
    
    
    
    
    #aggiornamento dei contatori sui periodi consecutivi in cui le scorte sono state diverse dal desiderato (serve per la fissazione dei markup)
    if(TRUE){ 
      for(i in (NFK+1):NF){
        
        if(is.nan(inv[t,i]/realsails[t,i])==TRUE){
          invlimsup[t,i]=0 #invlimsup[t-1,i]
          invliminf[t,i]=0 #invlimsup[t-1,i]
        }else{
          if((inv[t,i]/realsails[t,i])<sigma*(1-flexsigma)){
            if(t==1){
              invlimsup[t,i]=1
              invliminf[t,i]=0 
            }else{  
              invlimsup[t,i]=invlimsup[t,i]+1
              invliminf[t,i]=0
            }
          }else{
            if(t==1){
              invliminf[t,i]=1
              invlimsup[t,i]=0 
            }else{
              invliminf[t,i]=invliminf[t,i]+1
              invlimsup[t,i]=0
            }
          }
        } 
      }
    }
    
    
    
    
    # Aggiornamento depositi #########    
    if(t>1){
      # moneydeposit[t,(Nwork+1):N]=moneydeposit[t-1,(Nwork+1):N]+yhd[t-1,(Nwork+1):N]-nominalconsumptionexpenditure[t,(Nwork+1):N]
      moneydeposit[t,1:Nwork]=moneydeposit[t-1,1:Nwork]+yhd[t,1:Nwork]-nominalconsumptionexpenditure[t,1:Nwork]+creditdemand[t,1:Nwork]
      V[t,1:Nwork]=moneydeposit[t,1:Nwork]-hhdebt[t,1:Nwork]
      
      BondRepayed[t,(Nwork+1):N]=BondDemand[t-1,(Nwork+1):N]  #momentaneamente ho messo che durano un periodo
      Liquidityend[t,(Nwork+1):N]=moneydeposit[t-1,(Nwork+1):N]+BondRepayed[t,(Nwork+1):N]+yhd[t-1,(Nwork+1):N]-nominalconsumptionexpenditure[t,(Nwork+1):N]
      V[t,(Nwork+1):N]=Liquidityend[t,(Nwork+1):N]
      
    }else{
      moneydeposit[t,1:Nwork]=yhd[t,1:Nwork]-nominalconsumptionexpenditure[t,1:Nwork]
      V[t,1:Nwork]=moneydeposit[t,1:Nwork]
    }
    
    if(length(which(round(moneydeposit[t,1:N],3)<0))>0){
      print("problemnegativedeposit")
      print(t)
      ss=which(moneydeposit[t,1:N]<0)
      print(ss)
      print(moneydeposit[t,ss])
    }
    
    
    
    
    # for(i in (NFK+1):NF){
    #   
    #   revenues[t,i]=realsails[t,i]*p[t,i]
    #   if(revenues[t,i]<wb[t,i]){
    #     #print("problemC")
    #   }
    #   
    # }
    
    
    
    
    #pagamento rimborso debito (per ora non stiamo considerando gli interessi sul debito, infatti non entrano nei prezzi)
    #servicedebtF[]=0
    #rata alla francese (costante: rimborso capitale crescente e pagamento interessi decrescente..perchè si fa sul capitale residuo)
    # for(i in 1:NFK){
    #   # contare=1/z
    #   # for(j in t:(t+z-1)){
    #   #   if (loanstoricoF[j,i]>0){
    #   #     servicedebtFtot[t,i]=servicedebtFtot[t,i]+storicorata[j,i]
    #   #     interestpaymentF[t,i]=interestpaymentF[t,i]+(storicorata[j,i]-loanstoricoF[j,i]/z)
    #   #   }
    #   #   contare=contare+1/z
    #   # }
    #   # 
    #   # servicedebtF[t,i]=servicedebtFtot[t,i]-interestpaymentF[t,i]
    #   # 
    #   
    #   cashF[i]=cashF[i]+revenues[t,i]-wb[t,i]-servicedebtFtot[t,i] 
    #   if(round(cashF[i],4)<0){
    #     print("problemcashFk")
    #     print(cashF[i])
    #   }
    #   #A causa del mercato del lavoro unitario posso avere che produco iu della domanda quindi i ricavi sono minori dei salari pagati   
    #   # 
    #   # if(cashF[i]>=0){
    #   #   if(t>1){
    #   #     stockdebtF[t,i]=stockdebtF[t-1,i]-servicedebtF[t,i]
    #   #   }
    #   #   StockLoanB[t]=StockLoanB[t]-servicedebtF[t,i]
    #   # }else{
    #   #   loandemandF[t,i]=-cashF[i] 
    #   #   loanstoricoF[t+z,i]=loandemandF[t,i]
    #   #   StockLoanB[t]=StockLoanB[t]-servicedebtF[t,i]+loandemandF[t,i]
    #   #   if(t==1){
    #   #     stockdebtF[t,i]=loandemandF[t,i]
    #   #   }else{
    #   #     stockdebtF[t,i]=stockdebtF[t-1,i]+loandemandF[t,i]-servicedebtF[t,i]
    #   #   }
    #   #   
    #   #   cashF[i]=0
    #   #   if(intrate[t]==0){
    #   #     storicorata[t+z,i]=loandemandF[t,i]/z
    #   #   }else{
    #   #     storicorata[t+z,i]=(loandemandF[t,i]*intrate[t])/(1-1/((1+intrate[t])^z))
    #   #     if(is.nan(storicorata[t+z,i])==TRUE){
    #   #       storicorata[t+z,i]=0
    #   #     }
    #   #   }
    #   # }
    #   
    #   
    #   
    # }
    
    
    cashF[1:NFK]=cashF[1:NFK]+revenues[t,1:NFK]-wb[t,1:NFK]-servicedebtFtot[t,1:NFK] 
    #   if(round(cashF[i],4)<0){
    #     print("problemcashFk")
    #     print(cashF[i])
    #   }
    
    
    
    
    # Calcolo del servizio del debito totale delle imprese C ######
    
    # inizio a pagare la rata  nel periodo stesso in cui lo contraggo (perché viene fatto ad inizo periodo)
    for(i in (NFK+1):NF){
      
      #contare=1/zs
      for(j in (t+1):(t+zs)){
        if (storicoratashort[j,i]>0){
          servicedebtFtot[t,i]=servicedebtFtot[t,i]+storicoratashort[j,i]
          interestpaymentF[t,i]=interestpaymentF[t,i]+(storicoratashort[j,i]-storicoshortloandemandF[j,i]/zs)
        }
        # contare=contare+1/zs
      }
      
      #RATA con evoluzione ammortamento + SHORT TERM LOAN per pagamento salari
      
      #Sistema con quote di rimborso decrescenti (coerenti con l'ammortamento)
      g=1/z
      for(j in (t+1):(t+z)){
        if(loanstoricoF[j,i]>0){
          servicedebtFtot[t,i]=servicedebtFtot[t,i]+loanstoricoFtot[j,i]*g/spam
          interestpaymentF[t,i]=interestpaymentF[t,i]+(loanstoricoFtot[j,i]-loanstoricoF[j,i])*g/spam
        }
        g=g+1/z
      }
      
      contare=1/zp
      
      for(j in (t+1):(t+zp)){
        if(storicooverdtraft[j,i]>0){
          servicedebtFtot[t,i]=servicedebtFtot[t,i]+storicorataoverdraft[j,i]
          interestpaymentF[t,i]=interestpaymentF[t,i]+(storicorataoverdraft[j,i]-storicooverdtraft[j,i]/zp)
        }
        contare=contare+1/zp
      }
      
      #servicedebtFtot[t,i]=servicedebtFtot[t,i]+shortloandemandF[t,i]*(1+intrate[t])
      #interestpaymentF[t,i]=interestpaymentF[t,i]+shortloandemandF[t,i]*intrate[t]
      servicedebtF[t,i]=servicedebtFtot[t,i]-interestpaymentF[t,i]
      
      # calcolo cassa dell'impresa
      cashF[i]=cashF[i]+loandemandF[t,i]+shortloandemandF[t,i]+revenues[t,i]-wb[t,i]-kdfeasiblenominal[t,i]-servicedebtFtot[t,i]  #-ponzis[(t-1),i]                                                                                                         
      
      
      #Calcolo del valore residuo del capitale (serve per il calcolo della ricchezza netta delle imprese)
      counting=0
      f=1
      for(j in (t+2):(t+z)){
        counting=counting+f*(1/z)
        f=f+1
        ammresiduo[t,i]= ammresiduo[t,i]+storicoIN[j,i]*counting/spam
      }
      
      
      
      #valutazione situazione finanziaria dell' impresa: nel caso in cui è insolvente si verifica se ha i requisiti per chiedere un ulteriore prestito altrimenti va in bancarotta
      if(cashF[i]>=0){
        stockdebtF[t,i]=stockdebtF[t,i]-servicedebtF[t,i]
        StockLoanB[t]=StockLoanB[t]-servicedebtF[t,i]
        nponzi[i]=0
      }else{
        # print("overdraft")
        #print(t)
        #overdraftdemand[t,i]=abs(cashF[i])
        #ponzis[t,i]=overdraftdemand[t,i]
        #storicooverdtraft[t+zp,i]=overdraftdemand[t,i]
        
        #if(intrateoverdraft[t]==0){
        # storicorataoverdraft[t+zp,i]=overdraftdemand[t,i]/zp
        #}else{
        #  storicorataoverdraft[t+zp,i]=(overdraftdemand[t,i]*(1+intrate[t]*spamoverdraft))/zp        #(overdraftdemand[t,i]*intrateoverdraft[t])/(1-1/((1+intrateoverdraft[t])^zp))
        #  if(is.nan(storicorataoverdraft[t+zp,i])==TRUE){
        #    storicorataoverdraft[t+zp,i]=0
        #  }
        #}
        
        #  cashF[i]=0  #cosi sono riuscito effettivametne a sostenere il servizio al debito
        # stockdebtF[t,i]= stockdebtF[t,i]-servicedebtF[t,i]+overdraftdemand[t,i]
        #  StockLoanB[t]=StockLoanB[t]-servicedebtF[t,i]+overdraftdemand[t,i]
        
        
        #calcolao la ricchezza netta che avrei prima di pagare il servizio al debito, che comunque non riesco a pagare
        NW[t,i]=ammresiduo[t,i]+(cashF[i]+servicedebtFtot[t,i])-stockdebtF[t,i]+inv[t,i]*unitcost[t,i]
        Residualvalue[t,i]=ammresiduo[t,i]+inv[t,i]*unitcost[t,i]
        
        if(t<startingponzi | NW[t,i]>0){
          # print("ponzi")
          
          overdraftdemand[t,i]=abs(cashF[i])
          ponzis[t,i]=overdraftdemand[t,i]
          storicooverdtraft[t+zp+1,i]=overdraftdemand[t,i]
          
          if(intrate[t]==0){
            storicorataoverdraft[t+zp+1,i]=overdraftdemand[t,i]/zp
          }else{
            storicorataoverdraft[t+zp+1,i]=(overdraftdemand[t,i]*(1+intrateoverdraft[t]*spamoverdraft))/zp       #(overdraftdemand[t,i]*intrate[t])/(1-1/((1+intrate[t])^zp))
            if(is.nan(storicorataoverdraft[t+zp+1,i])==TRUE){
              storicorataoverdraft[t+zp+1,i]=0
            }
          }
          
          #loanstoricoF[t+z+1,i]=loandemandF[t+1,i]
          cashF[i]=0  #cosi sono riuscito effettivametne a sostenere il servizio al debito
          stockdebtF[t,i]= stockdebtF[t,i]-servicedebtF[t,i]+overdraftdemand[t,i]
          StockLoanB[t]=StockLoanB[t]-servicedebtF[t,i]+overdraftdemand[t,i]
          if(t>=startingponzi){
            ponzi[t,i]="ponzi"
            nponzi[i]=nponzi[i]+1
          }
          
        }else{
          
          if(nponzi[i]<limponzi){
            # stampa=sprintf("ponzi2 t=%i index=%i",t,i)
            # print(stampa)
            
            overdraftdemand[t,i]=abs(cashF[i])
            ponzis[t,i]=overdraftdemand[t,i]
            #loanstoricoF[t+z+1,i]=loandemandF[t+1,i]
            storicooverdtraft[t+zp+1,i]=overdraftdemand[t,i]
            if(intrate[t]==0){
              storicorataoverdraft[t+zp+1,i]=overdraftdemand[t,i]/zp
            }else{
              storicorataoverdraft[t+zp+1,i]=(overdraftdemand[t,i]*(1+intrateoverdraft[t]*spamoverdraft))/zp       #(overdraftdemand[t,i]*intrate[t])/(1-1/((1+intrate[t])^zp))
              if(is.nan(storicorataoverdraft[t+zp+1,i])==TRUE){
                storicorataoverdraft[t+zp+1,i]=0
              }
            }
            cashF[i]=0
            ponzi[t,i]="ponzi"
            nponzi[i]=nponzi[i]+1
            stockdebtF[t,i]= stockdebtF[t,i]-servicedebtF[t,i]+overdraftdemand[t,i]
            StockLoanB[t]=StockLoanB[t]-servicedebtF[t,i]+overdraftdemand[t,i]
            
          }else{
            
            # L'impresa fallisce
            
            
            #if(nbancarottac==0){
            #stampa=sprintf("bancarotta t=%f index=%f cashF=%f",t,i,cashF[i])
            # print(stampa)
            # print("bancarotta")
            # print(t)
            # print(i)
            # print(cashF[i])
            #}
            
            bancarotta[t,i]=1
            nbancarottac=nbancarottac+1
            #print(i)
            #print(t)
            cashF[i]=cashF[i]+servicedebtFtot[t,i]
            interestpaymentF[t,i]=0
            servicedebtFtot[t,i]=0
            servicedebtF[t,i]=0
            restituzioneparziale[t,i]=min(cashF[i],stockdebtF[t,i])   #inanzitutto restituisco parte del debito
            cashF[i]=cashF[i]-restituzioneparziale[t,i]
            if(cashF[i]!=0){
              print("problemaCashF2")
            }
            stockdebtF[t,i]=stockdebtF[t,i]-restituzioneparziale[t,i]
            StockLoanB[t]=StockLoanB[t]-restituzioneparziale[t,i]
            
            
            # Calcolo della perdita della banca e dei soldi reimmessi dal capitalista proprietario dell'impresa (al massimo uguali al valore residuo del capitale e delle scorte)
            if(moneydeposit[t,which(capitalistindexF==i)]>=Residualvalue[t,i]){
              moneydeposit[t,which(capitalistindexF==i)]=moneydeposit[t,which(capitalistindexF==i)]-Residualvalue[t,i]
              dissavingcap[t,which(capitalistindexF==i)]=Residualvalue[t,i]
              if(Residualvalue[t,i]>=stockdebtF[t,i]){
                servicericapital[t,i]=stockdebtF[t,i]
                cashF[i]=Residualvalue[t,i]-servicericapital[t,i]
                StockLoanB[t]=StockLoanB[t]-stockdebtF[t,i]
                stockdebtF[t,i]=0   #questo caso non puo accadere (lo posso togliere), perche sarebbe il caso con ricchezza netta positiva ( equindi, l 'impresa anche se insolvente avrebbe potuto richiedere un ulteriore prestito)
              }else{
                servicericapital[t,i]=Residualvalue[t,i]
                StockLoanB[t]=StockLoanB[t]-stockdebtF[t,i]
                PerditaB[t]=PerditaB[t]+stockdebtF[t,i]-servicericapital[t,i]
                stockdebtF[t,i]=0
                cashF[i]=0
              }
            }else{
              #a questo punto il capitalista mette tutti i soldi che ha, anche se inferiori al valore del capitale residuo
              #se sufficiente paga il debito rimasto
              dissavingcap[t,which(capitalistindexF==i)]=moneydeposit[t,which(capitalistindexF==i)]
              if(moneydeposit[t,which(capitalistindexF==i)]>=stockdebtF[t,i]){
                servicericapital[t,i]=stockdebtF[t,i]
                StockLoanB[t]=StockLoanB[t]-stockdebtF[t,i]
                cashF[i]=moneydeposit[t,which(capitalistindexF==i)]-stockdebtF[t,i]
                moneydeposit[t,which(capitalistindexF==i)]=0
                stockdebtF[t,i]=0 ##non puo mai accadere
              }else{
                servicericapital[t,i]=moneydeposit[t,which(capitalistindexF==i)]
                StockLoanB[t]=StockLoanB[t]-stockdebtF[t,i]
                PerditaB[t]=PerditaB[t]+stockdebtF[t,i]-moneydeposit[t,which(capitalistindexF==i)]
                moneydeposit[t,which(capitalistindexF==i)]=0
                cashF[i]=0
              }
            }
            
            #AZZERO LA STORIA RIMANENTE DELLE RATE DEI DEBITI CHE AVREI DOVUTO PAGARE IN FUTURO (questa impresa viene sostituita nel prossimo periodo con una che parte da 0..rimane solo il capitale )
            nponzi[i]=0
            stockdebtF[t,i]=0
            loanstoricoF[,i]=0
            storicoratashort[,i]=0
            loanstoricoFtot[,i]=0
            storicooverdtraft[,i]=0
            storicorataoverdraft[,i]=0
            # for(j in (t+2):(min(t+z,T+z+dk))){
            #   loanstoricoF[j,i]=0
            #   #storicorata[j,i]=0
            #   storicoratashort[j,i]=0
            #   loanstoricoFtot[j,i]=0
            # }
            # for(j in (t+1):(t+zp-1)){
            #   storicooverdtraft[j,i]=0
            #   storicorataoverdraft[j,i]=0
            # }
          }
          
        }
        
      }
    }
    
    
    currentcost[1:NFK]=wb[t,1:NFK] 
    currentcost[(NFK+1):NF]=ammortamento[t,(NFK+1):NF]+wb[t,(NFK+1):NF]*(1+intrateshort[t]*leveragew[t,(NFK+1):NF]*spamshort)
    
    #computing cost
    # for(i in 1:NF){
    #   if(i<=NFK){
    #     currentcost[i]=wb[t,i]    #indirettamente quest isalari sono anticipati dal settore C, che paga in anticipo il bene capitale
    #   }else{
    #     currentcost[i]=ammortamento[t,i]+wb[t,i]*(1+intrateshort[t]*leveragew[t,i]*spamshort)
    #   }
    # }
    
    
    
    #Profitti  ####
    if(FALSE){
      for(i in 1:NF){
        if(i<=NFK){
          #profit[t,i]=max(0,revenues[t,i]-(wageExogenous*realsails[t,i]/phi)) #questa Ã¨ giusta se non variano i salari
          for(j in (t+1):(t+dk)){
            contrevenues[t,i]=contrevenues[t,i]+storicovaluesails[j,i]
            contsails[t,i]=contsails[t,i]+storicosales[j,i]
          }
          
          profit[t,i]=max(0,contrevenues[t,i]-(wageaggregate[t]*contsails[t,i]/phi))  ##vale solo se i salari non variano nel tempo
        }else{
          profit[t,i]=max(0,revenues[t,i]-currentcost[i])
        }
        profit_fe[t,i]=max(0,profit[t,i])
        #in teoria l'impresa potrebbe distribuire piÃ¹ profitti perchÃ¨ in cassa ha un moneta maggiore dei soldi per il fatto che come uscita ha solo una parte del moent salari che dovrebbe pagare, perchÃ¨ l'indebitamento lo paga a pezzi
        if(bancarotta[t,i]==0){  #se l'imrpesa non e andata in bancarotta distribuisce i dividendi, questa consdizione non e necessariamente superflua
          div[t,i]=min(cashF[i],profit_fe[t,i]*teta)
          #div[t,i]=min(cashF[i],profit_fe[t,i])
          divpercap[i]=div[t,i]/capperfirm
          #y_cap[t]=y_cap[t]+divpercap[i]
          yh[t,which(capitalistindexF==i)]=divpercap[i]
          cashF[i]=cashF[i]-div[t,i]
        }
      }
    }
    
    if(TRUE){
      for(j in (t+1):(t+dk)){
        contrevenues[t,1:NFK]=contrevenues[t,1:NFK]+storicovaluesails[j,1:NFK]
        contsails[t,1:NFK]=contsails[t,1:NFK]+storicosales[j,1:NFK]
      }
      
      profit[t,1:NFK]=pmax(0,contrevenues[t,1:NFK]-(wageaggregate[t]*contsails[t,1:NFK]/phi))  ##vale solo se i salari non variano nel tempo
      profit[t,(NFK+1):NF]=pmax(0,revenues[t,(NFK+1):NF]-currentcost[(NFK+1):NF])
      profit_fe[t,1:NF]=pmax(0,profit[t,1:NF])
      
      div[t,1:NF]=ifelse(bancarotta[t,1:NF]==0,pmin(cashF[1:NF],profit_fe[t,1:NF]*teta),0)
      #div[t,i]=min(cashF[i],profit_fe[t,i])
      divpercap[1:NF]=ifelse(bancarotta[t,1:NF]==0,div[t,1:NF]/capperfirm,0)
      #y_cap[t]=y_cap[t]+divpercap[i]
      cashF[1:NF]=cashF[1:NF]-div[t,1:NF]
      
      
      fun4<-function(x){
        return(div[t,capitalistindexF[x]])
      }
      yh[t,(Nwork+1):N]=sapply((Nwork+1):N,fun4)
    }
    
    
    
    
    # Calcolo di tutti i flussi finanziari
    if(t>1){
      interestdepositworker[t]=sum(moneydeposit[t-1,which(codclass==0)])*intratedeposit[t-1]  #oppure Moneydeposit_worker[t]*intratedeposit[t]
      interestdepositpayB[t]=interestdepositworker[t]+intratedeposit[t-1]*sum(moneydeposit[t-1,which(codclass==1)])+Advances[t-1]*advanceinterest[t-1]     #Moneydeposit_worker[t-1]*intratedeposit[t-1]
      profitB[t]=sum(interestpaymentF[t,])-interestdepositpayB[t]+Hb2[t-1]*intratedeposit[t-1]+Bankcreditinterest[t]
    }else{
      profitB[t]=sum(interestpaymentF[t,])-interestdepositpayB[t] 
    }
    
    if(t>1){
      cashB[t]=cashB[t-1]+profitB[t]
    }else{
      cashB[t]=profitB[t]
    }
    
    if(round(cashB[t],4)<0){
      # print("problemBank")
      # print(t)
      AdvB[t]=-profitB[t]
      #AdvB[t]=max(-profitB[t]-Hb2[t-1],0)
      #deltaRes[t]=min(-profitB[t],Hb2[t-1])
      profitB[t]=0
      cashB[t]=0
    }
    
    divB[t]=min(cashB[t],profitB[t])
    cashB[t]=cashB[t]-divB[t]
    
    divpercapB[t]=divB[t]/Ncap
    
    #si ipotizza che i profitti della banca siano distribuiti equamente tra i capitalisti che possiedono anche le imprese)
    
    if(TRUE){
      
      if(t>1){
        yh[t,(Nwork+1):N]=yh[t,(Nwork+1):N]+divpercapB[t]+moneydeposit[t-1,(Nwork+1):N]*intratedeposit[t-1]+bondinterest[t-1]*BondDemand[t-1,(Nwork+1):N]
        yhd[t,(Nwork+1):N]=yh[t,(Nwork+1):N]*(1-taucap)
        # V[t,(Nwork+1):N]=moneydeposit[t,(Nwork+1):N]+BondDemand[t-1,(Nwork+1):N]
        # BondRepayed[t,(Nwork+1):N]=BondDemand[t-1,(Nwork+1):N]  #momentaneamente ho messo che durano un periodo
        # Liquidityend[t,(Nwork+1):N]=moneydeposit[t,(Nwork+1):N]+BondRepayed[t,(Nwork+1):N]
      }else{
        yh[t,(Nwork+1):N]=yh[t,(Nwork+1):N]+divpercapB[t]
        yhd[t,(Nwork+1):N]=yh[t,(Nwork+1):N]*(1-taucap)
      }
      
      
      
      # for(i in (Nwork+1):N){
      #   
      #   if(t>1){
      #   yh[t,i]=yh[t,i]+divpercapB[t]+moneydeposit[t-1,i]*intratedeposit[t-1]+bondinterest[t-1]*BondDemand[t-1,i]
      #   yhd[t,i]=yh[t,i]*(1-taucap)
      #   V[t,i]=moneydeposit[t,i]+BondDemand[t-1,i]
      #   BondRepayed[t,i]=BondDemand[t-1,i]  #momentaneamente ho messo che durano un periodo
      #   Liquidityend[t,i]=moneydeposit[t,i]+BondRepayed[t,i]
      #   }else{
      #   yh[t,i]=yh[t,i]+divpercapB[t]
      #   yhd[t,i]=yh[t,i]*(1-taucap)
      #   }
      # }
    }
    
    
    # REDDITI DEI CAPITALISTI #
    #divB[t]=max(0,profitB[t])
    if(FALSE){
      if(t>1){
        interestBond_cap[t]=bondinterest[t-1]*BondDemand_cap[t-1]  #va cambiato con lo stock al posto del flusso se considero long term bond
        y_cap[t]=y_cap[t]+divB[t]+intratedeposit[t-1]*Moneydeposit_cap[t-1]+interestBond_cap[t]
        
      }else{
        y_cap[t]= y_cap[t]+divB[t]
      }
      y_hcap[t]=y_cap[t]*(1-taucap)
      
    }   
    
    taxpaymentcap[t]=sum(yh[t,which(codclass==1)])*taucap
    Tax[t]=taxpaymentworker[t]+taxpaymentcap[t]
    
    
    
    #cALCOLO Net Wealth dell'impresa dopo la distribuzione dei profitti
    # 
    NW[t,1:NFK]=cashF[1:NFK]-stockdebtF[t,1:NFK] #se considero le inventories dovrei differenziare tra le inventoires prodotto perche  produco piu del desiderato e le inventories come working progess, per evitare problemi non le metto 
    valueinv[t,(NFK+1):NF]=inv[t,(NFK+1):NF]*unitcost[t,(NFK+1):NF]
    NW[t,(NFK+1):NF]=ammresiduo[t,(NFK+1):NF]+cashF[(NFK+1):NF]-stockdebtF[t,(NFK+1):NF]+valueinv[t,(NFK+1):NF]
    
    # 
    # for(i in 1:NF){
    #  if(i<=NFK){
    #     NW[t,i]=cashF[i]-stockdebtF[t,i] #se considero le inventories dovrei differenziare tra le inventoires prodotto perche  produco piu del desiderato e le inventories come working progess, per evitare problemi non le metto 
    #   }else{
    #     valueinv[t,i]=inv[t,i]*unitcost[t,i]
    #     NW[t,i]=ammresiduo[t,i]+cashF[i]-stockdebtF[t,i]+valueinv[t,i]
    #   }
    # }
    # 
    
    
    #modificare: mettere il bail out    
    
    
    
    
    
    
    #####BILANCIO PUBBLICO
    Gef[t]=Gef[t]+Ubenefit[t]
    Gtot[t]=Gef[t]+interestpaymentG[t]
    
    
    deficitG[t]=Gef[t]-Tax[t]-ProfitCB[t]+interestpaymentG[t]
    saldoprimario[t]=Gef[t]-Tax[t]
    saldosecondario[t]=Gef[t]-Tax[t]-ProfitCB[t]+interestpaymentG[t]
    
    
    if(t>1){
      cashG[t]=cashG[t-1]+Tax[t]-Gef[t]-bondrepayment[t]-interestpaymentG[t]+ProfitCB[t]
    }else{
      cashG[t]=Tax[t]-Gef[t]-bondrepayment[t]-interestpaymentG[t]+ProfitCB[t]
    }
    #Bond Government Supply
    if(cashG[t]<0){
      bond[t]=-cashG[t]
      bondstorico[t+tpub]=bond[t]
      bondscadenza[t+tpub]= bond[t]
    }
    cashG[t]=cashG[t]+bond[t]
    if(t>1){
      stockdebtG[t]=stockdebtG[t-1]+bond[t]-bondrepayment[t]
    }else{
      stockdebtG[t]=bond[t]
    }
    
    
    # Ricchezza privata
    #if(t>1){
    #V_cap[t]=y_hcap[t-1]-Cef_cap[t]+BondDemand_cap[t-1]+Moneydeposit_cap[t-1]
    #BondRepayed_cap[t]=BondDemand_cap[t-1]  #momentaneamente ho messo che durano un periodo
    #Liquidityend_cap[t]=y_hcap[t-1]-Cef_cap[t]+Moneydeposit_cap[t-1]+BondRepayed_cap[t]
    #}
    #if(V_cap[t]<0){
    #  print("problemVcap")
    #}
    
    
    #if(t>1){
    # V_worker[t]=y_hw[t]-Cef_w[t]+Moneydeposit_worker[t-1]
    #Liquidityend_worker[t]=y_hw[t]-Cef_w[t]+Moneydeposit_worker[t-1]
    #}else{
    #V_worker[t]=y_hw[t]-Cef_w[t]
    #Liquidityend_worker[t]=y_hw[t]-Cef_w[t]
    #}
    #if(V_worker[t]<0){
    #  print("problemVworker")
    #  print(t)
    #}
    #domanda di bond dei capitalsti, con tasso d interesse esogeno e residuo acquistato dalla BC
    
    
    # domanda di bond dei capitalisti
    bondtoprivate[t]=bond[t]
    for(i in (Nwork+1):N){
      if(t>1){
        BondDemand[t,i]=max(0,l_o*V[t,i]+l_uno*bondinterest[t]*V[t,i]-l_due*yhd[t-1,i])  #oppure yhd[t,i]?
        BondDemand[t,i]=min(BondDemand[t,i],Liquidityend[t,i])  ##serve con i bond a lungo termine in cui la ricchezza non e' tutta liquida e quindi potrei avere nei vincoli per acquistare l ammontare desiderato
        #if(BondDemand_cap[t]>bond[t]){
        #  print("problemcalibrationbonddemand")
        #  print(t)
        #}
        
        BondDemand[t,i]=min(BondDemand[t,i],bondtoprivate[t])
        bondtoprivate[t]=bondtoprivate[t]-BondDemand[t,i]
        moneydeposit[t,i]=Liquidityend[t,i]-BondDemand[t,i]
      }
    }
    
    #  BondDemand[t,(Nwork+1):N]=pmax(0,l_o*V[t,(Nwork+1):N]+l_uno*bondinterest[t]*V[t,(Nwork+1):N]-l_due*yhd[t-1,(Nwork+1):N])
    
    Moneydeposit_worker[t]=sum(moneydeposit[t,which(codclass[]==0)])  
    Moneydeposit_cap[t]=sum(moneydeposit[t,which(codclass[]==1)])
    cashHwmoment[t]=Moneydeposit_worker[t]
    cashHcapmoment[t]=Moneydeposit_cap[t]+sum(yhd[t,which(codclass[]==1)])
    
    
    # Contabilità SFC ######
    #La banca Centrale rifinanzia la Banca per l ammontare perso, la banca prende questo ammontare e lo mette nelle riserve della bance entrale( le concede un prestito)
    deltaAdvances[t]=PerditaB[t]+AdvB[t]
    if(t>1){
      Advances[t]=Advances[t-1]+deltaAdvances[t]     #sum(stockdebtF[t,])-Deposit[t]
      deltanonperformingcredit[t]=nonperformingloanbank[t]-nonperformingloanbank[t-1]
    }else{
      Advances[t]=deltaAdvances[t]
      deltanonperformingcredit[t]=nonperformingloanbank[t]
    }
    #la baca mette a riserva tali advances  -> AUMENTO LO STOCK DI PRESTITI DETENUTO( CHE TRORNA AL LIVELLO PREFALLIMENTO)
    #StockLoanB[t]=StockLoanB[t]+deltaAdvances[t]
    # if(t>1){
    #  ReservesCB[t]=ReservesCB[t-1]+deltaAdvances[t]
    #}else{
    #  ReservesCB[t]=deltaAdvances[t]
    #}
    LoanBDemand[t]=sum(loandemandF[t,])+sum(overdraftdemand[t,])+sum(shortloandemandF[t,])
    
    Deposit[t]=sum(cashF)+cashHcapmoment[t]+cashHwmoment[t]+cashG[t]
    Deposit2[t]=Moneydeposit_cap[t]+Moneydeposit_worker[t]
    
    
    
    
    if(t>1){
      deltaDeposit[t]=Deposit[t]-Deposit[t-1]
      deltaDeposit2[t]=Deposit2[t]-Deposit2[t-1]
    }else{
      deltaDeposit[t]=Deposit[t]
      deltaDeposit2[t]=Deposit2[t]
    }
    
    
    if(t>1){
      if((round(Deposit[t],4))<=(round(StockLoanB[t]+creditconsumptionB[t],4))){
        
        #print("advances alarm")
        #print(t)
        #print(t)
        #Advances[t]=Advances[t-1]+loandemandF[t]-servicedebtF[t]-deltaDeposit[t]
      }else{
        
        Hb[t]=round(Hb[t-1]+deltaDeposit[t]-(LoanBDemand[t]+sum(creditdemand[t,])-sum(capitalhh[t,])-sum(servicedebtF[t,])-sum(restituzioneparziale[t,])-sum(servicericapital[t,])-PerditaB[t]-deltaRes[t]),4)
        #Hb[t]= Hb[t-1]+deltaDeposit[t]-(loandemandF[t]-servicedebtF[t])
        #Hb[t]=Deposit[t]-StockLoanB[t]
      }
      if(Deposit2[t]>(StockLoanB[t]+nonperformingloanbank[t]+creditconsumptionB[t])){
        ## in teoria dovrei fare sta cosa anche per le advances perche' non vanno calcolare sui depositi che considarno il reddito, cmq non lo faccio perche tanto sono sempre uguali a zeroo, dal momento che c e una banca sola e non ce cash circolante 
        Hb2[t]=Deposit2[t-1]+deltaDeposit2[t]-deltaRes[t]-StockLoanB[t]-creditconsumptionB[t]-nonperformingloanbank[t] # metto a riserva la differenza tra i depositi e i presiti, per avere un quantitativo suficente per apgare i tassi d interesse... in realta potrei mettere di meno perche i tassi d interesse sui presitit sono maggiori de depositi
        # in teoria questo andrebbe fatto sempre, perche se i depositi in un periodo risultano minori dello stock di debito, invece che chiedere advances, riduco le riserve.. ma comunque in quel caso dal momento ce sto consideranod lo stock non e vero
        
        #se i depositi attivi sono maggiori dei prestti, metto a riserva in modo da creare un entrata per pagare gli interessi (questa e' una tecnica rigidad nel senso che no nsfrutto il gioco sul differenziale dei tassi d interesse per coprirmi) 
      }else{
        Hb2[t]=Hb2[t-1] 
      }
      
    }else{
      if(Deposit[t]<(StockLoanB[t]+nonperformingloanbank[t]+creditconsumptionB[t])){
       # print("problema advances")
       # print(t)
        #Advances[t]=loandemandF[t]-servicedebtF[t]-deltaDeposit[t]
      }else{
        Hb[t]=deltaDeposit[t]-(LoanBDemand[t]+sum(creditdemand[t,])-sum(capitalhh[t,])-sum(servicedebtF[t,])-sum(restituzioneparziale[t,])-sum(servicericapital[t,])-PerditaB[t])
        
      }
      if(Deposit2[t]>(StockLoanB[t]+nonperformingloanbank[t]+creditconsumptionB[t])){
        Hb2[t]=deltaDeposit2[t]-(LoanBDemand[t]+sum(creditdemand[t,])-sum(capitalhh[t,])-sum(servicedebtF[t,])-sum(restituzioneparziale[t,])-sum(servicericapital[t,])-PerditaB[t])  #se i depositi attivi sono maggiori dei prestti, metto a riserva in modo da creare un entrata per pagare gli interessi (questa e' una tecnica rigidad nel senso che no nsfrutto il gioco sul differenziale dei tassi d interesse per coprirmi) 
      }else{
        Hb2[t]=0    
      }
      
    }
    
    
    
    
    BondDemand_cap[t]=sum(BondDemand[t,which(codclass[]==1)])
    # if(t>1){
    #   deltaAdvances[t]=Advances[t]-Advances[t-1]
    # }else{
    #   deltaAdvances[t]=Advances[t] 
    # }
    
    # bond residui acquistati dalla banca centrale 
    BuyBond_CB[t]=bond[t]-BondDemand_cap[t]
    if(round(BuyBond_CB[t],4)<0){
      print("ProblemBondCB")
    }
    bondscadenzaCB[t+tpub]=BuyBond_CB[t]
    
    
    
    #Bilancio CB
    if(t>1){
      BondRepayed_CB[t]=bondscadenzaCB[t]
      BondCB[t]=BondCB[t-1]+BuyBond_CB[t]-BondRepayed_CB[t]
      HPM[t]=BondCB[t]+Advances[t]  #CHE DI FATTI VA A ZERO SE LA DIFFERENZA TRA DEPOSITI E LOAN VIENE MESSA TUTTA A ADVANCES INVERSE (ADVANES NEGATIVE)
      deltaH[t]=HPM[t]-HPM[t-1]
    }else{
      BondCB[t]=BuyBond_CB[t]
      HPM[t]=BondCB[t]+PerditaB[t]
      deltaH[t]=HPM[t]
    }
    
    
    #Controlli
    redundant[t]=round(Deposit[t]-StockLoanB[t]-nonperformingloanbank[t]-creditconsumptionB[t]-BondCB[t]-Advances[t],3)
    if(round(redundant[t],2)!=0 | length(which(round(inv[t,],3)<0))>0){
      print("problem sfc or inventories")
      print(t)
      errortrial=numtrial
      save.image("/nobackup/bnldd/errorsfc.RData")    
      stop()
    }
    
    
    
    NW_workers[t]=cashHwmoment[t]-sum(hhdebt[t,])-sum(nonperformingloanhh[t,])
    NW_cap[t]=cashHcapmoment[t]+sum(BondDemand[t,which(codclass[]==1)])
    NWB[t]=StockLoanB[t]-Deposit[t]+Hb[t]-Advances[t]+creditconsumptionB[t]+sum(nonperformingloanhh[t,]) # da controllare
    NWG[t]=cashG[t]-stockdebtG[t]
    NWCB[t]=BondCB[t]-Hb[t]+Advances[t]
    #NWCB[t]=BondCB[t]+Advances[t]-Hb[t]   #praticamente tutta la moneta creata dalla banca centrale è detenuta sotto forma di riserve dalla banca stessa, che [ appunto la differenza tra is suoi depositit e prestit]
    
    
    # cash flow della banca
    if(t>1){
      CFB[t]=CFB[t-1]+sum(servicedebtFtot[t,])-sum(servicedebtFtot[t,which(bancarotta[t,]==1)])-LoanBDemand[t]+deltaDeposit[t]+sum(restituzioneparziale[t,])+sum(servicericapital[t,])-sum(creditdemand[t,])+sum(effectivepaymentB[t,])
    }else{
      CFB[t]=sum(servicedebtFtot[t,])-LoanBDemand[t]+deltaDeposit[t]++sum(restituzioneparziale[t,])
    }
    #if(CFB[t]<0){
    # if(t>1){
    #Advances[t]=Advances[t-1]-CFB[t]  #LoanBDemand[t]-servicedebtFtot[t]-deltaDeposit[t]
    #}else{
    #  Advances[t]=-CFB[t]
    #}
    #}
    
    
    #calcolo dei marketshare realizzati ndalle imprese capital
    totalrealsailsK=sum(realsails[t,which(cod==0)])
    
    
    
    # for(i in 1:NFK){
    # marketshare[t,i]=realsails[t,i]/totalrealsailsK   #vedere come cambia se metto la domanda ricevuta invece delle vendite
    # if(is.nan(marketshare[t,i])==TRUE){
    #   marketshare[t,i]=0
    #}
    # ratiomarketshare[t,i]=marketshare[t,i]/((1-realsails[t,i]/totalrealsailsK)/(NFK-1))
    # if(is.nan(ratiomarketshare[t,i])==TRUE){
    #   ratiomarketshare[t,i]=0
    #  }
    #  }
    # yc[t,mc]=sum(y[t,which(cod==1)])
    #def_yc[t]=saldosecondario[t]/yc[t]
    #deb_yc[t,mc]=stockdebtG[t]/(sum(realsails[t,which(cod==1)]))
    
    
    
    percbondcap[t]=BondDemand_cap[t]/stockdebtG[t]
    percbondCB[t]=BondCB[t]/stockdebtG[t]
    
    
    if(FALSE){
      totdemandcapital=sum(demandperfirmcapital[t,])
      for(i in 1:NFK){
        marketshare[t,i]=demandperfirmcapital[t,i]/totdemandcapital   #vedere come cambia se metto la domanda ricevuta invece delle vendite
        if(is.nan(marketshare[t,i])==TRUE){
          marketshare[t,i]=0
        }
        ratiomarketshare[t,i]=marketshare[t,i]/((1-demandperfirmcapital[t,i]/totdemandcapital)/(NFK-1))
        if(is.nan(ratiomarketshare[t,i])==TRUE){
          ratiomarketshare[t,i]=0
        }
      }
      for(i in 1:NFK){
        if(marketshare[t,i]>(1/NFK*1.3)){
          if(t==1){
            limalzare[t,i]=1
            limabbassare[t,i]=0
          }else{
            limalzare[t,i]=limalzare[t-1,i]+1
            limabbassare[t,i]=0
          }
        }else if(marketshare[t,i]<(1/NFK*0.5)){
          if(t==1){
            limabbassare[t,i]=1
            limalzare[t,i]=0
          }else{
            limabbassare[t,i]=limabbassare[t-1,i]+1
            limalzare[t,i]=0
          }
        }else{
          limalzare[t,i]=0
          limabbassare[t,i]=0
        }
      }
    }   
    
    totalrealsailsC=0
    totalrealsailsC=sum(realsails[t,which(cod==1)])
    
    # for(i in (NFK+1):NF){
    #   priceweightedC[i]=(realsails[t,i]/totalrealsailsC)*p[t,i]
    #   markupweightedC[i]=(realsails[t,i]/totalrealsailsC)*markup[t,i]
    # }
    # 
    if(totalrealsailsC>0){
      priceweightedC[(NFK+1):NF]=realsails[t,(NFK+1):NF]/totalrealsailsC*p[t,(NFK+1):NF]
      markupweightedC[(NFK+1):NF]=realsails[t,(NFK+1):NF]/totalrealsailsC*markup[t,(NFK+1):NF]
    }
    
    totalrealsailsK=sum(realsails[t,which(cod==0)])
    # for(i in 1:NFK){
    #   if(totalrealsailsK>0){
    #     priceweightedK[i]=(realsails[t,i]/totalrealsailsK)*p[t,i]
    #     markupweightedK[i]=(realsails[t,i]/totalrealsailsK)*markup[t,i]
    #   }
    # }
    
    
    if(totalrealsailsK>0){
      priceweightedK[1:NFK]=(realsails[t,1:NFK]/totalrealsailsK)*p[t,1:NFK]
      markupweightedK[1:NFK]=(realsails[t,1:NFK]/totalrealsailsK)*markup[t,1:NFK]
    }
    
    #indice dei prezzi
    priceindexC[t]=sum(priceweightedC)
    priceindexK[t]=sum(priceweightedK)
    #indice dei markup
    markupindexC[t]=sum(markupweightedC)
    markupindexK[t]=sum(markupweightedK)
    #calcolo wage e profit share:
    profitshare[t]=sum(yh[t,which(codclass[]==1)])/sum(yh[t,])
    wageshare[t]=sum(yh[t,which(codclass[]==0)])/sum(yh[t,])
    #calcolo consumo dei capitalisti: da aggiornare
    #capitalistnominaldemand=sum(realconsumptiondemand[which(codclass==1)])*p[NF]
    # capitalistnominalconsumptionfeasible=capitalistnominaldemand-sum(residualconsumpiondemand[which(codclass==1)])*p[NF]
    #capitalistnominalconsumptionplusinvestment=capitalistnominalconsumptionfeasible+sum(kdfeasible[t,])*p[1]
    GDPreal[t]=sum(y[t,which(cod==0)])*priceindexK[1]+sum(y[t,which(cod==1)])*priceindexC[1]
    u_raggregate[t]=sum(k_used[t,])/ktot
    GDP[t]=sum(y[t,which(cod==1)]*p[t,which(cod==1)])+sum(y_valorek[t,]) #GDP non deflazionato
    Gweigth[t]=Gtot[t]/GDP[t]
    Investmentvalue[t]=sum(y_valorek[t,])
    Investmentwight[t]=Investmentvalue[t]/GDP[t]
    investmentwight2[t]=sum(kd[t,])*p[t,1]
    Consumptionhh[t]=sum(revenues[t,which(cod==1)])-Gcons[t]
    Cweight[t]=Consumptionhh[t]/GDP[t]
    
    if(TRUE){
      
      if(t==1){
      }else{
        inflazione[t]=(priceindexC[t]-priceindexC[t-1])/priceindexC[t-1]
        
      }
      
      produzionek[t]=sum(y[t,1:NFK])
      #GDPreale[t,mc]=GDPreal[t]
      InvestmentR[t]=sum(kdordinatofeasible[t,])
      Unemployment[t]=1-sum(employees[t,])/Nwork
      Employeestot[t]=sum(employees[t,])
      
      U[t]=(sum(employees[t,]))/Nwork
      Une[t]=1-U[t]
      Nonemp[t]=Nwork-sum(employees[t,])
      invtotC[t]=sum(inv[t,which(cod==1)])
      TotdebtF[t]=sum(stockdebtF[t,])
      
      y_hcap[t]=sum(yhd[t,which(codclass[]==1)])
      
      privatedebt_pil[t]=TotdebtF[t]/GDP[t]
      
      
      realyhworkers[t]=sum(realvalueyhd[t,1:Nwork])
      realyhcapitalist[t]=sum(realvalueyhd[t,(Nwork:N)])
      
      debpil[t]=stockdebtG[t]/(GDP[t])
      debpilreal[t]=stockdebtG[t]/(GDPreal[t])
      
      Def_Pil[t]=saldoprimario[t]/GDP[t]
      Def_Pil2[t]=saldosecondario[t]/GDP[t]
      
      firms=sort(k[t,],decreasing=TRUE)
      tresh=firms[perc10]   #5 % delle imprese piu grandi
      u_raggregatebigs[t]=sum(k_used[t,which(k[t,]>=tresh)])/sum(k[t,which(k[t,]>=tresh)])
      firms=sort(k[t,],decreasing=FALSE)[(NFK+1):NF]
      tresh2=firms[perc10] 
      u_raggregatesmall[t]=sum(k_used[t,which(k[t,]<=tresh2)])/sum(k[t,which(k[t,]<=tresh2)])
      firms=sort(k[t,],decreasing=TRUE)
      tresh4=firms[perc30] 
      tresh3=firms[perc60]
      u_raggregatemedium[t]=sum(k_used[t,which(k[t,]>tresh3 & k[t,]<tresh4)])/sum(k[t,which(k[t,]>tresh3 & k[t,]<tresh4)])
      
      
      #ue_pesati_aggregatebigs[t,(NFK+1):NF]=uefuturo[t,which(k[t,]>=tresh)]*k[t,which(k[t,]>=tresh)]/sum(k[t,which(k[t,]>=tresh)])
      
      
      
      if(t>dk){    
        ratio[t,(NFK+1):NF]=(wb[t,(NFK+1):NF]+sum(kdfeasiblenominal[(t-dk+1):t,(NFK+1):NF])/(dk*(1+markupk))+sum(kdfeasiblenominal[(t-dk):(t-1),(NFK+1):NF])*(markupk/(dk*(1+markupk)))+div[t-1,(NFK+1):NF])/(Gef[t]+Consumptionhh[t])-revenues[t,(NFK+1):NF]/(Gef[t]+Consumptionhh[t])
        meanratio[t]=sum(ratio[t,(NFK+1):NF])/NFC
      }
      
      if(t>1){
        gr[t,(NFK+1):NF]=(realsails[t,(NFK+1):NF]-realsails[t-1,(NFK+1):NF])/(realsails[t-1,(NFK+1):NF])
        gr[t,(NFK+1):NF]=ifelse(is.nan(gr[t,(NFK+1):NF])==TRUE,0,gr[t,(NFK+1):NF])
        gr[t,(NFK+1):NF]=ifelse(is.infinite(gr[t,(NFK+1):NF])==TRUE,1,gr[t,(NFK+1):NF])
        gr[t,(NFK+1):NF]=ifelse(round(realsails[t-1,(NFK+1):NF],5)==0,1,gr[t,(NFK+1):NF])
        
      }
      
      if(t>1){
        #g_yc[t]=(yc[t]-yc[t-1])/yc[t-1]
        g_GDP[t]=(GDP[t]-GDP[t-1])/GDP[t-1]
        g_Tax[t]=(Tax[t]-Tax[t-1])/Tax[t-1]
        g_U[t]=(Nonemp[t]-Nonemp[t-1])/Nonemp[t-1]
        g_GDPreal[t]=(GDPreal[t]-GDPreal[t-1])/GDPreal[t-1]
        if(is.nan(g_U[t])==TRUE){
          g_U[t]=0
        }
      }
      
      
      pastbaskettotworkers[t]=sum(pastbasket[t,which(codclass[]==0)])
      pastbaskettotcapitalists[t]=sum(pastbasket[t,which(codclass[]==1)])
      consumptionemployed[t]=sum(totrealconsumption[t,which(unemp[]==0)])
      yhdemployed[t]=sum(yhd[t,which(unemp[]==0)])
    }
    # if(t>1){
    #   gr[t,2]=(realsails[t,2]-realsails[t-1,2])/realsails[t-1,2]
    #   if(is.nan(gr[t,2])==TRUE){
    #     gr[t,2]=0
    #   }
    # }
    
    # 
    # consumptionworkers[t]=sum(nominalconsumptionexpenditure[t,which(codclass==0)])
    # consumptioncapitalist[t]=sum(nominalconsumptionexpenditure[t,which(codclass==1)])
    # 
    # cwpost[t]=consumptionworkers[t]/(sum(yhd[t,which(codclass==0)]))
    # if(t>1){
    #   ccappost[t]=consumptioncapitalist[t]/(sum(yhd[t-1,which(codclass==1)]))
    # }
    # 
    
    # if(cwpost[t]>1){
    #   print("problempropconsumptionWorkers")
    #   print(t)
    # }
    # if(ccappost[t]>1){
    #   print("problempropconsumptionCapitalists")
    #   print(t)
    # }
    
    
    if(FALSE){  
      # res_macro$Y[t]=sum(y[t,])
      #res_macro$Realconsumptiondemand[t]=sum(realconsumptiondemand[])
      # res_macro$Consumptiondemand[t]=sum(consumptiondemand)
      #res_macro$Residualconsumptiondemand[t]=sum(residualconsumpiondemand)
      #  res_macro$Invtot[t]=sum(inv[t,])
      # res_macro$InvFC[t]=sum(inv[t,which(cod==1)])
      #  res_macro$Nominaldemandcapital[t]=sum(kdfeasiblenominal[t,])
      # res_macro$RevenuesC[t]=sum(revenues[t,which(cod==1)])
      #res_macro$RevenuesK[t]=sum(revenues[t,which(cod==0)])
      #res_macro$RevenuesTot[t]=sum(revenues[t,])
      #res_macro$Currentcosttot[t]=sum(currentcost)
      #res_macro$CurrentcostC[t]=sum(currentcost[which(cod==1)])
      #res_macro$CurrentcostK[t]=sum(currentcost[which(cod==0)])
      #res_macro$Profittot[t]=sum(profit[t,])
      #res_macro$ProfitC[t]=sum(profit[t,which(cod==1)])
      #res_macro$ProfitK[t]=sum(profit[t,which(cod==0)])
      #res_macro$Dividendtot[t]=sum(div[t,])
      #res_macro$DividendC[t]=sum(div[t,which(cod==1)])
      #res_macro$DividendK[t]=sum(div[t,which(cod==0)])
      #res_macro$Employed[t]=sum(employees[t,])
      #res_macro$WB[t]=sum(wb[t,])
      #res_macro$Yhworkers[t]=sum(yh[t,which(codclass==0)])
      #res_macro$YHcapitalist[t]=sum(yh[t,which(codclass==1)])
      #res_macro$Laborrequired[t]=sum(lrequired[t,])
      #res_macro$Kinstalledintheperiod[t]=sum(Iinstalled[t,])
      ###res_macro$CashFC[t]=sum(cashF[which(cod==1)])
      ###res_macro$CashFK[t]=sum(cashF[which(cod==0)])
      #res_macro$CashF[t]=sum(cashF)
      #res_macro$CashH[t]=sum(cashH)
      #res_macro$Kdemanded[t]=sum(kd[t,])
      #res_macro$K[t]=sum(k[t,])
      #res_macro$domandalavoro[t]=sum(labordemand[t,])
      #res_macro$Stockdebt[t]=sum(stockdebtF[t,])
      #res_macro$Loandemand[t]=sum(LoanBDemand[t,])
      #res_macro$Debtservice[t]=sum(servicedebtF[t,])
      #res_macro$Ponzi[t]=sum(ponzis[t,])
      #res_macro$priceindC[t]=priceindexC
      #res_macro$priceindK[t]=priceindexK
      #res_macro$markupindC[t]=markupindexC
      #res_macro$markupindK[t]=markupindexK
      #  res_macro$capacitygrowthrate[t]=capacitygrowthrate[t]
      #  res_macro$realconsumptiongrowthrate[t]=realconsumptiongrowthrate[t]
      #  res_macro$ratiocapconsgrowthrate[t]=ratiocapconsgrowthrate[t]
      # res_macro$wageshare[t]=wageshare
      #  res_macro$profitshare[t]=profitshare
      # res_macro$NCC[t]=capitalistnominaldemand
      #  res_macro$NCCF[t]=capitalistnominalconsumptionfeasible
      # res_macro$NCCI[t]=capitalistnominalconsumptionplusinvestment
      #   "qatteso","inventories","inv_t","ydesisderato","produzioneeffettuata","uatteso","uattesosupply","lavoro richiesto","domandalavoro","occupazione","WB","domandaprestito","kdomenadato","kinstallato","venditereali","ricavi","uvincolatolavoro","urealizzato","ksegnato","domandaperleimpreseconsumer","domandaperl'imrpesacapital","serviziodebito","ponzi","ammortamenti","costicorrenti","profitti","dividendi","cassaF","cassaH"))))
      #real variables
      #RS$Iinstalled[t]=sum(Iinstalled[t,])
      #RS$Kd_t_dk[t]=sum(kd[t-dk,])
      #RS$K[t]=sum(k[t,])
      #RS$y_dc[t]=sum(y_d[t,which(cod==1)])
      #RS$yc[t]=sum(y[t,which(cod==1)])
      # RS$kd[t]=sum(kd[t,which(cod==1)])
      # RS$invc[t]=sum(inv[t,which(cod==1)])
      # RS$realsailsc[t]=sum(realsails[t,which(cod==1)])
      # RS$realconsumptiondemand[t]=sum(realconsumptiondemand[which(codclass==0)])
      # RS$invk[t]=sum(inv[t,which(cod==0)])
      # RS$y_dk[t]=sum(y_d[t,which(cod==0)])
      # RS$yk[t]=sum(y[t,which(cod==0)])
      # RS$ynewk[t]=sum(y_new[t,which(cod==0)])
      # RS$realsailsk[t]=sum(realsails[t,which(cod==0)])
      # RS$lrequired[t]=sum(lrequired[t,])
      # RS$employees[t]=sum(employees[t,])
      # #Nominal variables
      # NS$kdnominal[t]=sum(kdfeasiblenominal[t,])
      # NS$WBC[t]=sum(wb[t,which(cod==1)])     #
      # NS$revenuesc[t]=sum(revenues[t,which(cod==1)])    #
      # NS$debtserviceC[t]=sum(servicedebtF[t,which(cod==1)])   #
      # NS$loandemandC[t]=sum(loandemandF[t,which(cod==1)])
      # NS$StockdebtFC[t]=sum(stockdebtF[t,which(cod==1)])
      NS$cashC[t]=sum(cashF[which(cod==1)])   #
      # NS$profitC[t]=sum(profit[t,which(cod==1)])
      # NS$divC[t]=sum(div[t,which(cod==1)])
      # NS$revenuesk[t]=sum(revenues[t,which(cod==0)])
      # NS$WBK[t]=sum(wb[t,which(cod==0)])
      # NS$loandemandk[t]=sum(loandemandF[t,which(cod==0)])
      # NS$overdraftdemandC[t]=sum(overdraftdemand[t,which(cod==1)])
      # NS$debtserviceK[t]=sum(servicedebtF[t,which(cod==0)])
      # NS$StockdebtFK[t]=sum(stockdebtF[t,which(cod==0)])
      NS$cashK[t]=sum(cashF[which(cod==0)])
      # NS$debtservtotK[t]=sum(servicedebtFtot[t,which(cod==0)])
      # NS$profitK[t]=sum(profit[t,which(cod==0)])
      # NS$divK[t]=sum(div[t,which(cod==0)])
      # NS$WB[t]=sum(wb[t,])
      # NS$yhw[t]=sum(yh[t,which(codclass==0)])
      # NS$yhcapitalisti[t]=sum(yh[t,which(codclass==1)])
      # NS$nominalconsdemandw[t]=sum(nominalconsumptiondemand[which(codclass==0)])
      NS$nomianlconsxpw[t]=sum(nominalconsumptionexpenditure[t,which(codclass==0)])
      # NS$nominalconsdemandCap[t]=sum(nominalconsumptiondemand[which(codclass==1)])
      NS$nomconsexpcap[t]=sum(nominalconsumptionexpenditure[t,which(codclass==1)])
      # NS$cashHw[t]=sum(cashH[which(codclass==0)])
      # NS$cashHcap[t]=sum(cashH[which(codclass==1)])
      # NS$totrealconsumption[t]=sum(totrealconsumption)
      # NS$taxpaymentw[t]=sum(taxpayment[t,which(codclass==0)])
      # NS$taxpaymentcap[t]=sum(taxpayment[t,which(codclass==1)])
      # NS$Tax[t]=Tax[t]
      
      
      #THE BALANCE SHEET OF THE ECONOMY ##################################################################################
      #DEPOSITS
      BS[which(rownames(BS)=="Deposits"),which(colnames(BS)=="WORKERS"),t]=Moneydeposit_worker[t]
      BS[which(rownames(BS)=="Deposits"),which(colnames(BS)=="CAPITALISTS"),t]=cashHcapmoment[t]
      BS[which(rownames(BS)=="Deposits"),which(colnames(BS)=="FIRMSK"),t]=sum(cashF[which(cod==0)])
      BS[which(rownames(BS)=="Deposits"),which(colnames(BS)=="FIRMSC"),t]=sum(cashF[which(cod==1)])
      BS[which(rownames(BS)=="Deposits"),which(colnames(BS)=="BANKS"),t]=-Deposit[t]
      BS[which(rownames(BS)=="Deposits"),which(colnames(BS)=="GOVERNMENT"),t]=cashG[t]
      #LOANS
      BS[which(rownames(BS)=="Loans"),which(colnames(BS)=="FIRMSK"),t]=-sum(stockdebtF[t,which(cod==0)])
      BS[which(rownames(BS)=="Loans"),which(colnames(BS)=="FIRMSC"),t]=-sum(stockdebtF[t,which(cod==1)])
      BS[which(rownames(BS)=="Loans"),which(colnames(BS)=="BANKS"),t]=StockLoanB[t]
      #BS[which(rownames(BS)=="Loans"),which(colnames(BS)=="CB"),t]=-ReservesCB[t]
      # CREDIT CONSUMPTION
      BS[which(rownames(BS)=="Credit_Cons"),which(colnames(BS)=="WORKERS"),t]=-sum(hhdebt[t,])
      BS[which(rownames(BS)=="Credit_Cons"),which(colnames(BS)=="BANKS"),t]=creditconsumptionB[t]
      # Non performingloan credit
      BS[which(rownames(BS)=="Nonperforming_Cons"),which(colnames(BS)=="WORKERS"),t]=-sum(nonperformingloanhh[t,])
      BS[which(rownames(BS)=="Nonperforming_Cons"),which(colnames(BS)=="BANKS"),t]=nonperformingloanbank[t]
      
      #INVENTORIES
      BS[which(rownames(BS)=="Inventories"),which(colnames(BS)=="FIRMSC"),t]=sum(valueinv[t,which(cod==1)])
      #FIXED CAPITAL
      BS[which(rownames(BS)=="Fixed Capital"),which(colnames(BS)=="FIRMSC"),t]=sum(ammresiduo[t,])
      #ADVANCES
      BS[which(rownames(BS)=="Advances"),which(colnames(BS)=="BANKS"),t]=-Advances[t]
      BS[which(rownames(BS)=="Advances"),which(colnames(BS)=="CB"),t]=Advances[t]
      #HPM $ reserves
      BS[which(rownames(BS)=="HPM"),which(colnames(BS)=="CB"),t]=-HPM[t]
      BS[which(rownames(BS)=="HPM"),which(colnames(BS)=="BANKS"),t]=Hb[t]
      
      
      #Bonds
      BS[which(rownames(BS)=="Bond"),which(colnames(BS)=="GOVERNMENT"),t]=-stockdebtG[t]
      BS[which(rownames(BS)=="Bond"),which(colnames(BS)=="CB"),t]=BondCB[t]
      BS[which(rownames(BS)=="Bond"),which(colnames(BS)=="CAPITALISTS"),t]=BondDemand_cap[t]
      #NET WORTH
      BS[which(rownames(BS)=="Net Worth"),which(colnames(BS)=="WORKERS"),t]=-Moneydeposit_worker[t]+sum(hhdebt[t,])+sum(nonperformingloanhh[t,])  #-sum(cashH[which(codclass==0)])
      BS[which(rownames(BS)=="Net Worth"),which(colnames(BS)=="CAPITALISTS"),t]=-(cashHcapmoment[t]+BondDemand_cap[t])  #-sum(cashH[which(codclass==1)])-sum(yh[t,which(codclass==1)])
      BS[which(rownames(BS)=="Net Worth"),which(colnames(BS)=="FIRMSK"),t]=-sum(NW[t,which(cod==0)])
      BS[which(rownames(BS)=="Net Worth"),which(colnames(BS)=="FIRMSC"),t]=-sum(NW[t,which(cod==1)])
      BS[which(rownames(BS)=="Net Worth"),which(colnames(BS)=="BANKS"),t]=-NWB[t]
      BS[which(rownames(BS)=="Net Worth"),which(colnames(BS)=="GOVERNMENT"),t]=-NWG[t]
      BS[which(rownames(BS)=="Net Worth"),which(colnames(BS)=="CB"),t]=-NWCB[t]
      #CONSISTENCY CHECK ROWs
      BS[which(rownames(BS)=="Deposits"),which(colnames(BS)=="SUM"),t]=round(sum(BS[which(rownames(BS)=="Deposits"),,t]), digits = 4)
      BS[which(rownames(BS)=="Net Worth"),which(colnames(BS)=="SUM"),t]=round(sum(BS[which(rownames(BS)=="Net Worth"),,t]), digits = 4)
      BS[which(rownames(BS)=="Loans"),which(colnames(BS)=="SUM"),t]=round(sum(BS[which(rownames(BS)=="Loans"),,t]), digits = 4)
      BS[which(rownames(BS)=="Credit_Cons"),which(colnames(BS)=="SUM"),t]=round(sum(BS[which(rownames(BS)=="Credit_Cons"),,t]), digits = 4)
      BS[which(rownames(BS)=="Nonperforming_Cons"),which(colnames(BS)=="SUM"),t]=round(sum(BS[which(rownames(BS)=="Nonperforming_Cons"),,t]), digits = 4)
      BS[which(rownames(BS)=="Advances"),which(colnames(BS)=="SUM"),t]=round(sum(BS[which(rownames(BS)=="Advances"),,t]), digits = 4)
      BS[which(rownames(BS)=="Inventories"),which(colnames(BS)=="SUM"),t]=round(sum(BS[which(rownames(BS)=="Inventories"),,t]), digits = 4)
      BS[which(rownames(BS)=="Bond"),which(colnames(BS)=="SUM"),t]=round(sum(BS[which(rownames(BS)=="Bond"),,t]), digits = 4)
      BS[which(rownames(BS)=="Fixed Capital"),which(colnames(BS)=="SUM"),t]=round(sum(BS[which(rownames(BS)=="Fixed Capital"),,t]), digits = 4)
      BS[which(rownames(BS)=="HPM"),which(colnames(BS)=="SUM"),t]=round(sum(BS[which(rownames(BS)=="HPM"),,t]), digits = 4)
      #CONSISTENCY CHECK COLUMNSs
      BS[which(rownames(BS)=="SUM"),which(colnames(BS)=="WORKERS"),t]=round(sum(BS[,which(colnames(BS)=="WORKERS"),t]), digits = 4)
      BS[which(rownames(BS)=="SUM"),which(colnames(BS)=="CAPITALISTS"),t]=round(sum(BS[,which(colnames(BS)=="CAPITALISTS"),t]), digits = 4)
      BS[which(rownames(BS)=="SUM"),which(colnames(BS)=="FIRMSK"),t]=round(sum(BS[,which(colnames(BS)=="FIMRSK"),t]), digits = 4)
      BS[which(rownames(BS)=="SUM"),which(colnames(BS)=="FIRMSC"),t]=round(sum(BS[,which(colnames(BS)=="FIMRSC"),t]), digits = 4)
      BS[which(rownames(BS)=="SUM"),which(colnames(BS)=="BANKS"),t]=round(sum(BS[,which(colnames(BS)=="BANKS"),t]),4)
      BS[which(rownames(BS)=="SUM"),which(colnames(BS)=="GOVERNMENT"),t]=round(sum(BS[,which(colnames(BS)=="GOVERNMENT"),t]), digits = 4)
      BS[which(rownames(BS)=="SUM"),which(colnames(BS)=="CB"),t]=round(sum(BS[,which(colnames(BS)=="CB"),t]), digits = 4)
      BS[which(rownames(BS)=="SUM"),which(colnames(BS)=="SUM"),t]=round(sum(BS[,which(colnames(BS)=="SUM"),t]), digits = 4)
      
      #TRANSACTION MATRIX##############################
      #Consumption
      TM[which(rownames(TM)=="Consumption"),which(colnames(TM)=="WORKERS"),t]=-NS$nomianlconsxpw[t]
      TM[which(rownames(TM)=="Consumption"),which(colnames(TM)=="CAPITALISTS"),t]=-NS$nomconsexpcap[t]
      TM[which(rownames(TM)=="Consumption"),which(colnames(TM)=="FIRMSC"),t]=sum(revenues[t,(NFK+1):NF])-Gcons[t]
      #investments
      TM[which(rownames(TM)=="Investments"),which(colnames(TM)=="FIRMSC"),t]=-sum(kdfeasiblenominal[t,which(cod==1)])
      TM[which(rownames(TM)=="Investments"),which(colnames(TM)=="FIRMSK"),t]=sum(revenues[t,which(cod==0)])
      #GOV EXPENDITURE
      TM[which(rownames(TM)=="GovExp"),which(colnames(TM)=="GOVERNMENT"),t]=-Gcons[t]
      TM[which(rownames(TM)=="GovExp"),which(colnames(TM)=="FIRMSC"),t]=Gcons[t]
      #TAX
      TM[which(rownames(TM)=="Tax"),which(colnames(TM)=="WORKERS"),t]=-taxpaymentworker[t]
      TM[which(rownames(TM)=="Tax"),which(colnames(TM)=="CAPITALISTS"),t]=-taxpaymentcap[t]
      TM[which(rownames(TM)=="Tax"),which(colnames(TM)=="GOVERNMENT"),t]=Tax[t]
      #wages
      TM[which(rownames(TM)=="Wages"),which(colnames(TM)=="WORKERS"),t]=sum(wb[t,])
      TM[which(rownames(TM)=="Wages"),which(colnames(TM)=="FIRMSK"),t]=-sum(wb[t,which(cod==0)])
      TM[which(rownames(TM)=="Wages"),which(colnames(TM)=="FIRMSC"),t]=-sum(wb[t,which(cod==1)])
      #PROFITS
      TM[which(rownames(TM)=="Profits"),which(colnames(TM)=="FIRMSK"),t]=-sum(div[t,which(cod==0)])
      TM[which(rownames(TM)=="Profits"),which(colnames(TM)=="FIRMSC"),t]=-sum(div[t,which(cod==1)])
      TM[which(rownames(TM)=="Profits"),which(colnames(TM)=="BANKS"),t]=-divB[t]
      TM[which(rownames(TM)=="Profits"),which(colnames(TM)=="CAPITALISTS"),t]=sum(div[t,which(cod==0)])+sum(div[t,which(cod==1)])+divB[t]
      #ricapitalizzazione capitalisti
      TM[which(rownames(TM)=="Recapital"),which(colnames(TM)=="CAPITALISTS"),t]=-sum(dissavingcap[t,])
      TM[which(rownames(TM)=="Recapital"),which(colnames(TM)=="FIRMSC"),t]=sum(dissavingcap[t,])
      #interests laons
      TM[which(rownames(TM)=="IntOnLoans&ov"),which(colnames(TM)=="FIRMSK"),t]=-sum(interestpaymentF[t,which(cod==0)])
      TM[which(rownames(TM)=="IntOnLoans&ov"),which(colnames(TM)=="FIRMSC"),t]=-sum(interestpaymentF[t,which(cod==1)])
      TM[which(rownames(TM)=="IntOnLoans&ov"),which(colnames(TM)=="BANKS"),t]=sum(interestpaymentF[t,]) #dovrei aggiungere gli interessi che riceve dalla banca centrale
      #Interests credit to consumption
      TM[which(rownames(TM)=="IntCreditConsumption"),which(colnames(TM)=="WORKERS"),t]=-Bankcreditinterest[t]
      TM[which(rownames(TM)=="IntCreditConsumption"),which(colnames(TM)=="BANKS"),t]=Bankcreditinterest[t]
      
      #UnBenefit
      TM[which(rownames(TM)=="UnBenefit"),which(colnames(TM)=="GOVERNMENT"),t]=-Ubenefit[t]
      TM[which(rownames(TM)=="UnBenefit"),which(colnames(TM)=="WORKERS"),t]=Ubenefit[t]
      
      
      #INTERESTDEPOSIT
      if(t>1){
        TM[which(rownames(TM)=="InterestDeposit"),which(colnames(TM)=="WORKERS"),t]=Moneydeposit_worker[t-1]*intratedeposit[t-1] 
        TM[which(rownames(TM)=="InterestDeposit"),which(colnames(TM)=="CAPITALISTS"),t]=Moneydeposit_cap[t-1]*intratedeposit[t-1]
        TM[which(rownames(TM)=="InterestDeposit"),which(colnames(TM)=="BANKS"),t]=-Moneydeposit_cap[t-1]*intratedeposit[t-1]-Moneydeposit_worker[t-1]*intratedeposit[t-1]
        
        #PROFITTI CB
        TM[which(rownames(TM)=="ProfitCB"),which(colnames(TM)=="CB"),t]=-ProfitCB[t]
        TM[which(rownames(TM)=="ProfitCB"),which(colnames(TM)=="GOVERNMENT"),t]=ProfitCB[t]
        
        
        
        #interessi riserve
        TM[which(rownames(TM)=="InterestHb"),which(colnames(TM)=="CB"),t]=-Hb2[t-1]*intratedeposit[t-1]
        TM[which(rownames(TM)=="InterestHb"),which(colnames(TM)=="BANKS"),t]=Hb2[t-1]*intratedeposit[t-1]
        
        #iNTERESSI Advances
        TM[which(rownames(TM)=="InterestAdv"),which(colnames(TM)=="CB"),t]=Advances[t-1]*advanceinterest[t-1]
        TM[which(rownames(TM)=="InterestAdv"),which(colnames(TM)=="BANKS"),t]=-Advances[t-1]*advanceinterest[t-1]
      }
      #interest Bond pubblici
      TM[which(rownames(TM)=="InterestBond"),which(colnames(TM)=="CB"),t]=interestBond_CB[t]
      TM[which(rownames(TM)=="InterestBond"),which(colnames(TM)=="GOVERNMENT"),t]=-interestpaymentG[t]
      if(t>1){
        TM[which(rownames(TM)=="InterestBond"),which(colnames(TM)=="CAPITALISTS"),t]=bondinterest[t-1]*BondDemand_cap[t-1]
      }
      
      #Deltacash->deltadeposit
      if(t>1){
        TM[which(rownames(TM)=="deltadepositi"),which(colnames(TM)=="FIRMSK"),t]=-(NS$cashK[t]-NS$cashK[t-1])
        TM[which(rownames(TM)=="deltadepositi"),which(colnames(TM)=="FIRMSC"),t]=-(NS$cashC[t]-NS$cashC[t-1])
        TM[which(rownames(TM)=="deltadepositi"),which(colnames(TM)=="WORKERS"),t]=-(Moneydeposit_worker[t]-Moneydeposit_worker[t-1])
        TM[which(rownames(TM)=="deltadepositi"),which(colnames(TM)=="CAPITALISTS"),t]=-(cashHcapmoment[t]-cashHcapmoment[t-1])
        TM[which(rownames(TM)=="deltadepositi"),which(colnames(TM)=="GOVERNMENT"),t]=-(cashG[t]-cashG[t-1])
      }else{
        TM[which(rownames(TM)=="deltadepositi"),which(colnames(TM)=="FIRMSK"),t]=-NS$cashK[t]
        TM[which(rownames(TM)=="deltadepositi"),which(colnames(TM)=="FIRMSC"),t]=-NS$cashC[t]
        TM[which(rownames(TM)=="deltadepositi"),which(colnames(TM)=="WORKERS"),t]=-Moneydeposit_worker[t]
        TM[which(rownames(TM)=="deltadepositi"),which(colnames(TM)=="CAPITALISTS"),t]=-cashHcapmoment[t]
        TM[which(rownames(TM)=="deltadepositi"),which(colnames(TM)=="GOVERNMENT"),t]=-cashG[t]
      }
      TM[which(rownames(TM)=="deltadepositi"),which(colnames(TM)=="BANKS"),t]=deltaDeposit[t]
      #deltaAssetatCB[t]=PerditaB[t]
      #DELTALOANS
      TM[which(rownames(TM)=="deltaloan"),which(colnames(TM)=="FIRMSK"),t]=sum(loandemandF[t,which(cod==0)])-sum(servicedebtF[t,which(cod==0)])
      TM[which(rownames(TM)=="deltaloan"),which(colnames(TM)=="FIRMSC"),t]=sum(loandemandF[t,which(cod==1)])-sum(servicedebtF[t,which(cod==1)])+sum(shortloandemandF[t,which(cod==1)])+sum(overdraftdemand[t,which(cod==1)])-sum(restituzioneparziale[t,which(cod==1)])-sum(servicericapital[t,which(cod==1)])-PerditaB[t] #quest' ultimo corrsiponde al debito cancellato
      TM[which(rownames(TM)=="deltaloan"),which(colnames(TM)=="BANKS"),t]=-(LoanBDemand[t]-sum(servicedebtF[t,])-sum(restituzioneparziale[t,])-sum(servicericapital[t,])-PerditaB[t])
      TM[which(rownames(TM)=="deltaloan"),which(colnames(TM)=="CB"),t]=deltaAssetatCB[t] #andrebbe modificato con delta advances= stockdebito-depositi
      #Delta credit consumption
      TM[which(rownames(TM)=="deltacreditCons"),which(colnames(TM)=="BANKS"),t]=-(sum(creditdemand[t,])-sum(capitalhh[t,]))
      TM[which(rownames(TM)=="deltacreditCons"),which(colnames(TM)=="WORKERS"),t]=sum(creditdemand[t,])-sum(capitalhh[t,])
      #deltaAdvances
      TM[which(rownames(TM)=="deltaAdvances"),which(colnames(TM)=="BANKS"),t]=deltaAdvances[t] #andrebbe modificato con delta advances
      TM[which(rownames(TM)=="deltaAdvances"),which(colnames(TM)=="CB"),t]=-deltaAdvances[t]  #andrebbe modificato con delta advances
      #non performing loans
      TM[which(rownames(TM)=="deltanonperforming"),which(colnames(TM)=="FIRMSC"),t]=PerditaB[t]
      TM[which(rownames(TM)=="deltanonperforming"),which(colnames(TM)=="BANKS"),t]=-PerditaB[t]
      #delta bond
      TM[which(rownames(TM)=="deltaBond"),which(colnames(TM)=="GOVERNMENT"),t]=bond[t]-bondrepayment[t]
      if(t>1){
        TM[which(rownames(TM)=="deltaBond"),which(colnames(TM)=="CB"),t]=-(BondCB[t]-BondCB[t-1])
        TM[which(rownames(TM)=="deltaBond"),which(colnames(TM)=="CAPITALISTS"),t]=-(BondDemand_cap[t]-BondDemand_cap[t-1])
      }else{
        TM[which(rownames(TM)=="deltaBond"),which(colnames(TM)=="CB"),t]=-BondCB[t] 
        TM[which(rownames(TM)=="deltaBond"),which(colnames(TM)=="CAPITALISTS"),t]=-BondDemand_cap[t]
      }
      #delta Hb
      if(t>1){
        TM[which(rownames(TM)=="deltaHb"),which(colnames(TM)=="CB"),t]=Hb[t]-Hb[t-1]
        TM[which(rownames(TM)=="deltaHb"),which(colnames(TM)=="BANKS"),t]=-(Hb[t]-Hb[t-1])
      }else{
        TM[which(rownames(TM)=="deltaHb"),which(colnames(TM)=="CB"),t]=Hb[t]
        TM[which(rownames(TM)=="deltaHb"),which(colnames(TM)=="BANKS"),t]=-Hb[t]
      }
      
      #CONSISTENCY CHECK ROWs
      TM[which(rownames(TM)=="Consumption"),which(colnames(TM)=="SUM"),t]=round(sum(TM[which(rownames(TM)=="Consumption"),,t]),digits=4)
      TM[which(rownames(TM)=="Investments"),which(colnames(TM)=="SUM"),t]=round(sum(TM[which(rownames(TM)=="Investments"),,t]),digits=4)
      TM[which(rownames(TM)=="GovExp"),which(colnames(TM)=="SUM"),t]=round(sum(TM[which(rownames(TM)=="GovExp"),,t]),digits=4)
      TM[which(rownames(TM)=="Tax"),which(colnames(TM)=="SUM"),t]=round(sum(TM[which(rownames(TM)=="Tax"),,t]),digits=4)
      TM[which(rownames(TM)=="Wages"),which(colnames(TM)=="SUM"),t]=round(sum(TM[which(rownames(TM)=="Wages"),,t]),digits=4)
      TM[which(rownames(TM)=="Profits"),which(colnames(TM)=="SUM"),t]=round(sum(TM[which(rownames(TM)=="Profits"),,t]),digits=4)
      TM[which(rownames(TM)=="Recapital"),which(colnames(TM)=="SUM"),t]=round(sum(TM[which(rownames(TM)=="Recapital"),,t]),digits=4)
      TM[which(rownames(TM)=="IntOnLoans&ov"),which(colnames(TM)=="SUM"),t]=round(sum(TM[which(rownames(TM)=="IntOnLoans&ov"),,t]),digits=4)
      TM[which(rownames(TM)=="IntCreditConsumption"),which(colnames(TM)=="SUM"),t]=round(sum(TM[which(rownames(TM)=="IntCreditConsumption"),,t]),digits=4)
      TM[which(rownames(TM)=="UnBenefit"),which(colnames(TM)=="SUM"),t]=round(sum(TM[which(rownames(TM)=="UnBenefit"),,t]),digits=4)
      TM[which(rownames(TM)=="InterestDeposit"),which(colnames(TM)=="SUM"),t]=round(sum(TM[which(rownames(TM)=="InterestDeposit"),,t]),digits=4)
      TM[which(rownames(TM)=="ProfitCB"),which(colnames(TM)=="SUM"),t]=round(sum(TM[which(rownames(TM)=="ProfitCB"),,t]),digits=4)
      TM[which(rownames(TM)=="InterestHb"),which(colnames(TM)=="SUM"),t]=round(sum(TM[which(rownames(TM)=="InterestHb"),,t]),digits=4)
      TM[which(rownames(TM)=="InterestAdv"),which(colnames(TM)=="SUM"),t]=round(sum(TM[which(rownames(TM)=="InterestAdv"),,t]),digits=4)
      TM[which(rownames(TM)=="InterestBond"),which(colnames(TM)=="SUM"),t]=round(sum(TM[which(rownames(TM)=="InterestBond"),,t]),digits=4)
      TM[which(rownames(TM)=="deltadepositi"),which(colnames(TM)=="SUM"),t]=round(sum(TM[which(rownames(TM)=="deltadepositi"),,t]),digits=4)
      TM[which(rownames(TM)=="deltaloan"),which(colnames(TM)=="SUM"),t]=round(sum(TM[which(rownames(TM)=="deltaloan"),,t]),digits=4)
      TM[which(rownames(TM)=="deltacreditCons"),which(colnames(TM)=="SUM"),t]=round(sum(TM[which(rownames(TM)=="deltacreditCons"),,t]),digits=4)
      TM[which(rownames(TM)=="deltaBond"),which(colnames(TM)=="SUM"),t]=round(sum(TM[which(rownames(TM)=="deltaBond"),,t]),digits=4)
      TM[which(rownames(TM)=="deltaAdvances"),which(colnames(TM)=="SUM"),t]=round(sum(TM[which(rownames(TM)=="deltaAdvances"),,t]),digits=4)
      TM[which(rownames(TM)=="deltanonperforming"),which(colnames(TM)=="SUM"),t]=round(sum(TM[which(rownames(TM)=="deltanonperforming"),,t]),digits=4)
      TM[which(rownames(TM)=="deltaHb"),which(colnames(TM)=="SUM"),t]=round(sum(TM[which(rownames(TM)=="deltaHb"),,t]),digits=4)
      
      
      
      #CONSISTENCY CHECK COLUMNSs
      TM[which(rownames(TM)=="SUM"),which(colnames(TM)=="WORKERS"),t]=round(sum(TM[,which(colnames(TM)=="WORKERS"),t]),4)
      TM[which(rownames(TM)=="SUM"),which(colnames(TM)=="CAPITALISTS"),t]=round(sum(TM[,which(colnames(TM)=="CAPITALISTS"),t]),4)
      TM[which(rownames(TM)=="SUM"),which(colnames(TM)=="FIRMSK"),t]=round(sum(TM[,which(colnames(TM)=="FIRMSK"),t]),4)
      TM[which(rownames(TM)=="SUM"),which(colnames(TM)=="FIRMSC"),t]=round(sum(TM[,which(colnames(TM)=="FIRMSC"),t]),4)
      TM[which(rownames(TM)=="SUM"),which(colnames(TM)=="BANKS"),t]=round(sum(TM[,which(colnames(TM)=="BANKS"),t]),4)
      TM[which(rownames(TM)=="SUM"),which(colnames(TM)=="GOVERNMENT"),t]=round(sum(TM[,which(colnames(TM)=="GOVERNMENT"),t]),4)
      TM[which(rownames(TM)=="SUM"),which(colnames(TM)=="CB"),t]=round(sum(TM[,which(colnames(TM)=="CB"),t]),4)
      TM[which(rownames(TM)=="SUM"),which(colnames(TM)=="SUM"),t]=round(sum(TM[,which(colnames(TM)=="SUM"),t]),4)
      
      
      
    }
  }
}
# End ####
#toc()


#  plot(u_raggregate[300:T],type='l',col=3,lwd=1,lty=3,main='',xaxt='n',yaxt='n',ylab = NA,xlab = NA)
#  axis(side = 4)
#  legend("bottom",c("GDP","u_r (aggregate)"),  bty = "n", cex = 0.9, lty=c(1,3), lwd=c(2,2), col = c(1,3), box.lwd=0)
#  

# 
# mypath <- file.path("C:","Users","bnldd","Desktop","graph2",paste("myplot_", conta, ".png", sep = ""))
# 
# png(file=mypath)
# mytitle = paste("my title is", conta)
# 
# m=rbind(c(1,1),c(2,3))
# layout(m)
# par(tcl=-0.2,mgp=c(2,0.4,0))
# par(mar=c(0,1.3,0.2,1))
# plot(10:T, y[10:T,2], type='l',ylim=range(y[10:T,2]), col=1,main='',xaxt='n')
# #plot(g_GDP, type='l',ylim=range(g_GDP), col=1,main='',xaxt='n')
# title("y_c",line=-1)
# plot(10:T, u_r[10:T,2], type='l',ylim=range(u_r[10:T,2]), col=1,main='',xaxt='n')
# plot(Deb_Pil, type='l',ylim=range(Deb_Pil), col=1,main='',xaxt='n')
# plot(10:T, inv[10:T,2], type='l',ylim=range(inv[10:T,2]), col=1,main='',xaxt='n')
# #title("Trend_deb_Pil",line=-1)
# #plot(Def_Pil2, type='l',ylim=range(Def_Pil2), col=1,main='',xaxt='n')
# #title("Trend_def_Pil/GDP",line=-1)
# legend("topright",ncol=6, legend = c("phi",phivalue,"beta",betavalue,"sigma",sigmavalue,"v",vvalue,"consh", consh,"cons",cons),cex = 0.8,box.lty=0)
# 
# 
# 
# dev.off()
#conta=conta+1



#c(NW_workers[1:T])

#return(result[,trial])
sss=mean(u_raggregate[500:T])

# #  data.frame(yc=yc,GDP=GDP,InvInvestmentR=InvestmentR,Consumptionhh=Consumptionhh,Employeestot=Employeestot,debpil=debpil,Def_Pil2=Def_Pil2,priceindexC=priceindexC)
#c(GDPreal,priceindexC,u_raggregate,parametri[trial,],sss,alfasort[NFC],nbancarottac) 
# c(GDPreal,parametri[trial,])   
# 
#  }
#  numCores <- detectCores()
# cl <- makeCluster(numCores)
# # # 
#   clusterEvalQ(cl, library(VGAM))
#   clusterExport(cl,c("parametri","dati","datiIn","progval","ff","ncomb","eterogeneita","sceltastocastica","sceltastocasticaK"))
#   res=parSapply(cl,trial, ff)
#   stopCluster(cl)

# save.image("/nobackup/bnldd/10maggio/viva1.RData")     

#save(GDPreal,file="C:/Users/lored/Desktop/GDPcomputer.Rdata")

### Grafici Generali #####

# #G
#  plot(GDPreal[b:T],main='GDPreal',ylim=range(GDPreal[a:T]),type='l',lwd=1,lty=1,ylab='',xlab='')
# plot(markupindexC[b:T],ylim=range(markupindexC[a:T]),main='Markup index - Sector C',type='l')
# #plot(priceindexC[b:T],ylim=range(priceindexC[a:T]),main='price index - Sector C',type='l')
# 
# plot(Gcons[b:T],main='Gcons',ylim=range(Gcons[a:T]),type='l',lwd=1,lty=1,ylab='',xlab='')
# par(new=TRUE)
# plot(Ubenefit[b:T],main='Ubenefit',ylim=range(Ubenefit[a:T]),type='l',yaxt='n',lwd=1,lty=1,ylab='',xlab='')
# axis(side = 4)
# 
# plot(interestpaymentG[b:T],main='interest',ylim=range(interestpaymentG[a:T]),type='l',lwd=1,lty=1,ylab='',xlab='')
# 
# plot(log(GDPreal[b:T]),main='GDPreal',ylim=range(log(GDPreal[a:T])),type='l',lwd=1,lty=1,ylab='',xlab='')
# 
# 
# plot(log(debpil[b:T]), type='l',col=4,lty=3,ylim=range(log(debpil[b:T])),lwd=2,main='',ylab='',xlab='')
GDPusa=read.csv("C:/Users/lored/Desktop/GDP.csv",sep=",")
realGDPusa=GDPusa[,2]
groWthUSA=matrix(data=0, nrow=(length(realGDPusa)-1))

for(i in 2:length(realGDPusa)){
  groWthUSA[i]=(realGDPusa[i]-realGDPusa[i-1])/realGDPusa[i-1]
}

#options(scipen=999)
GDPusa.ts=as.ts(log(realGDPusa),frequency=1,start=2)
GDPusa.hp=hpfilter(GDPusa.ts,freq=1600,type="lambda")
varlogusa=var(GDPusa.hp$cycle)







m=rbind(c(1),c(2))
layout(m)
par(tcl=-0.2,mgp=c(2,0.4,0))
par(mar=c(1.4,1.8,3.6,2))


b=350
#smaGDpreal=SMA(GDPreal[b:T])
mingdp=min(GDPreal[b:T])*0.97
maxgdp=max(GDPreal[b:T])*1.03

#plot(unsatisfieddemand[1:T], type='l',lty=3,lwd=2,col=2,main='',xaxt='n')
#abline(h=0,col="purple",lty=2,lwd=2)
plot(GDPreal[b:T], type='l',col=1,lwd=1,lty=2,main='GDP real',ylim=range(mingdp,maxgdp),ylab='')
#par(new=TRUE)
#plot(smaGDpreal, type='l',ylim=range(mingdp,maxgdp),col=6,lty=3,lwd=2,main='',ylab='')
#par(new=TRUE)
#plot(priceindexC[b:T], type='l',col=5,lty=3,lwd=2,main='',ylab='')
plot(u_raggregate[b:T], type='l',col=2,lty=3,ylim=range(u_raggregate[b:T]),lwd=2,main='',ylab='',xlab='',font.main =1)
#axis(side = 4)

#beep()

#plot(log(GDPreal[b:T]), type='l',col=6,lty=1,lwd=2,main='',ylab='')
# 
#  u=20
#  regmodel=lm(GDP[u:T]~G[u:T])
#  summary(regmodel)
#  #mi=min(GDPreal)
#  regmodel2=lm(GDP[u:T]~G[u:T]+G[(u-1):(T-1)]+G[(u-2):(T-2)])
#  summary(regmodel2)
#  
#  regmodel3=lm(GDP[u:T]~G[u:T]+G[(u-1):(T-1)]+G[(u-2):(T-2)]+G[(u-3):(T-3)]+G[(u-4):(T-4)]+G[(u-5):(T-5)])
#  summary(regmodel3)
#  
# print(mean(GDP[b:T]))
# 
# print("var-log GDPusa:")
# print(varlogusa)
# print("var-log artifical:")
# print(var(log(GDPreal[200:T])))
# print("var-gdp USA")
# print(round(var(groWthUSA),4))
# print("var-gdp Artificial")
# print(var(g_GDPreal[200:T]))



if(FALSE){
  par(mar=c(3,3,3,3))
  b=400
  a=150
  #T=800
  plot(GDPreal[b:T], type='l',col=4,lty=1,ylim=range(25000,27500),lwd=1,main='',ylab='',xlab='',font.main =1)
  title("Credit to consumption and GDP",line=0.7,cex.main=1.1,font.main =1)
  par(new=TRUE)
  plot(markupindexC[b:T],type='l',col=1,lty=3,ylim=range(markupindexC[a:T]),yaxt="n",lwd=1,main='price C',ylab='',xlab='')
  axis(side = 4)
  
  #plot(ratio[b:T,2],type='l',col=1,lty=3,ylim=range(ratio[b:T,2]),lwd=1,main='Mean difference (??-??)',ylab='',xlab='')
  #plot(meanratio[b:T],type='l',col=1,lty=3,ylim=range(meanratio[b:T]),lwd=1,main='price C',ylab='',xlab='')
  
  
  #plot(creditconsumptionB[b:T],col=3,lty=3,main='',yaxt="n",ylim=range(creditconsumptionB[a:T]),type='l',ylab='',xlab='')
  #legend("bottomright", legend = c("GDP", "Credit"), col = c(4,3), lty = 3, cex = 0.8,box.lty=0)
  
  plot(GDPreal[b:T], type='l',col=4,lty=1,ylim=range(GDPreal[a:T]),lwd=1,main='',ylab='',xlab='',font.main =1)
  par(new=TRUE)
  #plot(creditconsumptionB[b:T],col=3,lty=3,main='',yaxt="n",ylim=range(creditconsumptionB[a:T]),type='l',ylab='',xlab='')
  plot(GDPA[b:T], type='l',col=5,lty=1,ylim=range(GDPreal[b:T]),lwd=1,main='',ylab='',xlab='',font.main =1)
  
  plot(u_raggregate[b:T], type='l',col=5,lty=1,ylim=range(u_raggregate[b:T]),lwd=1,main='',ylab='',xlab='',font.main =1)
}


if(FALSE){
  plot(cwpost[1:T],main='cw',ylim=range(cwpost[1:T]),type='l')
  plot(ccappost[1:T],main='ccap',ylim=range(ccappost[1:T]),type='l')
  plot(GDPreal[1:T],main='GDPreal',ylim=range(GDPreal[1:T]),type='l',ylab='',xlab='')
  
  ## MarkupC
  plot(markupindexC[b:T],ylim=range(markupindexC[a:T]),main='Markup index - Sector C',type='l')
  plot(realyhworkers[b:T],ylim=range(realyhworkers[a:T]),main='realyhworkers',type='l')
  plot(realyhcapitalist[b:T],ylim=range(realyhcapitalist[a:T]),main='realyhcapitalist',type='l')
  plot(consumptionemployed[b:T],ylim=range(consumptionemployed[a:T]),main='consumptionemployed',type='l')
  plot(yhdemployed[b:T],ylim=range(yhdemployed[a:T]),main='yhdemployed',type='l')
  
  
  
  plot(pastbaskettotworkers[b:T],ylim=range(pastbaskettotworkers[a:T]),main='pastbaskettotworkers',type='l')
  plot(pastbaskettotcapitalists[b:T],ylim=range(pastbaskettotcapitalists[a:T]),main='pastbaskettotcapitalists',type='l')
  
  
  plot(yc[b:T,1],ylim=range(yc[a:T,1]),main='yc',type='l')
  
  
  
  plot(markupindexK[b:T],ylim=range(markupindexK[a:T]),main='Markup index - Sector k',type='l')
  
  
  #legend("topleft",ncol=5,legend=c("z",z,"dk",dk,"perc",perc))
  plot(priceindexC[b:T],ylim=range(priceindexC[a:T]),main='priceindexC',type='l')
  plot(wageaggregate[b:T],ylim=range(wageaggregate[a:T]),main='wage nominal',type='l')
  plot(y_hcap[b:T],ylim=range(y_hcap[a:T]),main='Profitti',type='l')
}

if(FALSE){    
  b=400
  # T=600
  f=T
  a=230
  x<-(b:f)
  
  m=rbind(c(1,2),c(3,4))
  layout(m)
  par(tcl=-0.2,mgp=c(2,0.4,0))
  par(mar=c(1.5,1.5,1.5,1.5))
  
  plot(yc[b:f], type='l',col=1,lty=1,ylim=range(yc[a:T]),lwd=1,main='yc',ylab='',xlab='')
  plot(markupindexC[b:f], type='l',col=1,lty=1,ylim=range(markupindexC[a:T]),lwd=1,main='markupc',xaxt='n',ylab='',xlab='')
  #plot(markupindexK[b:f], type='l',col=1,lty=1,ylim=range(markupindexK[a:T]),lwd=1,main='markupk',xaxt='n',ylab='',xlab='')
  plot(Employeestot[b:f], type='l',col=1,lty=1,ylim=range(Employeestot[a:T]),lwd=1,main='Employees',xaxt='n',ylab='',xlab='')
  plot(wgov[b:f], type='l',col=1,lty=1,ylim=range(wgov[a:T]),lwd=1,main='wgov',xaxt='n',ylab='',xlab='')
  plot(GDPreal[b:f], type='l',col=1,lty=1,ylim=range(GDPreal[a:T]),lwd=1,main='GDPreal',xaxt='n',ylab='',xlab='')
  plot(inflazione[b:f], type='l',col=1,lty=1,ylim=range(inflazione[a:T]),lwd=1,main='inflazione',xaxt='n',ylab='',xlab='')
  plot(intrate[b:f], type='l',col=1,lty=1,ylim=range(intrate[a:T]),lwd=1,main='intrate',xaxt='n',ylab='',xlab='')
  plot(Def_Pil2[b:f], type='l',col=1,lty=1,ylim=range(Def_Pil2[a:f]),lwd=1,main='Def/GDP',xaxt='n',ylab='',xlab='')
  plot(debpil[b:f], type='l',col=4,lty=3,ylim=range(debpil[b:f]),lwd=2,main='',ylab='',xlab='')
  
  
  
  m=rbind(c(1))
  layout(m)
  par(tcl=0,mgp=c(2,0.4,0))
  par(mar=c(2,2,2,2))
  attach(mtcars)
  plot(debpil[b:T]*10, markupindexC[b:T],main="Debt/GDP and markup correlation",xlab="Unemployment ", ylab="Inflation", pch=4) 
  #abline(lm(inflazione[b:T]~Une[b:T]), col="red") # regression line (y~x)
  lines(lowess(debpil[b:T]*10,markupindexC[b:T]), col="blue",lwd=1,lty=1) 
  
  
  
  plot(debpil[b:f], type='l',col=4,lty=3,ylim=range(debpil[b:f]),lwd=2,main='',ylab='',xlab='')
  par(new=TRUE)
  plot(markupindexC[b:f], type='l',col=1,lty=1,ylim=range(0.39,0.6),lwd=1,main='Debt/GDP and markup dynamics',xaxt='n',yaxt='n',ylab='',xlab='')
  axis(side = 4)
  legend("bottomright",c("markup","Debt/GDP"),  bty = "n", cex = 1.1, lty=c(1,3), lwd=c(1,1), col = c(1,4), box.lwd=0)
  
  
  
  m=rbind(c(1))
  layout(m)
  par(tcl=0,mgp=c(2,0.4,0))
  par(mar=c(2,2,2,2))
  b=300
  # T=600
  f=T
  a=100
  
  ccf(intrate[b:T], inflazione[b:T], lag.max=40, plot=TRUE, main="",main.font=2)
  title("Inflation and interest rate lagged correlation", line = 0.5)
  ccf(intrate[b:T], markupindexC[b:T], lag.max=40, plot=TRUE, main="Flow & Rain")
  
  
  plot(GDP[b:f], type='l',col=4,lty=3,ylim=range(53500,57000),lwd=2,main='',ylab='',xlab='')
  par(new=TRUE)
  plot(GDPreale[b:f,1], type='l',col=1,lty=2,ylim=range(53500,57000),lwd=2,main='',ylab='',xlab='')
  legend("right",c("GDP (1 %)","GDP (2 %)"),  bty = "n", cex = 1.2, lty=c(3,2), lwd=c(2,2), col = c(4,1), box.lwd=0)
  
  plot(debpil[b:f]*10, type='l',col=4,lty=3,ylim=range(9,15),lwd=2,main='',ylab='',xlab='')
  par(new=TRUE)
  plot(debpilgen[b:f,1]*10, type='l',col=1,lty=2,ylim=range(9,15),lwd=2,main='',ylab='',xlab='')
  legend("right",c("Deb/GDP (1 %)","Deb/GDP (2 %)"),  bty = "n", cex = 1.2, lty=c(3,2), lwd=c(2,2), col = c(4,1), box.lwd=0)
  
  plot(yc[b:f], type='l',col=4,lty=3,ylim=range(6960,7100),lwd=2, yaxt='n',main='GDP at constant price (1%)',ylab='',xlab='')
  par(new=TRUE)
  plot(ygeneral[b:f,1], type='l',col=1,lty=2,ylim=range(6960,7100),lwd=2,yaxt='n',main='',ylab='',xlab='')
  legend("bottomright",c("GDP (1 %)","GDP (2 %)"),  bty = "n", cex = 1.2, lty=c(3,2), lwd=c(2,2), col = c(4,1), box.lwd=0)
  
  for(t in 1:T){
    GDPreal[t]=sum(y[t,which(cod==0)])*priceK[1]+sum(y[t,which(cod==1)])*priceC[1]
  }
  
  plot(GDPreale[b:f,2], type='l',col=4,lty=3,ylim=range(53500,54200),lwd=2,main='GDP at constant price (1%)',ylab='',xlab='')
  par(new=TRUE)
  plot(GDPreal[b:f], type='l',col=1,lty=2,ylim=range(53500,54200),lwd=2,main='',ylab='',xlab='')
  legend("topright",c("GDP (1 %)","GDP (2 %)"),  bty = "n", cex = 1.2, lty=c(3,2), lwd=c(2,2), col = c(4,1), box.lwd=0)
  
  
  
  
  m=rbind(c(1,1),c(2,2))
  layout(m)
  # par(tcl=0,mgp=c(2,0.4,0))
  par(mar=c(1.6,1.6,1.6,1.6))
  # 
  #GDP
  b=390
  a=170
  
  #plot(GDP[b:f], type='l',col=1,lty=1,ylim=range(GDP[a:T]),lwd=1,main='GDP',xaxt='n',ylab='',xlab='')
  
  plot(GDPreal[b:f], type='l',col=1,lty=1,ylim=range(53700,54100),lwd=1,main='GDPreal',xaxt='n',ylab='',xlab='')
  plot(debpil[b:f]*10, type='l',col=1,lty=1,ylim=range(debpil[a:f]*10),lwd=1,main='Deb/GDP',xaxt='n',ylab='',xlab='')
  par(new=TRUE)
  plot(debpilreal[b:f]*10, type='l',col=3,lty=2,ylim=range(8.5,10),lwd=1,main='',xaxt='n',yaxt='n',ylab='',xlab='')
  axis(side = 4)
  legend("top",ncol=2,c("Deb/GDP real","Deb/GDP nominal"),  bty = "n", cex = 0.9, lty=c(2,1), lwd=c(1,1), col = c(3,1), box.lwd=0)
  
  plot(deb_yc[b:f], type='l',col=1,lty=1,ylim=range(deb_yc[a:T]),lwd=1,main='deb_yc',xaxt='n',ylab='',xlab='')
  
  minimo=matrix(data=0, ncol=NFC)
  massimo=matrix(data=0, ncol=NFC)
  count=1
  for(j in (NFK+1):NF){
    minimo[count]=min(alfak[b:T,j])
    massimo[count]=max(alfak[b:T,j])
    count=count+1
  }
  mingen=min(minimo)
  maxgen=max(massimo)
  
  varianzacapacità=matrix(data=0,ncol=NF)
  
  m=rbind(c(1))
  layout(m)
  par(mar=c(2,2,2,2))
  
  colors=seq(NFC)
  count=1
  for(i in(NFK+1):NF){
    
    varianzacapacità[i]=var(alfak[b:T,i])
    # plot(alfak[b:T,i], type='l',col=count,lty=1,ylim=range(mingen,maxgen),lwd=1,main='alfak',xaxt='n',ylab='',xlab='')
    #  par(new=TRUE)
    # count=count+1
  }
  
  b=300
  avgvarianza=(sum(varianzacapacità[(NFK+1):NF])/NFC)*1000
  
  varianzagdp=var(yc[b:T])
  
  avgGDP=sum(yc[b:T])/length(yc[b:T])
  avgdebpil=sum(debpil[b:T])/(length(debpil[b:T]))
  
  avginflation=sum(inflazione[b:T])/(length(inflazione[b:T]))
  
  
  avggrowth=sum(g_yc[b:T])/(length(g_yc[b:T]))
  
  inf=matrix(data=0,nrow=T)
  for(i in 1:T){
    inf[i]=(1+inflazione[i])^12-1
  }
  
  
  
  
  avginflation2=sum(inf[b:T])/(length(inf[b:T]))
  
  avgint=sum(intrate[b:T])/(length(intrate[b:T]))
  
  
  m=rbind(c(1))
  layout(m)
  par(mar=c(2,2,2,2))
  b=300
  a=220
  plot(ygeneral[b:T,1], type='l',col=1,lty=2,ylim=range(6900,7030),lwd=1,main='GDP real',ylab='',xlab='')
  par(new=TRUE)
  plot(yc[b:T], type='l',col=6,lty=3,ylim=range(6900,7030),lwd=1,main='',xaxt='n',ylab='',xlab='')
  legend("bottom",ncol=2,c("Taylor rule","Fix interest rate"),  bty = "n", cex = 1.1, lty=c(2,3), lwd=c(1,1), col = c(1,6), box.lwd=0)
  
  
  
  
  plot(ygeneral[b:T,3], type='l',col=6,lty=3,ylim=range(6845,7030),lwd=1,main='',xaxt='n',ylab='',xlab='')
  par(new=TRUE)
  plot(ygeneral[b:T,1], type='l',col=4,lty=4,ylim=range(6845,7030),lwd=1,main='',xaxt='n',ylab='',xlab='')
  legend("top",ncol=3,c("HR","MR","LR"),  bty = "n", cex = 0.9, lty=c(4,2,3), lwd=c(1,1,1), col = c(4,1,6), box.lwd=0)
  
  
  
  
  #Philips curve
  m=rbind(c(1))
  layout(m)
  par(mar=c(2,2,2,2))
  b=200
  attach(mtcars)
  plot(Une[b:T], inf[b:T],main="Philips curve",xlab="Unemployment ", ylab="Inflation", pch=1) 
  #abline(lm(inflazione[b:T]~Une[b:T]), col="red") # regression line (y~x)
  lines(lowess(Une[b:T],inf[b:T]), col="blue",lwd=1,lty=1) 
  
  
  
  regmodel=lm(inf[b:f]~Une[b:T])
  regmodel
  
  
  
  
  #plot(markupindexK[b:f], type='l',col=1,lty=1,ylim=range(markupindexK[a:T]),lwd=1,main='markupk',xaxt='n',ylab='',xlab='')
  #plot(averagewageAggregate[b:f], type='l',col=1,lty=1,ylim=range(averagewageAggregate[a:T]),lwd=1,main='avgw',xaxt='n',ylab='',xlab='')
  plot(wgov[b:f], type='l',col=1,lty=1,ylim=range(wgov[a:T]),lwd=1,main='wgov',xaxt='n',ylab='',xlab='')
  plot(lperworkmedio[b:f], type='l',col=1,lty=1,ylim=range(lperworkmedio[a:T]),lwd=1,main='lperworkmedio',xaxt='n',ylab='',xlab='')
  plot(Employeestot[b:f], type='l',col=1,lty=1,ylim=range(Employeestot[a:T]),lwd=1,main='Employees',xaxt='n',ylab='',xlab='')
  
  
  
  
  
  m=rbind(c(1))
  b=180
  #Finanza pubblica nei due scenri considerati ->varizione del peso del settore pubblico nell'economia
  debpil1=res[(5*T+b):(6*T),1]
  deficit1=res[(6*T+b):(7*T),1]
  debpil2=res[(5*T+b):(6*T),2]
  deficit2=res[(6*T+b):(7*T),2]
  
  debpil1.ts=as.ts(debpil1,frequency=1,start=2)
  debpil2.ts=as.ts(debpil2,frequency=1,start=2)
  deficit1.ts=as.ts(deficit1,frequency=1,start=2)
  deficit2.ts=as.ts(deficit2,frequency=1,start=2)
  
  debpil1.hp=hpfilter(debpil1.ts,freq=1600,type="lambda")
  debpil2.hp=hpfilter(debpil2.ts,freq=1600,type="lambda")
  deficit1.hp=hpfilter(deficit1.ts,freq=1600,type="lambda")
  deficit2.hp=hpfilter(deficit2.ts,freq=1600,type="lambda")
  
  
  #plot trend DebPil
  plot(debpil1.hp$trend, type='l',ylim=range(2.5,7), col=1,main='',xaxt='n')
  par(new=TRUE)
  plot(debpil2.hp$trend, type='l',ylim=range(2.5,7), col=1,lty=2 ,main='',xaxt='n')
  
  #plot trend deficit
  plot(deficit1.hp$trend[100:length(deficit1.hp$trend)],ylim=range(-50,800), type='l',col=1,main='',xaxt='n')
  par(new=TRUE)
  plot(deficit2.hp$trend[100:length(deficit2.hp$trend)],ylim=range(-50,800), type='l',col=1,lty=2 ,main='',xaxt='n')
  
  
  #Finanza pubblica in ogni scenario
  #Scenario1
  ## 
  GDP1=res[(T+b):(2*T),1]
  GDP1.ts=as.ts(GDP1,frequency=1,start=2)
  GDP1.hp=hpfilter(GDP1.ts,freq=1600,type="lambda")
  
  
  
  ##Deficit GDP
  plot(GDP1,type='l',col=1,lwd=1,main='',xaxt='n')
  par(new=TRUE)
  plot(deficit1,xaxt='n',yaxt='n',type='l',col=9,lwd=2,lty=3,font.main=1,cex.main=0.75,main="",ylab = NA,xlab = NA)
  axis(side = 4)
  #Trend GDP
  
  
  ##DebpiL-GDP
  plot(GDP1.hp$cycle,type='l',col=1,lwd=2,main='',xaxt='n')
  par(new=TRUE)
  plot(debpil1.hp$cycle,xaxt='n',yaxt='n',type='l',col=2,lwd=2,lty=3,font.main=1,cex.main=0.75,main="",ylab = NA,xlab = NA)
  axis(side = 4)
  
  
  #Scenario2
  GDP2=res[(T+b):(2*T),2]
  GDP2.ts=as.ts(GDP2,frequency=1,start=2)
  GDP2.hp=hpfilter(GDP2.ts,freq=1600,type="lambda")
  
  #Deficit - GDP
  plot(GDP2,type='l',col=1,lwd=2,main='',xaxt='n')
  par(new=TRUE)
  plot(deficit2,xaxt='n',yaxt='n',type='l',col=9,lwd=2,lty=3,font.main=1,cex.main=0.75,main="",ylab = NA,xlab = NA)
  axis(side = 4)
  ##DebpiL-GDP
  plot(GDP2.hp$cycle,type='l',col=1,lwd=2,main='',xaxt='n')
  par(new=TRUE)
  plot(debpil2.hp$cycle,xaxt='n',yaxt='n',type='l',col=2,lwd=2,lty=3,font.main=1,cex.main=0.75,main="",ylab = NA,xlab = NA)
  axis(side = 4)
  
  
  save.image("/nobackup/bnldd/salarioaggregato/varianza.RData")
  
  
  #u_raggregate[t]=sum(k_used[t,])/ktot
  
  #plot(u_raggregate[b:t],ylim=range(u_raggregate[a:T]),main='u_r',type='l')
  
  
  
  yc.ts=as.ts(log(yc[b:T]),frequency=1,start=2)
  GDP.ts=as.ts(log(GDP[b:T]),frequency=1,start=2)
  in.ts=as.ts(log(produzionek[b:T]),frequency=1,start=2)
  invalue.ts=as.ts(log(Investmentvalue[b:T]),frequency=1,start=2)
  consh.ts=as.ts(log(Consumptionhh[b:T]),frequency=1,start=2)
  GDP.hp=hpfilter(GDP.ts,freq=1600,type="lambda")
  yc.hp=hpfilter(yc.ts,freq=1600,type="lambda")
  in.hp=hpfilter(in.ts,freq=1600,type="lambda")
  invalue.hp=hpfilter(invalue.ts,freq=1600,type="lambda")
  consh.hp=hpfilter(consh.ts,freq=1600,type="lambda")
  
  
  # Grafici componenti cicliche ####
  m=rbind(c(1))
  layout(m)
  par(tcl=-0.2,mgp=c(0,0.4,0))
  par(mar=c(1.5,2,2,1.5))
  plot(in.hp$cycle,type='l',ylim=range(-0.01,0.012),lty=2,col=8,lwd=2,ylab = NA,xlab = NA, main="Componenti cicliche")
  par(new="TRUE")
  plot(yc.hp$cycle,type='l',ylim=range(-0.01,0.012),lty=3,col=6,ylab = NA,xlab = NA)
  par(new="TRUE")
  plot(GDP.hp$cycle,type='l',ylim=range(-0.01,0.012),lty=1,ylab = NA,xlab = NA)
  legend("bottomright",c("GDP","Investments","Consumption"),  bty = "n", cex = 1, lty=c(1,2,3), lwd=c(2,2,2), col = c(1,8,6), box.lwd=0)
  
  
  
  
  
  # validazione empirica ####
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
  
  
  
  
  # Autocorrelazioni ####
  m=rbind(c(1,1),c(2,3))
  layout(m)
  par(tcl=0,mgp=c(2,0.4,0))
  par(mar=c(1.2,1.2,1.5,0.8))
  # 
  
  #yc.ts=as.ts(res[b:T,conta],frequency=1,start=2)
  GDP.ts=as.ts(GDPreal[b:T],frequency=1,start=2)
  in.ts=as.ts(produzionek[b:T],frequency=1,start=2)
  ch.ts=as.ts(Consumptionhh[b:T],frequency=1,start=2)
  
  # production=matrix(data=0, nrow=length(in.ts))
  # for(t in 1:length(in.ts)){
  #   if(t>=dk){
  #     for(i in (t-dk+1):t){
  #       production[t]=production[t]+in.ts[i]/dk
  #     }
  #   }else{
  #     production[t]=sum(in.ts[1:t])/dk
  #   }
  # }
  
  
  #filtro dati simulati
  GDP.hp=hpfilter(GDP.ts,freq=1600,type="lambda")
  #yc.hp=hpfilter(yc.ts,freq=1600,type="lambda")
  in.hp=hpfilter(in.ts,freq=1600,type="lambda")
  ch.hp=hpfilter(ch.ts,freq=1600,type="lambda")
  
  
  #GDP
  aGDP=acf(GDP.hp$cycle, lag.max=lagg, plot=FALSE)
  aGDP=as.numeric(unlist(aGDP))[1:(lagg+1)]
  
  plot(aGDP,ylim=range(-1,1), type='l')
  par(new="TRUE")
  plot(aGDPr,ylim=range(-1,1), type='l',lty=2)
  #title("GDP",line=-1)
  title("GDP",line=-1,font.main=1,cex.main=1.2)
  areaGDP=sum(abs(aGDP-aGDPr))
  legend("bottomright",c("Artificial","USA data"),  bty = "n", cex = 1, lty=c(1,2), lwd=c(2,2), col = c(1,1), box.lwd=0)
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
  
  #BS[,,T]
  # TM[,,T]
  
  
  
  GDPmean=matrix(data=0, nrow=T,ncol=1)
  sdGDPmean=matrix(data=0, nrow=T,ncol=1)
  for (t in 1:T){
    GDPmean[t]=mean(res[t,])
    sdGDPmean[t]=sd(res[t,])
  }
  
  
  
  # mediana
  b=400
  T=700
  GDPmedian=matrix(data=0, nrow=(T-b),ncol=1)
  GDPmad=matrix(data=0, nrow=(T-b),ncol=1)
  for (t in b:T){
    GDPmedian[t]=median(res[t,])
    GDPmad[t]=mad(res[t,], center = median(res[t,]), constant = 1.4826, na.rm = FALSE,  low = FALSE, high = FALSE)
  }
  
  GDPup=GDPmedian+2*GDPmad
  GDPdown=GDPmedian-2*GDPmad
  
  a=0.0005
  plot(GDPdown[b:T],type = 'l',lty=3,col=8,lwd=1,ylab = NA,xlab = NA,ylim=range(GDPdown[b:t]*(1-a),GDPup[b:t]*(1+a)))
  axis(side = 4)
  par(new=TRUE)
  plot(GDPup[b:T],type = 'l',lty=3,col=8,lwd=1,ylab = NA,xlab = NA,ylim=range(GDPdown[b:t]*(1-a),GDPup[b:t]*(1+a)))
  
  #lines(GDPdown[b:T],GDPup[b:T])
  #polygon(cbind(c(min(GDPdown[b:T]), GDPdown[b:T], max(GDPdown[b:T])), c(min(GDPup[b:T]), GDPup[b:T], min(GDPup[b:T]))), col="#00CC66")
  
  x=1:(T-b+1)
  polygon(c(x,rev(x)),c(GDPdown[b:T],rev(GDPup[b:T])),col="grey95", border = NA)
  par(new=TRUE)
  plot(GDPmedian[b:T], type='l',lty=1,col=1,lwd=1,ylab = NA,xlab = NA,ylim=range(GDPdown[b:t]*(1-a),GDPup[b:t]*(1+a)))
}
