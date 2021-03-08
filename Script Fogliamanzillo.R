#comandi per lettura file
#metodo 1: importa file excel già pulito
library(readxl)
dati_rid <- read_excel("dati_rid.xlsx") #read_excel("C:/Users/Francesco/Desktop/dati_rid.xlsx")
View(dati_rid)
dati<-dati_rid[,-c(4,8,30,32,35)]
dim(dati)
View(dati)
dati <-as.data.frame(dati)

#metodo 2: importa file txt
INSPROFDOTTRIC_Microdati_2018 <- read.delim("INSPROFDOTTRIC_Microdati_2018.txt")
View(INSPROFDOTTRIC_Microdati_2018)
INSPROFDOTTRIC_Microdati_2018
Questionario<-INSPROFDOTTRIC_Microdati_2018
table(Questionario$anno_dott)
# 2012 2014 
# 8172 7885 

# coorte che ha conseguito il dottorato di ricerca nel 2014
coorte<- subset(Questionario, Questionario$anno_dott == 2014)
coorte
View(coorte)
dim(coorte)
str(coorte)

#i seguenti passaggi sono necessari solo per file txt (il file excel è già pulito)
#elimino variabili con molti na 
dataset<-coorte[,-c(1,4,7,8,12,17,18,19,24,25,27,28,29,38,39,40,41,42,43,44,45,46,47,48,50,51,52,53,54,55,57,58,61,62,63,64,65,66,67,69,70,71,72,73,77,79,81,83,84,87,88,89,90,101,102,103,104,105,106,107,108,109,110,111,112,113,114,116,118,119,120,121,122,123,125,126,128,131,132,137,138,139,140,141,142,143,144,146,148,153,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,195,196,197,201,202,203,204)]
View(dataset)
dim(dataset)
# elimino unità con na
dati<-na.omit(dataset)
dim(dati)
dim(dati)
View(dati)
dimnames(dati)
row.names(dati) <- 1 : 6469

#le variabili sono numerate in base al file excel
#nei commenti ci sono le numerazioni per il file txt
#multicub prima batteria di domande
library("CUB", lib.loc="~/R/win-library/3.6")

#### Correzioni RS
vett<-c("qcorsi_s",    "ncorsi_s",   "doce_s",      "spazi_s",     "train_s","collab_s",   "incora_s" ,   "dot_s")
listord1<-Y
  dati[,match(vett,colnames(dati))] #17:24 prima batteria file txt 
for (j in 1:ncol(listord1)){
  listord1[,j]<-listord1[,j]-1
  ord<-listord1[,j]
  vett<-which(ord>=4)
  listord1[vett,j]<-ord[vett]-1
}  #da
vett<-c("qcorsi_s",    "ncorsi_s",   "doce_s",      "spazi_s",     "train_s","collab_s",   "incora_s" ,   "dot_s")
multicub(listord1, labels = vett,main ="CUB models on Dottori di Ricerca data set", pch = 19, pos = c(1,rep(3, ncol(listord1)-1)),ylim=c(0,1),xlim=c(0,1))
## fine correzioni RS

#multicub seconda batteria di domande
###### #Correzione RS 
listord2<- # dati[,c(32,33,34,35,36,37,38,39)] #38:45 seconda batteria file txt
for (j in 1:ncol(listord2)){
  listord2[,j]<-listord2[,j]-1
  ord<-listord2[,j]
  vett<-which(ord>=4)
  listord2[vett,j]<-ord[vett]-1
}  
labels<-names(dati)[38:45] 
multicub(listord2, labels = labels,
         caption ="CUB models on Dottori di Ricerca data set", pch = 19, pos = c(1,rep(3, ncol(listord)-1)),ylim=c(0.15,1),xlim=c(0.15,1))

#multicub entrambe le batterie
listord12<-dati[,c(17:24,38:45)]
labels<-names(dati)[c(17:24,38:45)] 
multicub(listord, labels = labels,
         caption ="CUB models on Dottori di Ricerca data set",pch = 19, pos = c(1,rep(3, ncol(listord)-1)),ylim=c(0.15,1),xlim=c(0.15,1))
## fine correzioni RS (potrebbe differenziare i due gruppi di item con colori o simboli diversi)

#analisi esplorativa delle variabili che presentano uncertainty e feeling piu elevato
hist(dati$dot_s)#+feel
hist(dati$doce_s) #+feel
hist(dati$ncorsi_s)#+uncert
hist(dati$qcorsi_s)#+uncert
hist(dati$auton_s)#+feel
hist(dati$sodd)#+feel
hist(dati$att_s)#+feel
hist(dati$stab_s) #un po di shelter e +uncert
hist(dati$carrie_s)#+uncert
tabulate(dati$stab_s)
tabulate(dati$carrie_s)


#pacchetto fastCUB
library(FastCUB)

## correzione RS
m<-8 # non m=10
# inoltre, nel dataset 'dati', la variabile 'sodd' ha delle categorie mancanti, bisogna attingere a quelle modificate
# non c'è bisogno di 'with(dati,...)' se poi comunque usa dati$ etc per ogni variabile
effe<-Formula(listord2$sodd~dati$eta_dott+dati$votolau+dati$viverip|dati$sesso+dati$ateneo_rip+dati$eta_dott)
#modello di test
cub<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub)
BIC(cub)

#parametri
param<-coef(cub)
param


#fastcub best subset
dip<-listord2$sodd
m<-8
Y<-dati[,33:36] #variabili numerate dal file excel
W<-dati[,20:24]

## Search for the best CUB model with covariates only for feeling
best0q<-bestcub(dip,m,Y=NULL,W=W,toler=1e-4,maxiter=100,iterc=5,alpha=0.05,invgen=TRUE)
str(best0q)
summary(best0q$bestmodel)

# fine correzioni RS


## Search for the best CUB model with covariates only for uncertainty
bestp0<-bestcub(dip,m,Y,W=NULL,toler=1e-4,maxiter=100,iterc=5,alpha=0.05,invgen=TRUE)
## Search for the best CUB model with covariates for both parameters
bestpq<-bestcub(dip,m,Y,W,toler=1e-4,maxiter=100,iterc=5,alpha=0.05, invgen=TRUE,
                mix=TRUE,tolmix=1e+3,fmix=1)
summary(bestpq)
final<-bestpq$bestmodel
summary(final)
#Error in dimnames(x) <- dn : 
#length of 'dimnames' [1] not equal to array extent





