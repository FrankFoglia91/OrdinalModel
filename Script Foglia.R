library(tidyverse)
library(janitor)
library(ggeffects)
library(FastCUB)
library(CUB)
library(MASS)
library(Cairo)
library(VGAM)

INSPROFDOTTRIC_Microdati_2018 <- read.delim("C:/Users/Francesco/Desktop/TESI/Dataset/INSPROFDOTTRIC_Microdati_2018.txt")
View(INSPROFDOTTRIC_Microdati_2018)
Questionario<-INSPROFDOTTRIC_Microdati_2018
table(Questionario$anno_dott)

#selezione coorte 2014
coorte <-filter(Questionario, anno_dott ==2014 )  #rimettere 2014
coorte2012<-filter(Questionario, !anno_dott ==2014 )  
dim(coorte)
dim(coorte2012)
### Analisi esplorativa dei dati
vedi rmarkdown


Questionario %>% 
  tabyl(anno_dott)

# Y=Batteria di domande sulla soddisfazione per il dottorato 
listord1<- select(coorte, qcorsi_s:dot_s)
for (j in 1:ncol(listord1)){
  listord1[,j]<-listord1[,j]-1
  ord<-listord1[,j]
  vett<-which(ord>=4)
  listord1[vett,j]<-ord[vett]-1
} 
class(listord1)
     
     
hist(listord1$qcorsi_s) #8 valori
hist(listord1$ncorsi_s)
hist(listord1$doce_s)
hist(listord1$spazi_s)
hist(listord1$train_s)
hist(listord1$collab_s)
hist(listord1$incora_s)
hist(listord1$dot_s)
hist(listord1$dot_s)

# Z=batteria di domande sulla soddisfazione per il lavoro
listord2<- select(coorte, att_s:sodd)
for (j in 1:ncol(listord2)){
  listord2[,j]<-listord2[,j]-1
  ord<-listord2[,j]
  vett<-which(ord>=4)
  listord2[vett,j]<-ord[vett]-1
}

tabulate(listord2$att_s)
tabulate(listord2$stab_s)
tabulate(listord2$auton_s)
tabulate(listord2$know_s)
tabulate(listord2$redd_s)
tabulate(listord2$carrie_s)
tabulate(listord2$prof_s)
tabulate(listord2$sodd)
hist(listord2$sodd)
     
#trasformo le covariate in dicotomiche 
#prima batteria 
qcorsi_d<-ifelse(listord1$qcorsi_s <= 7 , 0 , 1)
ncorsi_d<-ifelse(listord1$ncorsi_s <= 7 , 0 , 1)
doce_d<-ifelse(listord1$doce_s <= 7 , 0 , 1)
spazi_d<-ifelse(listord1$spazi_s <= 7 , 0 , 1)
train_d<-ifelse(listord1$train_s <= 7 , 0 , 1)
collab_d<-ifelse(listord1$collab_s <= 7 , 0 , 1) 
incora_d<-ifelse(listord1$incora_s <= 7 , 0 , 1)
dot_d<-ifelse(listord1$dot_s <= 7 , 0 , 1)
     
#2a batteria 
att_d<-ifelse(listord2$att_s <= 7 , 0 , 1)
stab_d<-ifelse(listord2$stab_s <= 7 , 0 , 1) 
auton_d<-ifelse(listord2$auton_s <= 7 , 0 , 1)
know_d<-ifelse(listord2$know_s <= 7 , 0 , 1)
redd_d<-ifelse(listord2$redd_s <= 7 , 0 , 1)
carrie_d<-ifelse(listord2$carrie_s <= 7 , 0 , 1)
prof_d<-ifelse(listord2$prof_s <= 7 , 0 , 1)
sodd_d<-ifelse(listord2$sodd <= 7 , 0 , 1)
     
###############################################
#trasformo le covariate in dicotomiche
# da 6 in giù
#prima batteria 
qcorsi_d<-ifelse(listord1$qcorsi_s <= 6 , 0 , 1)
ncorsi_d<-ifelse(listord1$ncorsi_s <= 6 , 0 , 1)
doce_d<-ifelse(listord1$doce_s <= 6 , 0 , 1)
spazi_d<-ifelse(listord1$spazi_s <= 6 , 0 , 1)
train_d<-ifelse(listord1$train_s <= 6 , 0 , 1)
collab_d<-ifelse(listord1$collab_s <= 6 , 0 , 1) 
incora_d<-ifelse(listord1$incora_s <= 6 , 0 , 1)
dot_d<-ifelse(listord1$dot_s <= 6 , 0 , 1)

#2a batteria 
att_d<-ifelse(listord2$att_s <= 6 , 0 , 1)
stab_d<-ifelse(listord2$stab_s <= 6 , 0 , 1) 
auton_d<-ifelse(listord2$auton_s <= 6 , 0 , 1)
know_d<-ifelse(listord2$know_s <= 6 , 0 , 1)
redd_d<-ifelse(listord2$redd_s <= 6 , 0 , 1)
carrie_d<-ifelse(listord2$carrie_s <= 6 , 0 , 1)
prof_d<-ifelse(listord2$prof_s <= 6 , 0 , 1)
sodd_d<-ifelse(listord2$sodd <= 6 , 0 , 1)
####################################################     
###############################################
    
#trasformo le covariate in dicotomiche
# da 5 in giù
#prima batteria 
qcorsi_d<-ifelse(listord1$qcorsi_s <= 5 , 0 , 1)
ncorsi_d<-ifelse(listord1$ncorsi_s <= 5 , 0 , 1)
doce_d<-ifelse(listord1$doce_s <= 5 , 0 , 1)
spazi_d<-ifelse(listord1$spazi_s <= 5 , 0 , 1)
train_d<-ifelse(listord1$train_s <= 5 , 0 , 1)
collab_d<-ifelse(listord1$collab_s <= 5 , 0 , 1) 
incora_d<-ifelse(listord1$incora_s <= 5 , 0 , 1)
dot_d<-ifelse(listord1$dot_s <= 5 , 0 , 1)
 
#2a batteria 
att_d<-ifelse(listord2$att_s <= 5 , 0 , 1)
stab_d<-ifelse(listord2$stab_s <= 5 , 0 , 1) 
auton_d<-ifelse(listord2$auton_s <= 5 , 0 , 1)
know_d<-ifelse(listord2$know_s <= 5 , 0 , 1)
redd_d<-ifelse(listord2$redd_s <= 5 , 0 , 1)
carrie_d<-ifelse(listord2$carrie_s <= 5 , 0 , 1)
prof_d<-ifelse(listord2$prof_s <= 5 , 0 , 1)
sodd_d<-ifelse(listord2$sodd <= 5 , 0 , 1)
####################################################     
###############################################
     
#trasformo le covariate in dicotomiche
# da 5 in giù
#prima batteria 
qcorsi_d<-ifelse(listord1$qcorsi_s <= 4 , 0 , 1)
ncorsi_d<-ifelse(listord1$ncorsi_s <= 4 , 0 , 1)
doce_d<-ifelse(listord1$doce_s <= 4 , 0 , 1)
spazi_d<-ifelse(listord1$spazi_s <= 4 , 0 , 1)
train_d<-ifelse(listord1$train_s <= 4 , 0 , 1)
collab_d<-ifelse(listord1$collab_s <= 4 , 0 , 1) 
incora_d<-ifelse(listord1$incora_s <= 4 , 0 , 1)
dot_d<-ifelse(listord1$dot_s <= 4 , 0 , 1)

#2a batteria 
att_d<-ifelse(listord2$att_s <= 4 , 0 , 1)
stab_d<-ifelse(listord2$stab_s <= 4 , 0 , 1) 
auton_d<-ifelse(listord2$auton_s <= 4 , 0 , 1)
know_d<-ifelse(listord2$know_s <= 4 , 0 , 1)
redd_d<-ifelse(listord2$redd_s <= 4 , 0 , 1)
carrie_d<-ifelse(listord2$carrie_s <= 4 , 0 , 1)
prof_d<-ifelse(listord2$prof_s <= 4 , 0 , 1)
sodd_d<-ifelse(listord2$sodd <= 4 , 0 , 1)
####################################################     
     
#per trattare le covariate come var ordinali (fattori)
#trasformo le modalità in m-1  variabili dicotomiche
# 
#prima batteria 
# la valutazione 1 viene scaricata nell'intercetta constraints
#qcorsi
qcorsi_2<-ifelse(listord1$qcorsi_s == 2 , 1 , 0)
qcorsi_3<-ifelse(listord1$qcorsi_s == 3 , 1 , 0)
qcorsi_4<-ifelse(listord1$qcorsi_s == 4 , 1 , 0)
qcorsi_5<-ifelse(listord1$qcorsi_s == 5 , 1 , 0)
qcorsi_6<-ifelse(listord1$qcorsi_s == 6 , 1 , 0)
qcorsi_7<-ifelse(listord1$qcorsi_s == 7 , 1 , 0)
qcorsi_8<-ifelse(listord1$qcorsi_s == 8 , 1 , 0)
tabyl(qcorsi_8)
tabyl(listord1$qcorsi_s)

#ncorsi
ncorsi_2<-ifelse(listord1$ncorsi_s == 2 , 1 , 0)
ncorsi_3<-ifelse(listord1$ncorsi_s == 3 , 1 , 0)
ncorsi_4<-ifelse(listord1$ncorsi_s == 4 , 1 , 0)
ncorsi_5<-ifelse(listord1$ncorsi_s == 5 , 1 , 0)
ncorsi_6<-ifelse(listord1$ncorsi_s == 6 , 1 , 0)
ncorsi_7<-ifelse(listord1$ncorsi_s == 7 , 1 , 0)
ncorsi_8<-ifelse(listord1$ncorsi_s == 8 , 1 , 0)
tabyl(ncorsi_8)
tabyl(listord1$ncorsi_s)

#doce
doce_2<-ifelse(listord1$doce_s == 2 , 1 , 0)
doce_3<-ifelse(listord1$doce_s == 3 , 1 , 0)
doce_4<-ifelse(listord1$doce_s == 4 , 1 , 0)
doce_5<-ifelse(listord1$doce_s == 5 , 1 , 0)
doce_6<-ifelse(listord1$doce_s == 6 , 1 , 0)
doce_7<-ifelse(listord1$doce_s == 7 , 1 , 0)
doce_8<-ifelse(listord1$doce_s == 8 , 1 , 0)
tabyl(doce_8)
tabyl(listord1$doce_s)

# spazi
spazi_2<-ifelse(listord1$spazi_s == 2 , 1 , 0)
spazi_3<-ifelse(listord1$spazi_s == 3 , 1 , 0)
spazi_4<-ifelse(listord1$spazi_s == 4 , 1 , 0)
spazi_5<-ifelse(listord1$spazi_s == 5 , 1 , 0)
spazi_6<-ifelse(listord1$spazi_s == 6 , 1 , 0)
spazi_7<-ifelse(listord1$spazi_s == 7 , 1 , 0)
spazi_8<-ifelse(listord1$spazi_s == 8 , 1 , 0)
tabyl(spazi_8)
tabyl(listord1$spazi_s)

#train
train_2<-ifelse(listord1$train_s == 2 , 1 , 0)
train_3<-ifelse(listord1$train_s == 3 , 1 , 0)
train_4<-ifelse(listord1$train_s == 4 , 1 , 0)
train_5<-ifelse(listord1$train_s == 5 , 1 , 0)
train_6<-ifelse(listord1$train_s == 6 , 1 , 0)
train_7<-ifelse(listord1$train_s == 7 , 1 , 0)
train_8<-ifelse(listord1$train_s == 8 , 1 , 0)
tabyl(train_8)
tabyl(listord1$train_s)

#collab
collab_2<-ifelse(listord1$collab_s == 2 , 1 , 0)
collab_3<-ifelse(listord1$collab_s == 3 , 1 , 0)
collab_4<-ifelse(listord1$collab_s == 4 , 1 , 0)
collab_5<-ifelse(listord1$collab_s == 5 , 1 , 0)
collab_6<-ifelse(listord1$collab_s == 6 , 1 , 0)
collab_7<-ifelse(listord1$collab_s == 7 , 1 , 0)
collab_8<-ifelse(listord1$collab_s == 8 , 1 , 0)
tabyl(collab_8)
tabyl(listord1$collab_s)

#incora
incora_2<-ifelse(listord1$incora_s == 2, 1 , 0)
incora_3<-ifelse(listord1$incora_s == 3, 1 , 0)
incora_4<-ifelse(listord1$incora_s == 4, 1 , 0)
incora_5<-ifelse(listord1$incora_s == 5, 1 , 0)
incora_6<-ifelse(listord1$incora_s == 6, 1 , 0)
incora_7<-ifelse(listord1$incora_s == 7, 1 , 0)
incora_8<-ifelse(listord1$incora_s == 8, 1 , 0)
tabyl(incora_8)
tabyl(listord1$incora_s)


#2a batteria 
#att 
att_2<-ifelse(listord2$att_s == 2, 1 , 0)
att_3<-ifelse(listord2$att_s == 3, 1 , 0)
att_4<-ifelse(listord2$att_s == 4, 1 , 0)
att_5<-ifelse(listord2$att_s == 5, 1 , 0)
att_6<-ifelse(listord2$att_s == 6, 1 , 0)
att_7<-ifelse(listord2$att_s == 7, 1 , 0)
att_8<-ifelse(listord2$att_s == 8, 1 , 0)
tabyl(att_8)
tabyl(listord2$att_s)

#stab
stab_2<-ifelse(listord2$stab_s == 2, 1 , 0)
stab_3<-ifelse(listord2$stab_s == 3, 1 , 0)
stab_4<-ifelse(listord2$stab_s == 4, 1 , 0)
stab_5<-ifelse(listord2$stab_s == 5, 1 , 0)
stab_6<-ifelse(listord2$stab_s == 6, 1 , 0)
stab_7<-ifelse(listord2$stab_s == 7, 1 , 0)
stab_8<-ifelse(listord2$stab_s == 8, 1 , 0)
tabyl(stab_8)
tabyl(listord2$stab_s)


#auton
auton_2<-ifelse(listord2$auton_s == 2, 1 , 0)
auton_3<-ifelse(listord2$auton_s == 3, 1 , 0)
auton_4<-ifelse(listord2$auton_s == 4, 1 , 0)
auton_5<-ifelse(listord2$auton_s == 5, 1 , 0)
auton_6<-ifelse(listord2$auton_s == 6, 1 , 0)
auton_7<-ifelse(listord2$auton_s == 7, 1 , 0)
auton_8<-ifelse(listord2$auton_s == 8, 1 , 0)
tabyl(auton_8)
tabyl(listord2$auton_s)

#know
know_2<-ifelse(listord2$know_s == 2, 1 , 0)
know_3<-ifelse(listord2$know_s == 3, 1 , 0)
know_4<-ifelse(listord2$know_s == 4, 1 , 0)
know_5<-ifelse(listord2$know_s == 5, 1 , 0)
know_6<-ifelse(listord2$know_s == 6, 1 , 0)
know_7<-ifelse(listord2$know_s == 7, 1 , 0)
know_8<-ifelse(listord2$know_s == 8, 1 , 0)
tabyl(know_8)
tabyl(listord2$know_s)

#redd
redd_2<-ifelse(listord2$redd_s == 2, 1 , 0)
redd_3<-ifelse(listord2$redd_s == 3, 1 , 0)
redd_4<-ifelse(listord2$redd_s == 4, 1 , 0)
redd_5<-ifelse(listord2$redd_s == 5, 1 , 0)
redd_6<-ifelse(listord2$redd_s == 6, 1 , 0)
redd_7<-ifelse(listord2$redd_s == 7, 1 , 0)
redd_8<-ifelse(listord2$redd_s == 8, 1 , 0)
tabyl(redd_8)
tabyl(listord2$redd_s)

#carrie
carrie_2<-ifelse(listord2$carrie_s == 2, 1 , 0)
carrie_3<-ifelse(listord2$carrie_s == 3, 1 , 0)
carrie_4<-ifelse(listord2$carrie_s == 4, 1 , 0)
carrie_5<-ifelse(listord2$carrie_s == 5, 1 , 0)
carrie_6<-ifelse(listord2$carrie_s == 6, 1 , 0)
carrie_7<-ifelse(listord2$carrie_s == 7, 1 , 0)
carrie_8<-ifelse(listord2$carrie_s == 8, 1 , 0)
tabyl(carrie_8)
tabyl(listord2$carrie_s)

# prof
prof_2<-ifelse(listord2$prof_s == 2, 1 , 0)
prof_3<-ifelse(listord2$prof_s == 3, 1 , 0)
prof_4<-ifelse(listord2$prof_s == 4, 1 , 0)
prof_5<-ifelse(listord2$prof_s == 5, 1 , 0)
prof_6<-ifelse(listord2$prof_s == 6, 1 , 0)
prof_7<-ifelse(listord2$prof_s == 7, 1 , 0)
prof_8<-ifelse(listord2$prof_s == 8, 1 , 0)
tabyl(prof_8)
tabyl(listord2$prof_s)

####################################################     



     
     
Y_dicot<-cbind(qcorsi_d,ncorsi_d,doce_d, spazi_d, train_d, collab_d,incora_d,dot_d,
            att_d,stab_d,auton_d, know_d,redd_d,carrie_d,prof_d,sodd_d)
X_ordinali<-cbind(qcorsi_2,qcorsi_3,qcorsi_4,qcorsi_5,qcorsi_6,qcorsi_7,qcorsi_8,
                  ncorsi_2,ncorsi_3,ncorsi_4,ncorsi_5,ncorsi_6,ncorsi_7,ncorsi_8,
                  doce_2,doce_3,doce_4,doce_5,doce_6,doce_7,doce_8,
                  spazi_2,spazi_3,spazi_4,spazi_5,spazi_6,spazi_7,spazi_8,
                  train_2,train_3,train_4,train_5,train_6,train_7,train_8,
                  collab_2,collab_3,collab_4,collab_5,collab_6,collab_7,collab_8,
                  incora_2,incora_3,incora_4,incora_5,incora_6,incora_7,incora_8,
                  att_2,att_3,att_4,att_5,att_6,att_7,att_8,
                  stab_2,stab_3,stab_4,stab_5,stab_6,stab_7,stab_8,
                  auton_2,auton_3,auton_4,auton_5,auton_6,auton_7,auton_8,
                  know_2,know_3,know_4,know_5,know_6,know_7,know_8,
                  redd_2,redd_3,redd_4,redd_5,redd_6,redd_7,redd_8,
                  carrie_2,carrie_3,carrie_4,carrie_5,carrie_6,carrie_7,carrie_8,
                  prof_2,prof_3,prof_4,prof_5,prof_6,prof_7,prof_8)

X_ind<-select(coorte, c(sesso, eta_dott, stciv, figlio ))
X_dott<-select(coorte, c(fuoric, didatt, borsa, 	paper,
                      estero_d, ateneo_rip, riplav, ore, cod_area_w,
                      rifare_d, itaest_l, lavora, tipo_lav_princ,ate7mod,
                      condiz, ricerca, prog_ric, viverip))
     
     
X_ind$sesso<-ifelse(X_ind$sesso==1  , 0 , 1) #0 maschi 1 femmine
X_dott$fuoric<-ifelse(X_dott$fuoric==1  , 0, 1) #0no 1fuoric
X_ind$figlio<-ifelse(X_ind$figlio==2  , 0 , 1) #0 no figli 1 si figli
X_dott$didatt<-ifelse(X_dott$didatt ==3  , 0 , 1) #1impegnato 0 Non impegnato in didat
X_ind$stciv<-ifelse(X_ind$stciv==2  , 1 ,0 )#  1= SPOSATO  0=NON SPOSATO
X_dott$borsa<-ifelse(X_dott$borsa==3  , 0 , 1)#1 Borsa 0 NO borsa
X_dott$estero_d<-ifelse(X_dott$estero_d==4 , 0 , 1) #0estero no durante il dott 1estero si durante il dott
X_dott$itaest_l<-ifelse(X_dott$itaest_l==1  , 0 , 1)#0Italia 1ESTERO
X_dott$lavora<-ifelse(X_dott$lavora==3  , 0 , 1)#0=NON LAVORA 1=LAVORA
X_dott$ricerca<-ifelse(X_dott$ricerca==3  , 0 , 1)#0no lav ricerca 1=si lavora ricerca
X_dott$prog_ric<-ifelse(X_dott$prog_ric==2  , 0 , 1)#0no prog_ric 1=si prog_ric
   
X_ind$eta_dott<-as.factor(X_ind$eta_dott)
X_ind$eta_dott <- fct_collapse(X_ind$eta_dott, 
                "Minore o uguale di 28"  = c("1"),
                "Maggiore a 28" = c("2","3","4"))
X_ind$eta_dott<-ifelse(X_ind$eta_dott == "Minore o uguale di 28" , 0 , 1)#0 eta<=28 1 eta>28 
     
     
X_dott$ateneo_rip<-as.factor(X_dott$ateneo_rip)
X_dott$ateneo_rip <- fct_collapse(X_dott$ateneo_rip, 
                         "Nord"   = c("1", "2"),
                         "Centro" = c("3"),
                         "Sud" = c("4", "5"))
ateneoNord<-ifelse(X_dott$ateneo_rip== "Nord" , 1 , 0)#0 no 1 si 
ateneoCentro<-ifelse(X_dott$ateneo_rip== "Centro" , 1 , 0)#0 no 1 si
ateneoSud<-ifelse(X_dott$ateneo_rip== "Sud" , 1 , 0)#0 no 1 si

X_dott$riplav<-as.factor(X_dott$riplav)

table(X_dott$riplav)
class(X_dott$riplav) 
X_dott$riplav <- fct_collapse(X_dott$riplav, 
                     "Nord"   = c("1", "2"),
                     "Centro" = c("3"),
                     "Sud" = c("4", "5"),
                     "Estero" =c("9"))

lavoroNord<-ifelse(X_dott$riplav== "Nord" , 1 , 0)#0 no 1 si 
lavoroCentro<-ifelse(X_dott$riplav== "Centro" , 1 , 0)#0 no 1 si
lavoroSud<-ifelse(X_dott$riplav== "Sud" , 1 , 0)#0 no 1 si
lavoroEstero<-ifelse(X_dott$riplav== "Estero" , 1 , 0)#0 no 1 si
     
     
table(is.na(X_dott$viverip))
     
X_dott$viverip<-as.factor(X_dott$viverip)
X_dott$viverip<-as.factor(X_dott$viverip)
class(X_dott$viverip) 
X_dott$viverip <- fct_collapse(X_dott$viverip, 
                      "Nord"   = c("1", "2"),
                      "Centro" = c("3"),
                      "Sud" = c("4", "5"),
                      "Estero" =c("9"))
     
viveNord<-ifelse(X_dott$viverip== "Nord" , 1 , 0)#0 no 1 si 
viveCentro<-ifelse(X_dott$viverip== "Centro" , 1 , 0)#0 no 1 si
viveSud<-ifelse(X_dott$viverip== "Sud" , 1 , 0)#0 no 1 si
viveEstero<-ifelse(X_dott$viverip== "Estero" , 1 , 0)#0 no 1 si
table(viveNord)#2955
table(X_dott$viverip=="Nord")
table(viveCentro)#1893
table(X_dott$viverip=="Centro")
table(viveSud)#1783
table(X_dott$viverip=="Sud")
table(viveEstero)#1254
table(X_dott$viverip=="Estero")

     
X_dott$cod_area_w<-as.factor(X_dott$cod_area_w)
X_dott$cod_area_w <- fct_collapse(X_dott$cod_area_w, 
           "Scienze MM FF NN" = c("1", "2","3","4","5"),
           "Scienze mediche" = c("6"),
           "Scienze agrarie e veterinarie" = c("7"),
           "Ingegneria " = c("8", "9"),
           "Scienze umanistiche e psicologiche"=c("10","11"),
           "Scienze giuridiche" = c("12"),
           "Scienze economiche e statistiche" =c("13"),
           "Scienze politiche e sociali"=c("14") )
#una va tenuta fuori come categoria di riferimento
Scienze_MMFFNN<-ifelse(X_dott$cod_area_w== "Scienze MM FF NN" , 1 , 0)#0 no 1 si 
Scienze_mediche<-ifelse(X_dott$cod_area_w== "Scienze mediche" , 1 , 0)#0 no 1 si
Scienze_agrarie<-ifelse(X_dott$cod_area_w== "Scienze agrarie e veterinarie" , 1 , 0)#0 no 1 si
Ingegneria<-ifelse(X_dott$cod_area_w== "Ingegneria" , 1 , 0)#0 no 1 si
Scienze_uman_psico<-ifelse(X_dott$cod_area_w== "Scienze umanistiche e psicologiche" , 1 , 0)#0 no 1 si
Sc_giurid<-ifelse(X_dott$cod_area_w== "Scienze giuridiche" , 1 , 0)#0 no 1 si
Sc_econ_stat<-ifelse(X_dott$cod_area_w== "Scienze economiche e statistiche" , 1 , 0)#0 no 1 si
Sc_pol_social<-ifelse(X_dott$cod_area_w== "Scienze politiche e sociali" , 1 , 0)#0 no 1 si



class(X_dott$cod_area_w)

X_dott$rifare_d<-as.factor(X_dott$rifare_d)
X_dott$rifare_d <- fct_collapse(X_dott$rifare_d, 
                   "Si"   = c("1"),
                   "No" = c("2"),
                 "Non so" = c("3"))

rifare_dott<-ifelse(X_dott$rifare_d== "Si" , 1 , 0)#0 no 1 si
non_rifare_dott<-ifelse(X_dott$rifare_d== "No" , 1 , 0)#0 no 1 si
incerto_rifare_dott<-ifelse(X_dott$rifare_d== "Non so" , 1 , 0)#0 no 1 si

X_dott$tipo_lav_princ<-as.factor(X_dott$tipo_lav_princ)
X_dott$tipo_lav_princ <- fct_collapse(X_dott$tipo_lav_princ, 
                         "Lavoro dipendente" = c("1"),
                         "Lavoro autonomo" = c("4"),
                       "Altro" = c("2","3","5"))

lav_dip<-ifelse(X_dott$tipo_lav_princ== "Lavoro dipendente" , 1 , 0)#0 no 1 si
lav_aut<-ifelse(X_dott$tipo_lav_princ== "Lavoro autonomo" , 1 , 0)#0 no 1 si
altro_tipolav<-ifelse(X_dott$tipo_lav_princ== "Altro" , 1 , 0)#0 no 1 si
table(X_dott$tipo_lav_princ)


X_dott$ate7mod<-as.factor(X_dott$ate7mod)
X_dott$ate7mod <- fct_collapse(X_dott$ate7mod, 
                      "Agricoltura" = c("1"),
                      "Industria" = c("2"),
                      "Istruzione" = c("3","4"),
                      "R&S"=c("5","6"),
                      "Altro"=c("7"))

agricolt<-ifelse(X_dott$ate7mod== "Agricoltura" , 1 , 0)#0 no 1 si
indust<-ifelse(X_dott$ate7mod== "Industria" , 1 , 0)#0 no 1 si
istruz<-ifelse(X_dott$ate7mod== "Istruzione" , 1 , 0)#0 no 1 si
ricer_svil<-ifelse(X_dott$ate7mod== "R&S" , 1 , 0)#0 no 1 si
altro_settore<-ifelse(X_dott$ate7mod== "Altro" , 1 , 0)#0 no 1 si


X_dott2<-select(X_dott, c(fuoric, didatt, borsa, paper,
                 estero_d,  ore, ricerca, prog_ric))


D<-cbind(ateneoNord, ateneoCentro, ateneoSud ,lavoroNord, lavoroSud, lavoroCentro, lavoroEstero,
        viveNord, viveCentro, viveSud, viveEstero, Scienze_MMFFNN, Scienze_mediche, Scienze_agrarie, Ingegneria, Scienze_uman_psico,
        Sc_giurid, Sc_econ_stat, Sc_pol_social, rifare_dott, non_rifare_dott, incerto_rifare_dott, 
        lav_aut, lav_dip, altro_tipolav, agricolt, indust, istruz, ricer_svil, altro_settore )

D<-as.data.frame(D)

dati<-cbind(listord1, listord2, Y_dicot,X_ordinali,X_ind, D,X_dott2)
class(dati)
table(complete.cases(dati))

#eliminare na
dati<-na.omit(dati)
dim(dati)
table(is.na(dati))
#FALSE 
#458490 
dati<-dati[complete.cases(dati),]
dim(dati)
     
     
sud<- filter(dati, viveSud==1 ) 
nord<-filter(dati, viveNord==1 )
centro<- filter(dati, viveCentro==1 )
estero<-filter(dati, viveEstero==1)
table(viveNord)#2955
table(viveCentro)#1893
table(viveSud)#1783
table(viveEstero)#1254
     
#effe<-Formula(dati$sodd~dati$sesso+dati$eta_dott+dati$stciv+dati$figlio+dati$figlio
#+dati$estero_d+dati$ateneoNord+ dati$ateneoCentro+ dati$ateneoSud + dati$lavoroNord+ dati$lavoroSud+ dati$lavoroCentro+
#dati$lavoroEstero+ dati$ore +dati$cod_area_w +dati$rifare_d+
#dati$itaest_l+dati$lavora+dati$tipo_lav_princ+dati$ate7mod+dati$condiz+dati$ricerca
#+dati$prog_ric|0)
     
######################################################
#best model sodd dott intero campione
effe<-Formula(dati$dot_s~dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)
#covar NO DICOTOM
effe<-Formula(dati$dot_s~dati$train_s|dati$qcorsi_s+dati$ncorsi_s+dati$doce_s+dati$spazi_s+dati$train_s+dati$collab_s+dati$incora_s)

###dicotm 7
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)     
#dicotom a 6
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)
# dicotom a 5
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)
#### dicotom a 4
effe<-Formula(dati$dot_s~dati$train_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)
#sodd lav per dott
dati$qcorsi_s+dati$ncorsi_s+dati$doce_s+dati$spazi_s+dati$train_s+dati$collab_s+dati$incora_s
effe<-Formula(dati$sodd~dati$spazi_s+dati$collab_s|dati$qcorsi_s+dati$ncorsi_s+dati$spazi_s+dati$collab_s+dati$incora_s)
effe<-with(nord, Formula(sodd~ncorsi_s+incora_s|spazi_s+collab_s+incora_s))
qcorsi_s+ncorsi_s+doce_s+spazi_s+train_s+collab_s+incora_s
effe<-with(centro, Formula(sodd~ncorsi_s+collab_s|ncorsi_s+spazi_s+incora_s))

effe<-with(sud, Formula(sodd~spazi_s|qcorsi_s))


dati$qcorsi_s+dati$ncorsi_s+dati$doce_s+dati$spazi_s+dati$train_s+dati$collab_s+dati$incora_s

effe<-Formula(dati$dot_s~dati$train_s|dati$train_s+dati$qcorsi_s+dati$ncorsi_s+dati$doce_s+dati$spazi_s+dati$collab_s+dati$incora_s)

bestcub_dot<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bestcub_dot)
BIC(bestcub_dot)


###################################################
#best model dott nord
### NO DICOTOM
effe<-Formula(nord$dot_s~nord$train_s|nord$qcorsi_s+nord$ncorsi_s+nord$doce_s+nord$spazi_s+nord$train_s+nord$collab_s+nord$incora_s)

######
#dicotom a 7
effe<-with(nord, Formula(dot_s~collab_d+incora_d|qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d))

#dicotom a 6
effe<-with(nord,Formula(dot_s~qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d|qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d))

effe<-with(nord,Formula(dot_s~train_d+collab_d+incora_d|qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d))
################# dicotom a 5
effe<-with(nord,Formula(dot_s~doce_d+train_d|qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d))

bestcub_dotnord<-fastCUB(effe,data=nord,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bestcub_dotnord)
BIC(bestcub_dotnord)

xtable(summary(bestcub_dotnord))

#best model dott centro
#no dicotom
effe<-with(centro, Formula(dot_s~0|qcorsi_s+ncorsi_s+doce_s+train_s+incora_s))
qcorsi_s+ncorsi_s+doce_s+spazi_s+train_s+collab_s+incora_s

#dicotom a 7
effe<-with(centro,Formula(dot_s~doce_d+incora_d|qcorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d))

#dicotom a 6
effe<-with(centro,Formula(dot_s~qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d|qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d))

effe<-with(centro,Formula(dot_s~doce_d+collab_d|qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d))
#######

bestcub_dot<-fastCUB(effe,data=centro,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bestcub_dot)
BIC(bestcub_dot)

xtable(summary(bestcub_dot))

#best model dott sud

# NO DICOTOM
effe<-with(sud,Formula(dot_s~0|qcorsi_s+ncorsi_s+doce_s+spazi_s+train_s+collab_s+incora_s))

## dicotom a 7
effe<-with(sud,Formula(dot_s~doce_d+incora_d|qcorsi_d+ncorsi_d+doce_d+spazi_d+collab_d+incora_d))

#dicotom a 6
effe<-with(sud,Formula(dot_s~qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d|qcorsi_d+ncorsi_d+doce_d+spazi_d+collab_d+incora_d))

effe<-with(sud,Formula(dot_s~doce_d+train_d|qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d))
###
bestcub_dot<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bestcub_dot)
BIC(bestcub_dot)

xtable(summary(bestcub_dot))

#############################################################
############################################### 
#best model sodd lavoro intero campione

effe<-with(sud,Formula(sodd~dot_s|dot_s))
bestcub_sodd<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bestcub_sodd)
BIC(bestcub_sodd)



### covar NO DICOTOMIZZ
effe<-with(dati,Formula(sodd~att_s+redd_s
                        |att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s))




effe<-with(dati,Formula(sodd~att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d
              |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))

### covar NO DICOTOMIZZ
effe<-with(dati,Formula(sodd~att_s+redd_s
                        |att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s))
#att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s
#dicotomizza a 7          
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))

#dicotom a 6
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))

#### dicotom a 5
effe<-with(dati,Formula(sodd~0
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))

att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d
### dicotom a 4
effe<-with(dati,Formula(sodd~0
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))
att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d

bestcub_sodd<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bestcub_sodd)
BIC(bestcub_sodd)

param<-coef(bestcub_sodd)
param

################################################
#best sodd Nord

# no dicotom 
effe<-with(nord,Formula(sodd~0
                        |att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s))

effe<-with(nord,Formula(sodd~att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s
                        |att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s))

att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s

#dicotom a 7
effe<-with(nord,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))

#dicotom a 6
effe<-with(dati,Formula(sodd~att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))

effe<-with(nord,Formula(sodd~att_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))

bestcub_sodd<-fastCUB(effe,data=nord,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bestcub_sodd)
BIC(bestcub_sodd)


#best sodd centro
# no dicotom
effe<-with(centro,Formula(sodd~0
                        |att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s))
att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s

##dicotom a 7
effe<-with(centro,Formula(sodd~prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))

#dicotom a 6
effe<-with(dati,Formula(sodd~att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))

effe<-with(centro,Formula(sodd~redd_d+prof_d
                        |att_d+stab_d+auton_d+redd_d+know_d+carrie_d+prof_d))

bestcub_sodd<-fastCUB(effe,data=centro,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bestcub_sodd)
BIC(bestcub_sodd)

#best sodd sud

# no dicotom
effe<-with(sud,Formula(sodd~0
                          |att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s))
att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s

## dicotom a 7
effe<-with(sud,Formula(sodd~stab_d
                       |att_d+stab_d+auton_d+redd_d+carrie_d+prof_d))
#dicotom a 6
effe<-with(dati,Formula(sodd~att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))

effe<-with(sud,Formula(sodd~stab_d
                        |att_d+stab_d+auton_d+redd_d+carrie_d+prof_d))

bestcub_sodd<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bestcub_sodd)
BIC(bestcub_sodd)
######trattare le variabili come fattori ordinali
#m-1 modalità
##tratto doce come fattore ordinale  
#doce 
effe<-with(dati,Formula(dot_s~doce_2+doce_3+doce_4+doce_5+doce_6+doce_7+doce_8
                        |doce_2+doce_3+doce_4+doce_5+doce_6+doce_7+doce_8))

effe<-with(dati,Formula(dot_s~doce_4+doce_5+doce_6+doce_7+doce_8
                        |doce_2+doce_3+doce_4+doce_5+doce_6+doce_7+doce_8))
dot_cub<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(dot_cub)
BIC(dot_cub)


#collab 
effe<-with(dati,Formula(dot_s~collab_2+collab_3+collab_4+collab_5+collab_6+collab_7+collab_8
                        |collab_2+collab_3+collab_4+collab_5+collab_6+collab_7+collab_8))

dot_cub<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(dot_cub)
BIC(dot_cub)

#incora 
effe<-with(dati,Formula(dot_s~incora_2+incora_3+incora_4+incora_5+incora_6+incora_7+incora_8
                        |incora_2+incora_3+incora_4+incora_5+incora_6+incora_7+incora_8))

dot_cub<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(dot_cub)
BIC(dot_cub)

#train 
effe<-with(dati,Formula(dot_s~train_2+train_3+train_4+train_5+train_6+train_7+train_8
                        |train_2+train_3+train_4+train_5+train_6+train_7+train_8))

dot_cub<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(dot_cub)
BIC(dot_cub)

#spazi 
effe<-with(dati,Formula(dot_s~spazi_2+spazi_3+spazi_4+spazi_5+spazi_6+spazi_7+spazi_8
                        |spazi_2+spazi_3+spazi_4+spazi_5+spazi_6+spazi_7+spazi_8))

dot_cub<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(dot_cub)
BIC(dot_cub)

#ncorsi 
effe<-with(dati,Formula(dot_s~ncorsi_2+ncorsi_3+ncorsi_4+ncorsi_5+ncorsi_6+ncorsi_7+ncorsi_8
                        |ncorsi_2+ncorsi_3+ncorsi_4+ncorsi_5+ncorsi_6+ncorsi_7+ncorsi_8))

dot_cub<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(dot_cub)
BIC(dot_cub)


#qcorsi 
effe<-with(dati,Formula(dot_s~qcorsi_2+qcorsi_3+qcorsi_4+qcorsi_5+qcorsi_6+qcorsi_7+qcorsi_8
                        |qcorsi_2+qcorsi_3+qcorsi_4+qcorsi_5+qcorsi_6+qcorsi_7+qcorsi_8))

dot_cub<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(dot_cub)
BIC(dot_cub)


#### 2a batteria

#auton 
effe<-with(dati,Formula(sodd~auton_5+auton_6+auton_7
                        |auton_2+auton_3+auton_4+auton_5+auton_6+auton_7+auton_8))

sodd_cub<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(sodd_cub)
BIC(sodd_cub)

#att 
effe<-with(dati,Formula(sodd~att_2+att_3+att_4+att_5+att_6+att_7+att_8
                        |att_2+att_3+att_4+att_5+att_6+att_7+att_8))

sodd_cub<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(sodd_cub)
BIC(sodd_cub)

#prof 
effe<-with(dati,Formula(sodd~prof_2+prof_3+prof_4+prof_5+prof_6+prof_7+prof_8
                        |prof_2+prof_3+prof_4+prof_5+prof_6+prof_7+prof_8))

sodd_cub<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(sodd_cub)
BIC(sodd_cub)

#know
effe<-with(dati,Formula(sodd~know_2+know_3+know_4+know_5+know_6+know_7+know_8
                        |know_2+know_3+know_4+know_5+know_6+know_7+know_8))

sodd_cub<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(sodd_cub)
BIC(sodd_cub)

#redd
effe<-with(dati,Formula(sodd~redd_2+redd_3+redd_4+redd_5+redd_6+redd_7+redd_8
                        |redd_2+redd_3+redd_4+redd_5+redd_6+redd_7+redd_8))

sodd_cub<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(sodd_cub)
BIC(sodd_cub)

#stab
effe<-with(dati,Formula(sodd~stab_2+stab_3+stab_4+stab_5+stab_6+stab_7+stab_8
                        |stab_2+stab_3+stab_4+stab_5+stab_6+stab_7+stab_8))

sodd_cub<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(sodd_cub)
BIC(sodd_cub)

#carrie
effe<-with(dati,Formula(sodd~carrie_2+carrie_3+carrie_4+carrie_6+carrie_7+carrie_8
                        |carrie_2+carrie_3+carrie_4+carrie_5+carrie_6+carrie_7+carrie_8))

sodd_cub<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(sodd_cub)
BIC(sodd_cub)



####################
#variabili relative all'area di dottorato e a variabili socio-demografiche
effe<-Formula(dati$dot_s~dati$sesso+dati$eta_dott+dati$ateneoCentro+dati$ateneoSud+
                dati$Scienze_mediche+dati$Scienze_agrarie+dati$Ingegneria+dati$Scienze_uman_psico+
                dati$Sc_giurid+dati$Sc_econ_stat+dati$Sc_pol_social+dati$non_rifare_dott+dati$incerto_rifare_dott+
                dati$fuoric+dati$didatt+ dati$borsa+ dati$paper+dati$estero_d+
                dati$prog_ric
              |dati$sesso+dati$eta_dott+dati$ateneoCentro+dati$ateneoSud+
                dati$Scienze_mediche+dati$Scienze_agrarie+dati$Ingegneria+dati$Scienze_uman_psico+
                dati$Sc_giurid+dati$Sc_econ_stat+dati$Sc_pol_social+dati$non_rifare_dott+dati$incerto_rifare_dott+
                dati$fuoric+dati$didatt+dati$borsa+ dati$paper+dati$estero_d+
                dati$prog_ric)
############
### set di variabili
dati$sesso+dati$eta_dott
dati$Scienze_mediche+dati$Scienze_agrarie+dati$Ingegneria+dati$Scienze_uman_psico+
  dati$Sc_giurid+dati$Sc_econ_stat+dati$Sc_pol_social+dati$non_rifare_dott+dati$incerto_rifare_dott+
  dati$fuoric+dati$didatt+dati$borsa+ dati$paper+dati$estero_d+dati$prog_ric)




effe<-Formula(dati$dot_s~dati$eta_dott+
        dati$Scienze_uman_psico+dati$non_rifare_dott+
        dati$didatt+ dati$paper+dati$estero_d+
        dati$prog_ric|
        dati$sesso+ dati$Scienze_mediche+dati$Sc_pol_social+dati$non_rifare_dott+dati$incerto_rifare_dott+
        dati$fuoric+dati$didatt+ dati$paper+dati$estero_d)

bcub_dot<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bcub_dot, digits = 3)
BIC(bcub_dot, digits = 3)


BIC(bcub_dott, digits = 3)
[1] 26376
#### mettere questo     
effe<-Formula(dati$dot_s~dati$eta_dott+ 
              dati$Scienze_uman_psico+dati$paper+dati$estero_d
              |dati$Scienze_mediche+dati$Sc_pol_social+dati$non_rifare_dott+dati$incerto_rifare_dott+
              dati$fuoric+ dati$paper+dati$estero_d)
BIC(bcub_dott, digits = 3)
[1] 26369.72
     
bcub_dott<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bcub_dott, digits = 3)
BIC(bcub_dott, digits = 3)
     
##############################################################
#### ripartizione ateneo dove è stato conseguitop il dottorato
dott_sud<- filter(dati, ateneoSud==1)
effe<-Formula(dott_sud$dot_s~dott_sud$sesso+dott_sud$eta_dott+dott_sud$Scienze_mediche+dott_sud$Scienze_agrarie+dott_sud$Scienze_uman_psico+
                dott_sud$Sc_giurid+dott_sud$Sc_econ_stat+dott_sud$Sc_pol_social+dott_sud$non_rifare_dott+dott_sud$incerto_rifare_dott+
                dott_sud$fuoric+dott_sud$didatt+ dott_sud$borsa+ dott_sud$paper+dott_sud$estero_d+
                dott_sud$prog_ric
             |dott_sud$sesso+dott_sud$eta_dott+dott_sud$Scienze_mediche+dott_sud$Scienze_agrarie+dott_sud$Scienze_uman_psico+
                dott_sud$Sc_giurid+dott_sud$Sc_econ_stat+dott_sud$Sc_pol_social+dott_sud$non_rifare_dott+dott_sud$incerto_rifare_dott+
                dott_sud$fuoric+dott_sud$didatt+ dott_sud$borsa+ dott_sud$paper+dott_sud$estero_d+
                dott_sud$prog_ric)

effe<-Formula(dott_sud$dot_s~dott_sud$Scienze_uman_psico+
                 dott_sud$paper
              |dott_sud$non_rifare_dott+dott_sud$incerto_rifare_dott+
                dott_sud$paper+dott_sud$estero_d)

bcub_dottsud<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bcub_dottsud, digits = 3)
BIC(bcub_dottsud, digits = 3)
     


sodddot<-as.factor(dati$dot_s)
class(sodddot)
pom<-polr(sodddot~ eta_dott+Scienze_uman_psico+
            paper+estero_d+didatt+
            non_rifare_dott+sesso+
            Scienze_mediche+Sc_pol_social+
            incerto_rifare_dott+fuoric, method="logistic", data = dati)
summary(pom)
#dati$non
step.mod<-stepAIC(pom,trace=FALSE, direction = "backward", k=log(nrow(dati)))
step.mod<-stepAIC(pom,trace=FALSE, direction = "backward")

summary(step.mod)
BIC(step.mod)
logLik(step.mod)
#conf<-confint(pom)
tab<-exp(cbind(OR=step.mod$coefficients))
tab<-exp(cbind(-pom$coefficients, pom$coefficients))
tab
exp(0.012)


table(X_dott$ate7mod)
############################################################################
dott_nord<- filter(dati, ateneoNord==1)
effe<-Formula(dott_nord$dot_s~dott_nord$sesso+dott_nord$eta_dott+dott_nord$Scienze_mediche+dott_nord$Scienze_agrarie+dott_nord$Scienze_uman_psico+
                dott_nord$Sc_giurid+dott_nord$Sc_econ_stat+dott_nord$Sc_pol_social+dott_nord$non_rifare_dott+dott_nord$incerto_rifare_dott+
                dott_nord$fuoric+dott_nord$didatt+ dott_nord$borsa+ dott_nord$paper+dott_nord$estero_d+
                dott_nord$prog_ric
              |dott_nord$sesso+dott_nord$eta_dott+dott_nord$Scienze_mediche+dott_nord$Scienze_agrarie+dott_nord$Scienze_uman_psico+
                dott_nord$Sc_giurid+dott_nord$Sc_econ_stat+dott_nord$Sc_pol_social+dott_nord$non_rifare_dott+dott_nord$incerto_rifare_dott+
                dott_nord$fuoric+dott_nord$didatt+ dott_nord$borsa+ dott_nord$paper+dott_nord$estero_d+
                dott_nord$prog_ric)

effe<-Formula(dott_nord$dot_s~dott_nord$eta_dott+
                dott_nord$non_rifare_dott+
                dott_nord$didatt
              |dott_nord$sesso+dott_nord$Scienze_mediche+dott_nord$fuoric
              +dott_nord$Sc_pol_social+dott_nord$non_rifare_dott+dott_nord$incerto_rifare_dott+
                dott_nord$paper+dott_nord$estero_d)

bcub_dottnord<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bcub_dottnord, digits = 3)
BIC(bcub_dottnord, digits = 3)
   
table(X_dott$ate7mod)
####################################################################
dott_centro<- filter(dati, ateneoCentro==1)
dim(sud)

3330+2104+1961
dim(dott_sud)
effe<-Formula(dott_centro$dot_s~dott_centro$sesso+dott_centro$eta_dott+dott_centro$Scienze_mediche+dott_centro$Scienze_agrarie+dott_centro$Scienze_uman_psico+
                dott_centro$Sc_giurid+dott_centro$Sc_econ_stat+dott_centro$Sc_pol_social+dott_centro$non_rifare_dott+dott_centro$incerto_rifare_dott+
                dott_centro$fuoric+dott_centro$didatt+ dott_centro$borsa+ dott_centro$paper+dott_centro$estero_d+
                dott_centro$prog_ric
              |dott_centro$sesso+dott_centro$eta_dott+dott_centro$Scienze_mediche+dott_centro$Scienze_agrarie+dott_centro$Scienze_uman_psico+
                dott_centro$Sc_giurid+dott_centro$Sc_econ_stat+dott_centro$Sc_pol_social+dott_centro$non_rifare_dott+dott_centro$incerto_rifare_dott+
                dott_centro$fuoric+dott_centro$didatt+ dott_centro$borsa+ dott_centro$paper+dott_centro$estero_d+
                dott_centro$prog_ric)

effe<-Formula(dott_centro$dot_s~dott_centro$Scienze_uman_psico+
                dott_centro$paper+dott_centro$estero_d
              |dott_centro$non_rifare_dott+dott_centro$incerto_rifare_dott+
                dott_centro$fuoric+ dott_centro$paper+dott_centro$estero_d)

 
bcub_dottcentro<-fastCUB(effe,data=dott_centro,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bcub_dottcentro, digits = 3)
BIC(bcub_dottcentro, digits = 3)
     
     
#########################################################################
#lavoro
     
effe<-Formula(dati$sodd~dati$sesso+dati$eta_dott+dati$Scienze_mediche+dati$Scienze_agrarie+dati$Scienze_uman_psico+
                dati$Sc_giurid+dati$Sc_econ_stat+dati$Sc_pol_social+
                dati$ateneoCentro+dati$ateneoSud+ dati$stciv+ dati$figlio+
                dati$lav_dip+dati$altro_tipolav+
                dati$indust+dati$istruz+dati$ricer_svil+dati$altro_settore
              |dati$sesso+dati$eta_dott+dati$Scienze_mediche+dati$Scienze_agrarie+dati$Scienze_uman_psico+
                dati$Sc_giurid+dati$Sc_econ_stat+dati$Sc_pol_social+dati$non_rifare_dott+dati$incerto_rifare_dott+
                dati$fuoric+dati$didatt+dati$borsa+ dati$paper+dati$estero_d+
                dati$prog_ric+ dati$ateneoCentro+dati$ateneoSud+ dati$stciv+ dati$figlio+
                dati$lavoroSud+dati$lavoroCentro+dati$lavoroEstero+ dati$lav_dip+dati$altro_tipolav+
                dati$indust+dati$istruz+dati$ricer_svil+dati$altro_settore)

#╚ set di  variabili
dati$sesso+dati$eta_dott+dati$Scienze_mediche+dati$Scienze_agrarie+dati$Scienze_uman_psico+
  dati$Sc_giurid+dati$Sc_econ_stat+dati$Sc_pol_social+
  dati$ateneoCentro+dati$ateneoSud+ dati$stciv+ dati$figlio+
  dati$lav_dip+dati$altro_tipolav+
  dati$indust+dati$istruz+dati$ricer_svil+dati$altro_settore


effe<-Formula(dati$sodd~dati$eta_dott+dati$Scienze_uman_psico+
                dati$stciv+ dati$ateneoCentro+dati$ateneoSud+
                dati$lavoroEstero+dati$ore
              |dati$sesso+dati$Sc_pol_social+
                dati$altro_tipolav+dati$lavoroEstero+dati$ore)


bcub_soddx<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bcub_soddx, digits = 3)
BIC(bcub_soddx, digits = 3)



soddlav<-as.factor(dati$sodd)
class(soddlav)
### POM NO DICOTOMIZZ
pom<-polr(soddlav~ eta_dott+Scienze_uman_psico+
            stciv+ ateneoCentro+ateneoSud+
            lavoroEstero+ore
          +sesso+Sc_pol_social+
          altro_tipolav+lavoroEstero+ore, method="logistic", data = dati)
summary(pom)

step.mod<-stepAIC(pom,trace=FALSE, direction = "backward", k=log(nrow(dati)))
step.mod<-stepAIC(pom,trace=FALSE, direction = "backward")

summary(step.mod)
BIC(step.mod)
logLik(step.mod)
tab<-exp(cbind(OR=step.mod$coefficients))
tab

####################################################
# NORD sodd lav altre covariate
sesso+eta_dott+Scienze_mediche+Scienze_agrarie+Scienze_uman_psico+
  Sc_giurid+Sc_econ_stat+Sc_pol_social+
  ateneoCentro+ateneoSud+ stciv+ figlio+
  lav_dip+altro_tipolav+
  indust+istruz+ricer_svil+altro_settore+
  lavoroEstero+ore


effe<-with(nord,Formula(sodd~eta_dott+ore
              |sesso+altro_tipolav))


bcub_soddx<-fastCUB(effe,data=nord,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bcub_soddx, digits = 3)
BIC(bcub_soddx, digits = 3)

###########################################################☺
centro

sesso+eta_dott+Scienze_mediche+Scienze_agrarie+Scienze_uman_psico+
  Sc_giurid+Sc_econ_stat+Sc_pol_social+
  ateneoCentro+ateneoSud+ stciv+ figlio+
  lav_dip+altro_tipolav+
  indust+istruz+ricer_svil+altro_settore+
  lavoroEstero+ore


effe<-with(centro,Formula(sodd~Scienze_uman_psico
                             |sesso+Sc_pol_social+
                           altro_tipolav+ore))


bcub_soddx<-fastCUB(effe,data=centro,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bcub_soddx, digits = 3)
BIC(bcub_soddx, digits = 3)


############################################################
#SUD

sesso+eta_dott+Scienze_mediche+Scienze_agrarie+Scienze_uman_psico+
  Sc_giurid+Sc_econ_stat+Sc_pol_social+
  ateneoCentro+ateneoSud+ stciv+ figlio+
  lav_dip+altro_tipolav+
  indust+istruz+ricer_svil+altro_settore+
  lavoroEstero+ore


effe<-with(sud,Formula(sodd~stciv
                          |eta_dott+
                         Sc_giurid+altro_tipolav+
                         ore))


bcub_soddx<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bcub_soddx, digits = 3)
BIC(bcub_soddx, digits = 3)



#####################################################
library(MASS)
pom<-polr(norddlav~ att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d, method="logistic", data = dati)
summary(pom)
     
soddlav<-as.factor(dati$sodd)
norddlav<-as.factor(dati$sodd)
class(sodd)
summary(pom)
step.mod<-stepAIC(pom,trace=FALSE, direction = "backward", k=log(nrow(dati)))
step.mod<-stepAIC(pom,trace=FALSE, direction = "backward")

summary(step.mod)
BIC(pom)
BIC(step.mod)
     
     
     
effe<-Formula(dati$sodd~dati$sesso+dati$eta_dott+dati$stciv+dati$figlio+dati$figlio
              +dati$estero_d+dati$ateneoNord+ dati$ateneoCentro+ dati$ateneoSud + dati$lavoroNord+ dati$lavoroSud+ dati$lavoroCentro+
                dati$lavoroEstero+dati$stab_d+dati$auton_d+dati$know_d+dati$redd_d+dati$carrie_d+dati$prof_d)


     
effe<-Formula(dati$sodd~dati$sesso+dati$eta_dott+dati$stciv+dati$figlio+
                dati$estero_d+ dati$ateneoCentro+ dati$ateneoSud + dati$lavoroSud+ dati$lavoroCentro+
                dati$lavoroEstero+ dati$att_d+dati$stab_d+dati$auton_d+dati$know_d+dati$redd_d+dati$carrie_d+dati$prof_d|dati$sesso+dati$eta_dott+dati$stciv+dati$figlio+
                dati$estero_d+ dati$ateneoCentro+ dati$ateneoSud + dati$lavoroSud+ dati$lavoroCentro+
                dati$lavoroEstero+ dati$att_d+dati$stab_d+dati$auton_d+dati$know_d+dati$redd_d+dati$carrie_d+dati$prof_d)

     
effe <- Formula(dati$sodd ~ dati$eta_dott + dati$stciv + dati$lavoroSud +
                  dati$lavoroEstero + dati$att_d + dati$prof_d |
                  dati$sesso +
                  dati$lavoroEstero + dati$att_d + dati$stab_d + dati$auton_d +
                  dati$know_d + dati$redd_d + dati$carrie_d + dati$prof_d)

cub_sodd<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_sodd, digits = 3)
BIC(cub_sodd, digits = 3)

###################
#correlazione di spearman per dati ordinali
#misura la monotonia e non la linearità
batteria_lavoro<-dati[, c(9:16)]
  
batteria_dott<-dati[, c(1:8)]
batteria_dott<-dati[, c("dot_s","qcorsi_d", "ncorsi_d", "doce_d")]
coortedott<-coorte[, c(30:37)]
batteria_dott7<-select(dati,qcorsi_d:incora_d, dot_s)
batteria_lavoro7<-select(dati,  att_d:prof_d, sodd)

View(batteria_dott7)
#tra le variabili riscalate e non riscalate
#il coefficiente di corr di spearman cambia poco

cor(batteria_lavoro7,  method =  "spearman")
cor(batteria_dott7,  method =  "spearman")     
corr_dotsodd<-dati[, c(8,16)]
cor(corr_dotsodd,  method =  "spearman") 

cor.test(dati$sodd, dati$dot_s, method =  c("spearman"))
cor(coortedott,  method =  "spearman")     

cor(batteria_lavoro,  method =  "spearman")
cor(batteria_dott,  method =  "spearman")     

###CAPACITà ESPLICATIVA####

######### dicotomiz a 7
effe<-Formula(dati$sodd~dati$att_d+dati$prof_d|dati$att_d+dati$stab_d+dati$auton_d+dati$know_d+dati$redd_d+dati$carrie_d+dati$prof_d)
cub_sodd<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_sodd, digits = 3)
BIC(cub_sodd, digits = 3)
######### dicotomiz a 6
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))

######## dicotomiz a 5
effe<-with(dati,Formula(sodd~0
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))
###### dicotom a 4
effe<-with(dati,Formula(sodd~0
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))
### no dicotomizz 
effe<-with(dati,Formula(sodd~att_s+redd_s
                        |att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s))
###Nord var no dicotom
effe<-with(nord,Formula(sodd~0
                        |att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s))
###Centro var no dicotom
effe<-with(centro,Formula(sodd~0
                        |att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s))
###Sud var no dicotom
effe<-with(sud,Formula(sodd~0
                          |att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s))


## Nord dicotom a 7
effe<-with(nord,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))
## Centro dicotom a 7
effe<-with(centro,Formula(sodd~prof_d
                          |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))
## Sud dicotom a 7
effe<-with(sud,Formula(sodd~stab_d
                       |att_d+stab_d+auton_d+redd_d+carrie_d+prof_d))
#nord dicotom a 6
effe<-with(nord,Formula(sodd~att_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))
#centro dicotom a 6
effe<-with(centro,Formula(sodd~redd_d+prof_d
                          |att_d+stab_d+auton_d+redd_d+know_d+carrie_d+prof_d))
#sud dicotom a 6
effe<-with(sud,Formula(sodd~stab_d
                       |att_d+stab_d+auton_d+redd_d+carrie_d+prof_d))


cub_sodd<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_sodd, digits = 3)
BIC(cub_sodd, digits = 3)

lm<-logLik(cub_sodd)
lm

effe<-with(sud, Formula(sodd ~0|0))
cub_dot00<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot00, digits = 3)
l00<-logLik(cub_dot00)
l00
#capacita esplicative 
e_x=(lx-l00)/(lm-l00)

#capacità esplicativa cub sodd lavoro intero campione 
#capacita esplicative 
e_x=(lx-l00)/(lm-l00)
#valuto la cacità esplicativa di att_d
effe<-with(sud, Formula(sodd ~0|att_d))
cub_dot_x<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx

e_at=(-5007.188+5241.586)/(-4811.05+5241.586)
e_at    
e_attt=(lx-l00)/(lm-l00)
e_attt

#valuto la cacità esplicativa di stab_d
effe<-with(sud, Formula(sodd ~stab_d|stab_d))
cub_dot_x<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx

e_sta=(-5130.734+5241.586)/(-4811.05+5241.586)
e_sta    
e_stab=(lx-l00)/(lm-l00)
e_stab


#valuto la cacità esplicativa di auton_d
effe<-with(sud, Formula(sodd ~0|auton_d))
cub_dot_x<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx

e_aut=(-5085.357+5241.586)/(-4811.05+5241.586)
e_aut    
e_auton=(lx-l00)/(lm-l00)
e_auton

#valuto la cacità esplicativa di know_d
effe<-with(sud, Formula(sodd ~know_d|know_d))
cub_dot_x<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx

e_kn=(-5033.375+5241.586)/(-3750.406+5241.586)
e_kn    
e_know=(lx-l00)/(lm-l00)
e_know

#valuto la cacità esplicativa di redd_d
effe<-with(sud, Formula(sodd ~0|redd_d))
cub_dot_x<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx

e_red=(-5151.291+5241.586)/(-4811.05+5241.586)
e_red    
e_redd=(lx-l00)/(lm-l00)
e_redd

#valuto la cacità esplicativa di carrie_d
effe<-with(sud, Formula(sodd ~0|carrie_d))
cub_dot_x<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx

e_car=(-5070.886+5241.586)/(-4811.05+5241.586)
e_car    
e_carr=(lx-l00)/(lm-l00)
e_carr

#valuto la cacità esplicativa di prof_d
effe<-with(sud, Formula(sodd ~0|prof_d))
cub_dot_x<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx

e_pr=(-5008.753+5241.586)/(-4811.05+5241.586)
e_pr    
e_prof=(lx-l00)/(lm-l00)
e_prof

#########################################
#capacità esplicativa pom sodd lavoro intero campione

soddlav<-as.factor(dati$sodd)
class(soddlav)
### POM NO DICOTOMIZZ
pom<-polr(soddlav~ att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s, method="logistic", data = dati)
summary(pom)
BIC(pom)
logLik(pom)
conf<-confint(pom)
tab<-exp(cbind(OR=pom$coefficients, conf))
tab<-exp(cbind(-pom$coefficients, pom$coefficients))
tab
exp(0.8426)
exp(-0.8426)
library(brant)
brant(pom)
## si rifiuta l'ipotesi di proporzionalità
dati %>% 
  tabyl(dot_s, train_s)
### POM dicotomizz a 7
pom<-polr(soddlav~ att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d, method="logistic", data = nord)
summary(pom)
############ pom dicotom a 6
pom<-polr(soddlav~ att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d, method="logistic", data = nord)
summary(pom)
#### dicotomiz a 5
pom<-polr(soddlav~att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d, method="logistic", data = dati)
summary(pom)
####### dicotomiz a 4
pom<-polr(soddlav~ att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d, method="logistic", data = dati)
summary(pom)
######
step.mod<-stepAIC(pom,trace=FALSE, direction = "backward", k=log(nrow(dati)))
summary(step.mod)
#
### ripartizioni sodd lav no dicotom
soddlav<-as.factor(sud$sodd)
class(soddlav)
###no dicotom
pom<-polr(soddlav~att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s, method="logistic", data = sud)
summary(pom)
##prova vgam
pom_vgam <- vglm( sodd~att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s ,
             cumulative( parallel = TRUE),  sud)


exp(0.724)
summary(pom_vgam)
predict.vgam(pom_vgam)
sud=as.data.frame(sud)
sud

###  dicotom a 7
pom<-polr(soddlav~att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d, method="logistic", data = sud)
summary(pom)


lm<-logLik(pom)
lm
BIC(pom)
BIC(step.mod)

conf<-confint(pom)
tab<-exp(cbind(OR=pom$coefficients, conf))
tab



pom0<-polr(formula=soddlav~1, method="logistic", data = sud)
summary(pom0)
l00<-logLik(pom0)
l00
#capacita esplicative 
e_x=(lx-l00)/(lm-l00)
#########################################
#capacita esplicative di att_d
pom_att<-polr(formula=soddlav~att_d , method="logistic", data = sud)
summary(pom_att)
lx<-logLik(pom_att)
lx

e_at=(-4931.379+5218.367)/(-4688.654+5218.367)
e_at
e_att=(lx-l00)/(lm-l00)
e_att

#capacita esplicative di stab_d
pom_stab<-polr(formula=soddlav~stab_d , method="logistic", data = sud)
summary(pom_stab)
lx<-logLik(pom_stab)
lx

e_st=(-5087.34+5218.367)/(-4688.654+5218.367)
e_st
e_stab=(lx-l00)/(lm-l00)
e_stab


#capacita esplicative di auton_d
pom_auton<-polr(formula=soddlav~auton_d , method="logistic", data = sud)
summary(pom_auton)
lx<-logLik(pom_auton)
lx

e_au=(-5036.812+5218.367)/(-4688.654+5218.367)
e_au
e_auton=(lx-l00)/(lm-l00)
e_auton

#capacita esplicative di know_D
pom_know<-polr(formula=soddlav~know_d , method="logistic", data = sud)
summary(pom_know)
lx<-logLik(pom_know)
lx

e_kn=(-5120.901+5218.367)/(-4688.654+5218.367)
e_kn
e_know=(lx-l00)/(lm-l00)
e_know

#capacita esplicative di redd_d
pom_redd<-polr(formula=soddlav~redd_d, method="logistic", data = sud)
summary(pom_redd)
lx<-logLik(pom_redd)
lx

e_red=(-5116.731+5218.367)/(-4688.654+5218.367)
e_red
e_redd=(lx-l00)/(lm-l00)
e_redd

#capacita esplicative di carrie_d
pom_carrie<-polr(formula=soddlav~carrie_d, method="logistic", data = sud)
summary(pom_carrie)
lx<-logLik(pom_carrie)
lx

e_car=(-5005.531+5218.367)/(-4688.654+5218.367)
e_car
e_carrie=(lx-l00)/(lm-l00)
e_carrie

#capacita esplicative di prof_d
pom_prof<-polr(formula=soddlav~prof_d , method="logistic", data = sud)
summary(pom_prof)
lx<-logLik(pom_prof)
lx


e_pr=(-4937.641+5218.367)/(-4688.654+5218.367)
e_pr
e_prof=(lx-l00)/(lm-l00)
e_prof

#####FARE DICOTOM A 6 E BASTA


#####################################################
#############################################
#best model sodd dott intero campione
### no dicotom
effe<-with(dati, Formula(dot_s~train_s|qcorsi_s+ncorsi_s+doce_s+spazi_s+train_s+collab_s+incora_s))     

bestcub_dot<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_sodd)

cub_sodd<-GEM(Formula(sodd~0|0|0), family = "cub" ,data=dati)
theor<-fitted(cub_sodd)[,1]
df<-tibble(sodd=1:8,
           prob=theor) %>% 
  mutate(sodd=factor(sodd))
df %>% 
  ggplot(aes(x=sodd,y=prob ))+
  geom_col(fill="darkblue")+
  theme_bw()+
  labs(title = "Distribuzione di probabilità del modello CUB con variabile dipendente sodd", x="sodd", y="Probabilità", fill="sodd")+
  theme(legend.position = "none",panel.grid = element_blank())+
  ggsave(file="probdot.eps")




cub_dot<-GEM(Formula(dot_s~0|0|0), family = "cub" ,data=dati)
theor<-fitted(cub_dot)[,1]
df<-as.data.frame(theor)
df<-tibble(dot_s=1:8,
           prob=theor) %>% 
  mutate(dot_s=factor(dot_s))


df %>% 
  ggplot(aes(x=dot_s,y=prob ))+
  geom_col(fill="red")+
  theme_bw()+
  labs(title = "Distribuzione di probabilità del modello CUB con variabile dipendente sodd", x="dot_s", y="Probabilità", fill="dot_s")+
  theme(legend.position = "none",panel.grid = element_blank())+
  ggsave(file="probsodd.eps")



########### dicotomizz a 7
effe<-with(dati, Formula(dot_s~doce_d+train_d+collab_d+incora_d|qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d))     
bestcub_dot<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
##### dicotomizz a 6
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)
bestcub_dot<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
####### dicotom a 5
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)
#### dicotom a 4
effe<-Formula(dati$dot_s~dati$train_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)
####no dicotom
effe<-Formula(dati$dot_s~dati$train_s|dati$qcorsi_s+dati$ncorsi_s+dati$doce_s+dati$spazi_s+dati$train_s+dati$collab_s+dati$incora_s)
bestcub_dot<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)

effe<-with(dati, Formula(dot_s~doce_d+train_d+collab_d+incora_d|qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d))     
#### best model sodd dott nord
### nord no dicotom
effe<-with(nord, Formula(dot_s~train_s|qcorsi_s+ncorsi_s+doce_s+spazi_s+train_s+collab_s+incora_s))     
bestcub_dot<-fastCUB(effe,data=nord,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
###  dicotom a 7
effe<-with(nord, Formula(dot_s~collab_d+incora_d|qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d))
bestcub_dot<-fastCUB(effe,data=nord,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
## dicotom a 7
effe<-with(nord, Formula(dot_s~collab_d+incora_d|qcorsi_d+ncorsi_d+doce_d+train_d+collab_d+incora_d))
bestcub_dot<-fastCUB(effe,data=nord,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
## dicotom a 6
effe<-with(nord,Formula(dot_s~train_d+collab_d+incora_d|qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d))
bestcub_dot<-fastCUB(effe,data=nord,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
### Centro dott
# no dicotom
effe<-with(centro, Formula(dot_s~0|qcorsi_s+ncorsi_s+doce_s+train_s+incora_s))
bestcub_dot<-fastCUB(effe,data=centro,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)

#  dicotom a 7
effe<-with(centro,Formula(dot_s~doce_d+incora_d|qcorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d))
bestcub_dot<-fastCUB(effe,data=centro,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)

#  dicotom a 6
effe<-with(centro,Formula(dot_s~doce_d+collab_d|qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d))
bestcub_dot<-fastCUB(effe,data=centro,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)

### Sud dott
# no dicotom
effe<-with(sud, Formula(dot_s~0|qcorsi_s+ncorsi_s+doce_s+spazi_s+train_s+collab_s+incora_s))
bestcub_dot<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
#  dicotom a 7
effe<-with(sud,Formula(dot_s~doce_d+incora_d|qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d))
bestcub_dot<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
#  dicotom a 6
effe<-with(sud,Formula(dot_s~doce_d+train_d|qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d))
bestcub_dot<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)




effe<-Formula(dati$sodd~dati$spazi_s+dati$collab_s|dati$qcorsi_s+dati$ncorsi_s+dati$spazi_s+dati$collab_s+dati$incora_s)
effe<-with(nord, Formula(sodd~ncorsi_s+incora_s|spazi_s+collab_s+incora_s))
qcorsi_s+ncorsi_s+doce_s+spazi_s+train_s+collab_s+incora_s
effe<-with(centro, Formula(sodd~ncorsi_s+collab_s|ncorsi_s+spazi_s+incora_s))

effe<-with(dati, Formula(sodd~dot_s|dot_s))

bestcub_dot<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bestcub_dot)
BIC(bestcub_dot)




summary(bestcub_dot)
BIC(bestcub_dot)
lm<-logLik(bestcub_dot)
lm

effe<-with(dati, Formula(sodd ~0|0))
cub_dot00<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot00, digits = 3)
l00<-logLik(cub_dot00)
l00
#capacita esplicative 
e_x=(lx-l00)/(lm-l00)
#########################################
#valuto la cacità esplicativa di qcorsi_d
effe<-with(dati, Formula(sodd ~dot_s|dot_s))
cub_dot_x<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_qcorsi=(lx-l00)/(lm-l00)
e_qcorsi

#valuto la cacità esplicativa di ncorsi_D
effe<-with(centro, Formula(sodd ~ ncorsi_s|ncorsi_s))
cub_dot_x<-fastCUB(effe,data=centro,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx

e_ncor=(-12746.51+14657.25)/(-10368.21+14657.25)
e_ncor    
e_ncorsi=(lx-l00)/(lm-l00)
e_ncorsi

#valuto la cacità esplicativa di doce_D
effe<-with(sud, Formula(dot_s ~doce_d|doce_d))
cub_dot_x<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx

e_doc=(-12633.66+14657.25)/(-10368.21+14657.25)
e_doc    
e_doce=(lx-l00)/(lm-l00)
e_doce

#valuto la cacità esplicativa di spazi_d
effe<-with(sud, Formula(sodd ~spazi_s|0))
cub_dot_x<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx

e_spaz=(-13187.6+14657.25)/(-10368.21+14657.25)
e_spaz    
e_spazi=(lx-l00)/(lm-l00)
e_spazi

#valuto la cacità esplicativa di train_d
effe<-with(nord, Formula(dot_s ~train_d|train_d))
cub_dot_x<-fastCUB(effe,data=nord,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx

e_tra=(-11991.35+14657.25)/(-10368.21+14657.25)
e_tra    
e_train=(lx-l00)/(lm-l00)
e_train

#valuto la cacità esplicativa di collab_d
effe<-with(centro,Formula(sodd ~0|collab_s))
cub_dot_x<-fastCUB(effe,data=centro,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx

e_col=(-12123.59+14657.25)/(-10368.21+14657.25)
e_col    
e_collab=(lx-l00)/(lm-l00)
e_collab

#valuto la cacità esplicativa di incora_d
effe<-with(centro, Formula(sodd ~0|incora_s))
cub_dot_x<-fastCUB(effe,data=centro,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx

e_inc=(-12477.08+14657.25)/(-10368.21+14657.25)
e_inc    
e_incora=(lx-l00)/(lm-l00)
e_incora

#prova emilia
#ifelse(dati[, c(1:8)]==5,1,0) 

#########################################
#capacità esplicativa pom sodd lavoro intero campione
#W<-select(sud, qcorsi_d, ncorsi_d, doce_d, spazi_d, train_d, collab_d ,incora_d)
sodddot<-as.factor(dati$dot_s)
class(sodddot)
#per il lavoro
soddlav<-as.factor(dati$sodd)
class(soddlav)

######## pom no dicotom
pom<-polr(sodddot~qcorsi_s+ncorsi_s+doce_s+spazi_s+train_s+collab_s+incora_s, method="logistic", data = dati)
## lavoro
pom<-polr(soddlav~qcorsi_s+ncorsi_s+doce_s+spazi_s+train_s+collab_s+incora_s, method="logistic", data = dati)
## definitivo
pom<-polr(soddlav~ncorsi_s+spazi_s+collab_s+incora_s, method="logistic", data = dati)
#nord
pom<-polr(soddlav~ncorsi_s+spazi_s+collab_s+incora_s, method="logistic", data = nord)
#centro
pom<-polr(soddlav~spazi_s+collab_s+incora_s, method="logistic", data = centro)
#sud
pom<-polr(soddlav~spazi_s+collab_s, method="logistic", data = sud)


#pom<-polr(sodddot~sesso, method="logistic", data = dati)
summary(pom)
tab<-exp(cbind(OR=pom$coefficients))
tab


soddlav<-as.factor(sud$sodd)
pom<-polr(soddlav~dot_s, method="logistic", data = sud)
summary(pom)

logLik(pom)
BIC(pom)

brant(pom)
### grafico importante per pom
library(ggeffects)
library(dplyr) # For the pipes


pom %>%
  ggpredict(terms = c("ncorsi_s","spazi_s","collab_s","incora_s")) %>%
  plot()


pom %>%
  ggpredict(terms = c("qcorsi_s","ncorsi_s","doce_s","spazi_s","train_s","collab_s","incora_s")) %>%
  plot()


library(ggeffects)
library(dplyr) # For the pipes
pom %>% 
  ggpredict(c("dot_s")) %>%
  plot()+
  scale_x_discrete(limits=1:8)+
  labs(title = "sodd",y="Probabilità")+
  theme_bw()


pom %>% 
  ggpredict(c("spazi_s")) %>%
  plot()+
  scale_x_discrete(limits=1:8)+
  labs(title = "sodd",y="Probabilità")+
  theme_bw()


pom %>% 
  ggpredict(c("collab_s")) %>%
  plot()+
  scale_x_discrete(limits=1:8)+
  labs(title = "sodd",y="Probabilità")+
  theme_bw()


pom %>% 
  ggpredict(c("incora_s")) %>%
  plot()+
  scale_x_discrete(limits=1:8)+
  labs(title = "sodd",y="Probabilità")+
  theme_bw()


  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  
we %>% 
  plot()
we<-ggpredict(pom, c("sesso"))
 plot(we)+
   ggplot(we,aes(x=x,y=predicted))
   geom_point()
# esportare in LANDSCAPE e dimensioni 4*6 in


brant(pom)
pom<-polr(soddlav~ att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d, method="logistic", data = dati)
pom_vgam <- vglm( dot_s~qcorsi_s+ncorsi_s+doce_s+spazi_s+train_s+collab_s+incora_s ,
                  cumulative( parallel = TRUE, reverse = F),  dati)

pom_vgam <- vglm( sodddot~qcorsi_s+ncorsi_s+doce_s+spazi_s+train_s+collab_s+incora_s ,
                  cumulative( parallel = TRUE),  dati)
summary(pom_vgam)
logLik(pom_vgam)

### pom dicotom a 7
pom<-polr(sodddot~qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d, method="logistic", data = sud)
summary(pom)
######## pom dicotom a 6
pom<-polr(sodddot~qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d, method="logistic", data = sud)
summary(pom)
##### pom dicotom a 5
pom<-polr(sodddot~qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d, method="logistic", data = dati)
summary(pom)
######## pom dicotom a 4
pom<-polr(sodddot~qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d, method="logistic", data = dati)
summary(pom)
#######
step.mod<-stepAIC(pom,trace=FALSE, direction = "backward", k=log(nrow(dati)))
summary(step.mod)
conf<-confint(pom)
tab<-exp(cbind(OR=pom$coefficients, conf))
tab
###########

## lavoro
soddlav<-as.factor(sud$sodd)
class(soddlav)

## definitivo
pom<-polr(soddlav~ncorsi_s+spazi_s+collab_s+incora_s, method="logistic", data = dati)
#nord
pom<-polr(soddlav~ncorsi_s+spazi_s+collab_s+incora_s, method="logistic", data = nord)
#centro
pom<-polr(soddlav~spazi_s+collab_s+incora_s, method="logistic", data = centro)
#sud
pom<-polr(soddlav~spazi_s+collab_s, method="logistic", data = sud)


summary(pom)

lm<-logLik(pom)
lm

BIC(pom)
BIC(step.mod)

pom0<-polr(formula=soddlav~1 , method="logistic", data = sud)
summary(pom0)
l00<-logLik(pom0)
l00
#capacita esplicative 
e_x=(lx-l00)/(lm-l00)
#########################################
#capacita esplicative di qcorsi_d
pom_x<-polr(formula=sodddot~qcorsi_s, method="logistic", data = sud)
summary(pom_x)
lx<-logLik(pom_x)
lx

e_qc=(-2923.559+3546.832)/(-2256.311+3546.832)
e_qc
e_qc=(lx-l00)/(lm-l00)
e_qc

#capacita esplicative di ncornord
pom_x<-polr(formula=soddlav~ncorsi_s , method="logistic", data = nord)
summary(pom_x)
lx<-logLik(pom_x)
lx

e_nc=(-2944.044+3546.832)/(-2256.311+3546.832)
e_nc
e_nc=(lx-l00)/(lm-l00)
e_nc
#capacita esplicative di doce_d
pom_x<-polr(formula=sodddot~doce_d , method="logistic", data = nord)
summary(pom_x)
lx<-logLik(pom_x)
lx

e_dc=(-2977.674+3546.832)/(-2256.311+3546.832)
e_dc
e_dc=(lx-l00)/(lm-l00)
e_dc
#capacita esplicative di spazi_d
pom_x<-polr(formula=soddlav~spazi_s , method="logistic", data = sud)
summary(pom_x)
lx<-logLik(pom_x)
lx

e_spz=(-5365.208+5508.554)/(-4909.757+5508.554)
e_spz
e_spz=(lx-l00)/(lm-l00)
e_spz

#capacita esplicative di train_d
pom_x<-polr(formula=sodddot~train_d , method="logistic", data = nord)
summary(pom_x)
lx<-logLik(pom_x)
lx

e_tr=(-2785.285+3546.832)/(-2256.311+3546.832)
e_tr
e_trai=(lx-l00)/(lm-l00)
e_trai

#capacita esplicative di collab_d
pom_x<-polr(formula=soddlav~collab_s , method="logistic", data = sud)
summary(pom_x)
lx<-logLik(pom_x)
lx

e_clb=(-5164.895+5508.554)/(-4909.757+5508.554)
e_clb
e_clb=(lx-l00)/(lm-l00)
e_clb

#capacita esplicative di incora_d
pom_x<-polr(formula=soddlav~incora_s , method="logistic", data =centro)
summary(pom_x)
lx<-logLik(pom_x)
lx

e_incor=(-2913.988+3546.832)/(-2256.311+3546.832)
e_incor
e_incor=(lx-l00)/(lm-l00)
e_incor

## fare sodd lav per non dicotm 
## e poi dicotomm a 6 per tutti


effe<-Formula(dati$dot_s~dati$eta_dott+
                dati$Scienze_uman_psico+
                 dati$paper+dati$estero_d|
                 dati$Scienze_mediche+dati$Sc_pol_social+dati$non_rifare_dott+dati$incerto_rifare_dott+
                dati$fuoric+ dati$paper+dati$estero_d)

bcub_dot<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bcub_dot, digits = 3)
BIC(bcub_dot, digits = 3)

summary(bcub_dot)
BIC(bcub_dot)
lm<-logLik(bcub_dot)
lm

effe<-with(dati, Formula(dot_s ~0|0))
cub_dot00<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot00, digits = 3)
l00<-logLik(cub_dot00)
l00
#capacita esplicative 
e_x=(lx-l00)/(lm-l00)
#########################################
#valuto la cacità esplicativa di qcorsi_d
effe<-with(dati, Formula(dot_s ~eta_dott|0))
cub_dot_x<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_etadott=(lx-l00)/(lm-l00)
e_etadott# 0.006

###♠

effe<-with(dati, Formula(dot_s ~Scienze_uman_psico|0))
cub_dot_x<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_up=(lx-l00)/(lm-l00)
e_up# 0.006



##
effe<-with(dati, Formula(dot_s ~paper|paper))
cub_dot_x<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx

e_qcor=(-14620.96+14657.25)/(-13124.95+14657.25)
e_qcor    
e_paper=(lx-l00)/(lm-l00)
e_paper# 0.023

#####!
effe<-with(dati, Formula(dot_s ~estero_d|estero_d))
cub_dot_x<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx

e_qcor=(-14620.96+14657.25)/(-13124.95+14657.25)
e_qcor    
e_estdott=(lx-l00)/(lm-l00)
e_estdott# 0.024


#####

effe<-with(dati, Formula(dot_s ~0|Scienze_mediche))
cub_dot_x<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx

e_qcor=(-14620.96+14657.25)/(-13124.95+14657.25)
e_qcor    
e_scmed=(lx-l00)/(lm-l00)
e_scmed# 0.010


#####


effe<-with(dati, Formula(dot_s ~0|Sc_pol_social))
cub_dot_x<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx

e_qcor=(-14620.96+14657.25)/(-13124.95+14657.25)
e_qcor    
e_scpol=(lx-l00)/(lm-l00)
e_scpol# 0.007

####
effe<-with(dati, Formula(dot_s ~0|non_rifare_dott))
cub_dot_x<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx

e_qcor=(-14620.96+14657.25)/(-13124.95+14657.25)
e_qcor    
e_nfd=(lx-l00)/(lm-l00)
e_nfd# 0.509

####
effe<-with(dati, Formula(dot_s ~0|incerto_rifare_dott))
cub_dot_x<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx

e_qcor=(-14620.96+14657.25)/(-13124.95+14657.25)
e_qcor    
e_id=(lx-l00)/(lm-l00)
e_id# 0.189


####
effe<-with(dati, Formula(dot_s ~0|fuoric))
cub_dot_x<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx

e_qcor=(-14620.96+14657.25)/(-13124.95+14657.25)
e_qcor    
e_fc=(lx-l00)/(lm-l00)
e_fc# 0.004


####



sodddot<-as.factor(dati$dot_s)
class(sodddot)
pom<-polr(sodddot~ eta_dott+Scienze_uman_psico+
            paper+estero_d+didatt+
            non_rifare_dott+sesso+
            Scienze_mediche+Sc_pol_social+
            incerto_rifare_dott+fuoric, method="logistic", data = dati)
summary(pom)
#dati$non
step.mod<-stepAIC(pom,trace=FALSE, direction = "backward", k=log(nrow(dati)))
step.mod<-stepAIC(pom,trace=FALSE, direction = "backward")

summary(step.mod)
BIC(step.mod)
logLik(step.mod)
library(brant)
brant(step.mod)
#conf<-confint(pom)
tab<-exp(cbind(OR=step.mod$coefficients))
tab<-exp(cbind(-pom$coefficients, pom$coefficients))
tab
exp(0.012)

lm<-logLik(step.mod)
lm

BIC(pom)
BIC(step.mod)

pom0<-polr(formula=sodddot~1 , method="logistic", data = centro)
summary(pom0)
l00<-logLik(pom0)
l00
#capacita esplicative 
e_x=(lx-l00)/(lm-l00)
#########################################
#capacita esplicative di paper
pom_x<-polr(formula=sodddot~paper, method="logistic", data = centro)
summary(pom_x)
lx<-logLik(pom_x)
lx

e_qc=(-2923.559+3546.832)/(-2256.311+3546.832)
e_qc
e_paper=(lx-l00)/(lm-l00)
e_paper #Dati 0.028
#Nord 0.029
#sud 0.018

#capacita esplicative di est dott
pom_x<-polr(formula=sodddot~estero_d, method="logistic", data = centro)
summary(pom_x)
lx<-logLik(pom_x)
lx

e_qc=(-2923.559+3546.832)/(-2256.311+3546.832)
e_qc
e_ed=(lx-l00)/(lm-l00)
e_ed #dati 0.021
# nord 0.025
#centro 0.022
#sud 0.0199

#capacita esplicative di est dott
pom_x<-polr(formula=sodddot~sesso, method="logistic", data = nord)
summary(pom_x)
lx<-logLik(pom_x)
lx

e_qc=(-2923.559+3546.832)/(-2256.311+3546.832)
e_qc
e_sex=(lx-l00)/(lm-l00)
e_sex # dati 0.019



#capacita esplicative di sc pol
pom_x<-polr(formula=sodddot~Sc_pol_social, method="logistic", data = dati)
summary(pom_x)
lx<-logLik(pom_x)
lx

e_qc=(-2923.559+3546.832)/(-2256.311+3546.832)
e_qc
e_scpol=(lx-l00)/(lm-l00)
e_scpol #0.007

#capacita esplicative di Scienze_mediche
pom_x<-polr(formula=sodddot~Scienze_mediche, method="logistic", data = nord)
summary(pom_x)
lx<-logLik(pom_x)
lx

e_qc=(-2923.559+3546.832)/(-2256.311+3546.832)
e_qc
e_scmed=(lx-l00)/(lm-l00)
e_scmed #dati 0.011
# nord 0.022

#capacita esplicative di non_rifare_dott
pom_x<-polr(formula=sodddot~non_rifare_dott, method="logistic", data = centro)
summary(pom_x)
lx<-logLik(pom_x)
lx

e_qc=(-2923.559+3546.832)/(-2256.311+3546.832)
e_qc
e_nfd=(lx-l00)/(lm-l00)
e_nfd #dati 0.542
#nord 0.551
#centro 0.59
#sud 0.498

#capacita esplicative di invcerto
pom_x<-polr(formula=sodddot~incerto_rifare_dott, method="logistic", data = centro)
summary(pom_x)
lx<-logLik(pom_x)
lx

e_qc=(-2923.559+3546.832)/(-2256.311+3546.832)
e_qc
e_id=(lx-l00)/(lm-l00)
e_id #dati 0.153
#nord 0.136
# centro 0.150
#sud 0.229 

#capacita esplicative di fuoric
pom_x<-polr(formula=sodddot~fuoric, method="logistic", data = dati)
summary(pom_x)
lx<-logLik(pom_x)
lx

e_qc=(-2923.559+3546.832)/(-2256.311+3546.832)
e_qc
e_fc=(lx-l00)/(lm-l00)
e_fc #0.006

#fare le ripartizioni ranking
##Nord
effe<-Formula(dott_nord$dot_s~dott_nord$eta_dott+
                dott_nord$non_rifare_dott+
                dott_nord$didatt
              |dott_nord$sesso+dott_nord$Scienze_mediche+dott_nord$fuoric
              +dott_nord$Sc_pol_social+dott_nord$non_rifare_dott+dott_nord$incerto_rifare_dott+
                dott_nord$paper+dott_nord$estero_d)

bcub_dot<-fastCUB(effe,data=dott_nord,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bcub_dot, digits = 3)
BIC(bcub_dot, digits = 3)

## Centro

effe<-Formula(dott_centro$dot_s~dott_centro$Scienze_uman_psico+
                dott_centro$paper+dott_centro$estero_d
              |dott_centro$non_rifare_dott+dott_centro$incerto_rifare_dott+
                dott_centro$fuoric+ dott_centro$paper+dott_centro$estero_d)


bcub_dot<-fastCUB(effe,data=dott_centro,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bcub_dot, digits = 3)
BIC(bcub_dot, digits = 3)


### sud
effe<-Formula(dott_sud$dot_s~dott_sud$Scienze_uman_psico+
                dott_sud$paper
              |dott_sud$non_rifare_dott+dott_sud$incerto_rifare_dott+
                dott_sud$paper+dott_sud$estero_d)

bcub_dot<-fastCUB(effe,data=dott_sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bcub_dot, digits = 3)
BIC(bcub_dot, digits = 3)



summary(bcub_dot)
BIC(bcub_dot)
lm<-logLik(bcub_dot)
lm

effe<-with(dott_sud, Formula(dot_s ~0|0))
cub_dot00<-fastCUB(effe,data=dott_sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot00, digits = 3)
l00<-logLik(cub_dot00)
l00
#capacita esplicative 
e_x=(lx-l00)/(lm-l00)
#########################################
#valuto la cacità esplicativa di eta_dott
effe<-with(dott_nord, Formula(dot_s ~eta_dott|0))
cub_dot_x<-fastCUB(effe,data=dott_nord,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx 

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_etadott=(lx-l00)/(lm-l00)
e_etadott# nord 0.004 eta_dottorato





###♠
#valuto la cacità esplicativa di didatt
effe<-with(dott_nord, Formula(dot_s ~didatt|0))
cub_dot_x<-fastCUB(effe,data=dott_nord,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx 

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_didat=(lx-l00)/(lm-l00)
e_didat# NORD didatt 0.005


###♠

#valuto la cacità esplicativa di SC um e psico
effe<-with(dott_sud, Formula(dot_s ~Scienze_uman_psico|0))
cub_dot_x<-fastCUB(effe,data=dott_sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx 

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_scumps=(lx-l00)/(lm-l00)
e_scumps# centro sc um e psico 0.008
#sud 0.002


#valuto la cacità esplicativa di qcorsi_d
effe<-with(dott_sud, Formula(dot_s ~0|estero_d))
cub_dot_x<-fastCUB(effe,data=dott_sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx 

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_estero_dot=(lx-l00)/(lm-l00)
e_estero_dot ## nord 0.009
#Centro estero_dott d xi e pi 0.036
#sud estero_dott  0.006
###♠

#valuto la cacità esplicativa di qcorsi_d
effe<-with(dott_sud, Formula(dot_s ~0|non_rifare_dott))
cub_dot_x<-fastCUB(effe,data=dott_sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx 

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_nfd=(lx-l00)/(lm-l00)
e_nfd
#♠ centro non rifare dott 0.517
# nord non rifare dott 0.524
# sud 0.471
###♠


#valuto la cacità esplicativa di qcorsi_d
effe<-with(dott_nord, Formula(dot_s ~0|sesso))
cub_dot_x<-fastCUB(effe,data=dott_nord,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx 

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_sex=(lx-l00)/(lm-l00)
e_sex  #nord genere 0.023
###♠

#valuto la cacità esplicativa di qcorsi_d
effe<-with(dott_nord, Formula(dot_s ~0|Scienze_mediche))
cub_dot_x<-fastCUB(effe,data=dott_nord,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx 

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_scmed=(lx-l00)/(lm-l00)
e_scmed# nord  sc mediche 0.018
###♠

#valuto la cacità esplicativa di qcorsi_d
effe<-with(dott_nord, Formula(dot_s ~0|Sc_pol_social))
cub_dot_x<-fastCUB(effe,data=dott_nord,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx 

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_scpol=(lx-l00)/(lm-l00)
e_scpol# nord sc pol e sociali 0.008

#fine centro

###♠
#valuto la cacità esplicativa di qcorsi_d
effe<-with(dott_sud, Formula(dot_s ~0|incerto_rifare_dott))
cub_dot_x<-fastCUB(effe,data=dott_sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx 

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_id=(lx-l00)/(lm-l00)
e_id# nord incerto rifare dott 0.180
# Centro incerto rifare dott 0.161
#sud 0.239

###♠

#valuto la cacità esplicativa di paper
effe<-with(dott_sud, Formula(dot_s ~paper|paper))
cub_dot_x<-fastCUB(effe,data=dott_sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx 

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_pap=(lx-l00)/(lm-l00)
e_pap# nord  0.020
#centro 0.039
# sud 0.009

###♠


#valuto la cacità esplicativa di qcorsi_d
effe<-with(dott_nord, Formula(dot_s ~0|fuoric))
cub_dot_x<-fastCUB(effe,data=dott_nord,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx 

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_fc=(lx-l00)/(lm-l00)
e_fc# nord 0.003
#centro 0.014

###♠
#### Lavoro intero campione


effe<-Formula(dati$sodd~dati$eta_dott+dati$Scienze_uman_psico+
                dati$stciv+ dati$ateneoCentro+dati$ateneoSud+
                dati$lavoroEstero+dati$ore
              |dati$sesso+dati$Sc_pol_social+
                dati$altro_tipolav+dati$lavoroEstero+dati$ore)


bcub_soddx<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bcub_soddx, digits = 3)
BIC(bcub_soddx, digits = 3)


####nord 
effe<-with(nord,Formula(sodd~eta_dott+ore
                        |sesso+altro_tipolav))


bcub_soddx<-fastCUB(effe,data=nord,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bcub_soddx, digits = 3)
BIC(bcub_soddx, digits = 3)
##### centro
effe<-with(centro,Formula(sodd~Scienze_uman_psico
                          |sesso+Sc_pol_social+
                            altro_tipolav+ore))


bcub_soddx<-fastCUB(effe,data=centro,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bcub_soddx, digits = 3)
BIC(bcub_soddx, digits = 3)

####
#SUD
effe<-with(sud,Formula(sodd~stciv
                       |eta_dott+
                         Sc_giurid+altro_tipolav+
                         ore))


bcub_soddx<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bcub_soddx, digits = 3)
BIC(bcub_soddx, digits = 3)



summary(bcub_soddx)
BIC(bcub_soddx)
lm<-logLik(bcub_soddx)
lm

effe<-with(sud, Formula(sodd ~0|0))
cub_dot00<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot00, digits = 3)
l00<-logLik(cub_dot00)
l00
#capacita esplicative 
e_x=(lx-l00)/(lm-l00)
#########################################
#valuto la cacità esplicativa di eta_dott
effe<-with(sud, Formula(sodd ~0|eta_dott))
cub_dot_x<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx 

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_etadott=(lx-l00)/(lm-l00)
e_etadott# dati 0.050 eta_dottorato
#nord 0.098
#sud 0.102

#valuto la cacità esplicativa di sc giridiche
effe<-with(sud, Formula(sodd ~0|Sc_giurid))
cub_dot_x<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx 

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_scgiu=(lx-l00)/(lm-l00)
e_scgiu#
#sud 0.199

###♠


###♠
#valuto la cacità esplicativa di eta_dott
effe<-with(centro, Formula(sodd ~Scienze_uman_psico|0))
cub_dot_x<-fastCUB(effe,data=centro,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx 

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_scumps=(lx-l00)/(lm-l00)
e_scumps# dati 0.016 
# centro 0.081

###♠
#valuto la cacità esplicativa di eta_dott
effe<-with(sud, Formula(sodd ~stciv|0))
cub_dot_x<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx 

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_stciv=(lx-l00)/(lm-l00)
e_stciv# dati 0.010 
# sud 0.119

###♠

#valuto la cacità esplicativa di eta_dott
effe<-with(dati, Formula(sodd ~ateneoCentro|0))
cub_dot_x<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx 

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_atecen=(lx-l00)/(lm-l00)
e_atecen# dati 0.005 


###♠

#valuto la cacità esplicativa di eta_dott
effe<-with(dati, Formula(sodd ~ateneoSud|0))
cub_dot_x<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx 

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_atesud=(lx-l00)/(lm-l00)
e_atesud# dati 0.029 


###♠

#valuto la cacità esplicativa di eta_dott
effe<-with(dati, Formula(sodd ~lavoroEstero|lavoroEstero))
cub_dot_x<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx 

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_lavest=(lx-l00)/(lm-l00)
e_lavest# dati 0.561


###♠

#valuto la cacità esplicativa di eta_dott
effe<-with(sud, Formula(sodd ~0|ore))
cub_dot_x<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx 

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_ore=(lx-l00)/(lm-l00)
e_ore# dati 0.159 
# nord 0.117
# centro 0.198
#sud 0.274
###♠

#valuto la cacità esplicativa di eta_dott
effe<-with(centro, Formula(sodd ~0|sesso))
cub_dot_x<-fastCUB(effe,data=centro,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx 

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_sex=(lx-l00)/(lm-l00)
e_sex# dati 0.073
#nord 0.219 
#centro 0.197

###♠

#valuto la cacità esplicativa di eta_dott
effe<-with(centro, Formula(sodd ~0|Sc_pol_social))
cub_dot_x<-fastCUB(effe,data=centro,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx 

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_scpol=(lx-l00)/(lm-l00)
e_scpol# dati 0.018 
#centro 0.116
###♠

#valuto la cacità esplicativa di eta_dott
effe<-with(sud, Formula(sodd ~0|altro_tipolav))
cub_dot_x<-fastCUB(effe,data=sud,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(cub_dot_x, digits = 3)
lx<-logLik(cub_dot_x)
lx 

e_qcor=(-4859.621+5521.134)/(-4036.311+5521.134)
e_qcor    
e_altrolav=(lx-l00)/(lm-l00)
e_altrolav# dati 0.154 
# nord 0.621
#centro 0.435
# sud 0.310
###♠
#POM#


soddlav<-as.factor(dati$sodd)
class(soddlav)
### POM NO DICOTOMIZZ
pom<-polr(soddlav~ eta_dott+Scienze_uman_psico+
            stciv+ ateneoCentro+ateneoSud+
            lavoroEstero+ore
          +sesso+Sc_pol_social+
            altro_tipolav+lavoroEstero+ore, method="logistic", data = dati)
summary(pom)

step.mod<-stepAIC(pom,trace=FALSE, direction = "backward", k=log(nrow(dati)))
step.mod<-stepAIC(pom,trace=FALSE, direction = "backward")

summary(step.mod)
BIC(step.mod)
logLik(step.mod)
brant(step.mod)

lm<-logLik(step.mod)
lm

BIC(pom)
BIC(step.mod)
#soddlav<-as.factor(dati$sodd)
pom0<-polr(formula=soddlav~1 , method="logistic", data = sud)
summary(pom0)
l00<-logLik(pom0)
l00
#capacita esplicative 
e_x=(lx-l00)/(lm-l00)
#########################################
#capacita esplicative di paper
pom_x<-polr(formula=soddlav~eta_dott, method="logistic", data = centro)
summary(pom_x)
lx<-logLik(pom_x)
lx

e_qc=(-2923.559+3546.832)/(-2256.311+3546.832)
e_qc
e_etadot=(lx-l00)/(lm-l00)
e_etadot #Dati 0.061
#Nord 0.055

#sud 0.018
######
#capacita esplicative di paper
pom_x<-polr(formula=soddlav~stciv, method="logistic", data = centro)
summary(pom_x)
lx<-logLik(pom_x)
lx

e_qc=(-2923.559+3546.832)/(-2256.311+3546.832)
e_qc
e_stciv=(lx-l00)/(lm-l00)
e_stciv #Dati 0.018
#Nord 0.073

#centro 0.224
######

#capacita esplicative di paper
pom_x<-polr(formula=soddlav~sesso, method="logistic", data = nord)
summary(pom_x)
lx<-logLik(pom_x)
lx

e_qc=(-2923.559+3546.832)/(-2256.311+3546.832)
e_qc
e_sex=(lx-l00)/(lm-l00)
e_sex #Dati 0.082
#Nord 0.223

#sud 0.018
######
#capacita esplicative di paper
pom_x<-polr(formula=soddlav~Sc_pol_social, method="logistic", data = dati)
summary(pom_x)
lx<-logLik(pom_x)
lx

e_qc=(-2923.559+3546.832)/(-2256.311+3546.832)
e_qc
e_scpol=(lx-l00)/(lm-l00)
e_scpol #Dati 0.021


#sud 0.018
######
#capacita esplicative di paper
pom_x<-polr(formula=soddlav~lavoroEstero, method="logistic", data = dati)
summary(pom_x)
lx<-logLik(pom_x)
lx

e_qc=(-2923.559+3546.832)/(-2256.311+3546.832)
e_qc
e_le=(lx-l00)/(lm-l00)
e_le #Dati 0.587


#sud 0.018
######
#capacita esplicative di paper
pom_x<-polr(formula=soddlav~altro_tipolav, method="logistic", data = sud)
summary(pom_x)
lx<-logLik(pom_x)
lx

e_qc=(-2923.559+3546.832)/(-2256.311+3546.832)
e_qc
e_altro=(lx-l00)/(lm-l00)
e_altro #Dati 0.142
#Nord 0.533
#centro 0.484
#sud 0.466
######

#capacita esplicative di paper
pom_x<-polr(formula=soddlav~ore, method="logistic", data = sud)
summary(pom_x)
lx<-logLik(pom_x)
lx

e_qc=(-2923.559+3546.832)/(-2256.311+3546.832)
e_qc
e_ore=(lx-l00)/(lm-l00)
e_ore #Dati 0.173
#Nord 0.178
#centro 0.328
#sud 0.476
######



ggplot(sud, aes(x=sodd)) +
  geom_histogram(binwidth=1, colour="black", fill="white")


ggplot(dati, aes(x=dati$sodd)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=1,
                 colour="black", fill="white")+
  stat_function(fun = dnorm, args = list(mean = mean(dati$sodd), sd = sd(dati$sodd))+
                  theme_bw() +
                  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
                ggtitle("we"))

hist(dati$dot_s, col="red")
c<-1:10
hist(dati$dot_s, breaks = seq(1,8,1), col="white",   xlab="Valutazione espressa", ylab= "Frequenza",
        main="Diagramma a barre di dot")
boxplot(dati$dot_s, main="Box-plot di dot")

xfit<-seq(min(dati$dot_s),max(dati$sodd),length=40)
yfit<-dnorm(xfit,mean=mean(dati$dot_s),sd=sd(dati$dot_s))
yfit <- yfit*diff(h$mids[1:2])*length(dati$dot_s)
lines(xfit, yfit, col="blue", lwd=2)

ggplot(dati, aes(x=dati$sodd)) +
  geom_bar(stat="count", colour="red")+
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())

ggplot(data = dati, mapping = aes(x = sodd), scale_x_discrete() )+
  geom_bar()
soddisfazione_lavoro<-factor(dati$sodd)
  ggplot(dati)+
  stat_count(mapping= aes(x=soddisfazione_lavoro)) 

  geom_bar(stat="count", colour="black", fill="white")+
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
   ggplot(mpg, aes(dati$sodd))+
   geom_bar()
  
   x, alpha, color, fill, 

hist(dati$sodd)
hist(dati$dot_s)

skim(dati$dot_s)
summary(dati$dot_s)
table(dati$dot_s)
glimpse(dati)
mean(dati$dot_s)
var(dati$dot_s)
skewness(dati$dot_s)
kurtosis(dati$dot_s)
(4.996-6)/1.900
skew(dati$dot_s)
skewness(dati$dot_s)


skim(dati$sodd)
summary(dati$sodd)
table(dati$sodd)
glimpse(dati)
mean(dati$sodd)
var(dati$sodd)
skewness(dati$dot_s)
kurtosis(dati$dot_s)
(5.184-6)/2.852
skew(dati$sodd)
skewness(dati$sodd)




skew(dati$dot_s)
skewness(dati$dot_s) 

hist(dati$sodd, breaks = seq(1,8,1), col="white",   xlab="Valutazione espressa", ylab= "Frequenza",
     main="Diagramma a barre di sodd")
boxplot(dati$sodd, main="Box-plot di sodd")


XT<-table(dati$sodd)/7395

barplot(XT,  
        col="dark gray",
        main = "Diagramma a barre di sodd",
        xlab="Scala di valutazione",
        ylab="Frequenza",
        ylim = c(0,0.3))


table(dati$sodd)/dim(dati)

XD<- xtabs(~ dati$sodd, dati)
sum(XD)
pctXT<-prop.table(XD)

pctXT<-as_tibble(pctXT)

pctXT %>% 
  ggplot(aes(x=`dati$sodd`,y=n))+
  geom_col(fill="navyblue")+
  theme_bw()+
  theme(panel.grid =  element_blank())+
  labs(x="sodd",y="Frequenze relative")+
  ylim(0,0.30)


barplot(pctXT,  
        col="dark gray",
        main = "Diagramma a barre di sodd",
        xlab="Scala di valutazione",
        ylab="Frequenza",
        
        
        ylim = c(0,1))

table(coorte$dot_s)/dim(coorte)

XD<- xtabs(~ dati$dot_s, dati)
sum(XD)
pctXT<-prop.table(XD)

pctXT<-as_tibble(pctXT)

pctXT %>% 
  ggplot(aes(x=`dati$dot_s`,y=n))+
  geom_col(fill="red")+
  theme_bw()+
  theme(panel.grid =  element_blank())+
  labs(x="Soddisfazione per il dottorato (coorte 2014)",y="Frequenze relative")+
  #scale_x_discrete(labels=c("0" = "No", "1" = "Si"))+
  ylim(0,0.3)                 
                   
  labs(x="Area dottorato",y="Frequenze relative")+
  scale_x_discrete(labels=c("1" = "Scienze MMFFNN", "2" = "Sc. mediche", "3"="Sc. agrarie e veterinarie","4"="Ingegneria", "5"="Sc. umane e psicologiche","6"="Sc. giuridiche","7"="Sc. economiche e statistiche","8"="Sc. politiche e sociali"),guide = guide_axis(n.dodge = 3)) 

p + scale_x_discrete(labels=c("0" = "M", "1" = "F")

cub_dot<-GEM(Formula(dot_s~0|0|0), family="cub",data=dati)
summary(cub_dot,digits=3)

makeplot(cub_dot)


cub_sodd<-GEM(Formula(sodd~0|0|0), family="cub",data=dati)
summary(cub_sodd,digits=3)

makeplot(cub_sodd)


barplot(XD,  
        col="dark gray",
        main = "Diagramma a barre di dot",
        xlab="Scala di valutazione",
        ylab="Frequenza",
        ylim=c(0,2000))

boxplot(dati$qcorsi_d,dati$ncorsi_s,dati$dot_s,
        data=dati,names = c("sodd", "ncorsi", "dot"),
        main = "Box-plot ")

  
coorte$ate7mod<-fct_collapse(factor(coorte$ate7mod), 
                      "Agricoltura"   = c("1"),
                      "Industria" = c("2"),
                      "Istruzione" = c("3","4"),
                    "Ricerca e Sviluppo"=c("5","6"),
                    "Altro"=c("7"))

coorte$cod_area_w <- fct_collapse(factor(coorte$cod_area_w), 
                                  "Sc. MM FF NN" = c("1", "2","3","4","5"),
                                  "Sc. mediche" = c("6"),
                                  "Sc. agrarie e veterinarie" = c("7"),
                                  "Ingegneria " = c("8", "9"),
                                  "Sc. umanistiche e psicologiche"=c("10","11"),
                                  "Sc. giuridiche"= c("12"),
                                  "Sc. economiche e statistiche" =c("13"),
                                  "Sc politiche e sociali"=c("14"))


unique(coorte$cod_area_w)


X_dott$riplav <- fct_collapse(X_dott$riplav, 
                              "Nord"   = c("1", "2"),
                              "Centro" = c("3"),
                              "Sud" = c("4", "5"),
                              "Estero" =c("9"))

coorte$rifare_d<-fct_collapse(factor(coorte$rifare_d), 
                            "Si"   = c("1"),
                            "No" = c("2"),
                            "Non so" = c("3"))
levels(X_dott$viverip)
levels(coorte$cod_area_w)
we<-coorte %>% 
  filter(viverip!="Estero") %>% 
  droplevels()

XD<- xtabs(~ coorte$cod_area_w) 
pctXT<-prop.table(XD)

pctXT<-as_tibble(pctXT)

pctXT %>%
  ggplot(aes(x=`coorte$cod_area_w`,y=n))+
  geom_col(fill="gold1")+
  theme_bw()+
  theme(panel.grid =  element_blank())+
  labs(x="Settore lavoro (ate7mod)",y="Frequenze relative")+
  scale_x_discrete(labels=c("1" = "Agricoltura", "2" = "Industria", "3"="Istruzione","4"="Ricerca e Sviluppo","5"="Altro"))+
  theme(axis.text.x = element_text(angle = 90))

+
  ylim(0,0.4)



pctXT %>%
  ggplot(aes(x=`coorte$cod_area_w`,y=n))+
  geom_col(fill="firebrick")+
  theme_bw()+
  theme(panel.grid =  element_blank())+
  labs(x="Area dottorato",y="Frequenze relative")+
   scale_x_discrete(labels=c("1" = "Sc.MMFFNN", "2" = "Sc.mediche", "3"="Sc.agrarie e veterinarie","4"="Ingegneria", "5"="Sc.umane e psicologiche","6"="Sc.giuridiche","7"="Sc.economiche e statistiche","8"="Sc. politiche e sociali"))+ #,guide = guide_axis(n.dodge = 3)) +
  theme(axis.text.x = element_text(angle = 90))+
  ylim(0,0.4)+
  coord_flip()


p + scale_x_discrete(labels=c("0" = "M", "1" = "F",
                              "2" = "Dose 2"))
glimpse(coorte$paper)

coorte %>% 
  ggplot(aes(x=coorte$paper))+
  geom_histogram(binwidth = 3, fill="royalblue")+
  scale_x_continuous(breaks = c(0,10,20,30,40,50))+
  labs(x="paper",y="Frequenza")+
  theme_bw()
  theme(panel.grid =  element_blank())


## Indice di eterogeneità normalizzato di gini
library(labstatR)
 
 > f <- factor(c("a", "b", "c", "b", "a", "c", "a", "b", "b", "c", + "a")) > f
 [1] a b c b a c a b b c a Levels: a b c
  k <- 8 
  n <- 7395
enne <- table(dati$sodd) 
enne
 E <- k/(k - 1) * (1 - 1/n^2 * sum(enne^2)) 
 E
 ##sodd
 E(dati$prof_s)
 [1] 0.941
 
 # da non mettere
 cv(dati$sodd)

 
 ## indice di laakso e taga 
 
 view(laakso)
 sodrel<-table(dati$sodd)/7395
 laakso(sodrel)
 
 lt<-table(dati$prof_s)/7395
 laakso(lt)
 we
 
 
 
 N<- 1/((0.034^2)+(0.052^2)+(0.070^2)+(0.131^2)+(0.238^2)+(0.260^2)+(0.142^2)+(0.068^2))
 
 
 
 
 library("electoral")
we<-c(253,390,521, 976, 1764, 1927,
     1056, 508)
enp(votes=we) 
 
 E(dati$dot_s)
 [1] 0.969
 E(dati$incora_s)
 #0990
 
 cv(dati$dot_s)

 vc <-varclus(~ dati$qcorsi_s+ dati$ncorsi_s +  dati$doce_s+dati$spazi_s +dati$train_s+dati$collab_s+dati$incora_s , sim= "spearman", data=dati)
 plot(vc) # Figure 8.2

 redun(~ dati$qcorsi_d+ dati$ncorsi_d +  dati$doce_d+dati$spazi_d +dati$train_d+dati$collab_d+dati$incora_d, r2=.3, type="adjusted" ,nk=0 ,iterms = TRUE, pc=TRUE,data=dati )
 
 library(Hmisc)
 getHdata(support)
 support <- support[complete.cases(support[, c("sfdm2", "adlsc", "sex", "age", "meanbp")]), ]
 sfdm <- as.integer (support$sfdm2 ) - 1
 
 sf1 <- function (y) {
   c(' Y ≥ 1 ' = qlogis (mean(sfdm >= 1)), 
     ' Y ≥ 2 ' = qlogis (mean(sfdm >= 2)),
     ' Y ≥ 3 ' = qlogis (mean(sfdm >= 3))
   )
 }
 11
 sf2 <- function (y) {
   c(' Y ≥ 1 ' = qlogis (mean(y >= 1)), 
     ' Y ≥ 2 ' = qlogis (mean(y >= 2)),
     ' Y ≥ 3 ' = qlogis (mean(y >= 3))
   )
 }
 
 s1 <- summary(sfdm ~ adlsc + sex + age + meanbp, fun=sf1,
               data = support)
 s2 <- summary(sfdm ~ adlsc + sex + age + meanbp, fun=sf2,
               data = support)  
 
 plot(s1, which =1:3, pch =1:3, xlab = ' logit ', main = ' ', width.factor = 1.4, cex.lab = 0.75)
 
 plot(s2, which =1:3, pch =1:3, xlab = ' logit ', main = ' ', width.factor = 1.4, cex.lab = 0.75)
 

 ### SVG multicub coorte 2014
CairoSVG("multicub2012.svg")
listord12<-dati[,c(1:16)]
labels<-names(dati)[c(1:16)] 
 
n<-length(listord12) 
n
vettcol<-symb<-rep(NA,n) 
vettcol
vettcol1<-rep("red",8) 
vettcol2<-rep("black",8)
symb[1:8]<-19
symb[9:16]<-2
symb
c(rep(19,8),rep(9,8))
multicub(listord12, labels = labels,
        main ="Modelli CUB per la coorte del 2012", pch =symb,pos = c(1,rep(3, ncol(listord12)-1)),ylim=c(0,1),xlim=c(0,1), cex=0.7, colours=c(vettcol2,vettcol1))
 
 
legend("bottomleft",legend=c("Batteria Lavoro","Batteria Dottorato"),
      cex=0.7,text.col="black",pch=c(2,19), 
      col=c("red","black"))
 
 
dev.off()

cub_sodd<-GEM(Formula(sodd~att_s+know_s+carrie_s+prof_s|att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s|att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s), family = "cube" ,data=sud)
cub_sodd<-GEM(Formula(sodd~0|0|0), family = "cub" ,data=sud)
cub_sodd<-GEM(Formula(sodd~att_s+redd_s+carrie_s+prof_s), family = "ihg" ,data=sud)

summary(cub_sodd)
BIC(cub_sodd)


E(dati$att_s)
E(dati$know_s)

cub_sodd<-GEM(Formula(dot_s~0), family = "cub" ,data=dati)
summary(cub_sodd)


## dissimilarità 
#sodd
cub_sodd<-GEM(Formula(sodd~0|0|0), family = "cub", data=sud)
summary(cub_sodd)
makeplot(cub_sodd)

predict.vgam(pom)

theor<-fitted(cub_sodd)[,1]
freq<-tabulate(dati$sodd, nbins = 8)/7395
dissim(freq, theor)##0.036
#c12=0.034

#att
cub_att<-GEM(Formula(att_s~0|0|0), family = "cub", data=dati)
summary(cub_att)
makeplot(cub_att)

theor<-fitted(cub_att)[,1]
freq<-tabulate(dati$att_s, nbins = 8)/7395
dissim(freq, theor)##0.089
#c12=0.092
#stab
cub_stab<-GEM(Formula(stab_s~0|0|0), family = "cub", data=dati)
summary(cub_stab)
makeplot(cub_stab)

theor<-fitted(cub_stab)[,1]
freq<-tabulate(dati$stab_s, nbins = 8)/7395
dissim(freq, theor)##0.050
#c12=0.088
#prof
cub_prof<-GEM(Formula(prof_s~0|0|0), family = "cub", data=dati)
summary(cub_prof)
makeplot(cub_prof)

theor<-fitted(cub_prof)[,1]
freq<-tabulate(dati$prof_s, nbins = 8)/7395
dissim(freq, theor)##0.058
#c12=0.066
#know
cub_know<-GEM(Formula(know_s~0|0|0), family = "cub", data=dati)
summary(cub_know)
makeplot(cub_know)

theor<-fitted(cub_know)[,1]
freq<-tabulate(dati$know_s, nbins = 8)/7395
dissim(freq, theor)##0.084
#c12=0.075
#redd
cub_redd<-GEM(Formula(redd_s~0|0|0), family = "cub", data=dati)
summary(cub_redd)
makeplot(cub_redd)

theor<-fitted(cub_redd)[,1]
freq<-tabulate(dati$redd_s, nbins = 8)/7395
dissim(freq, theor)##0.084
#c12=0.088
#auton
cub_aut<-GEM(Formula(auton_s~0|0|0), family = "cub", data=dati)
summary(cub_aut)
makeplot(cub_aut)

theor<-fitted(cub_aut)[,1]
freq<-tabulate(dati$auton_s, nbins = 8)/7395
dissim(freq, theor)##0.077
#c12=0.092
#carrie
cub_car<-GEM(Formula(carrie_s~0|0|0), family = "cub", data=dati)
summary(cub_car)
makeplot(cub_car)

theor<-fitted(cub_car)[,1]
freq<-tabulate(dati$carrie_s, nbins = 8)/7395
dissim(freq, theor)##0.082
#c12=0.093
### batteria dottorato
cub_dot<-GEM(Formula(dot_s~0|0|0), family = "cub", data=dati)
summary(cub_dot)
makeplot(cub_dot)

theor<-fitted(cub_dot)[,1]
freq<-tabulate(dati$dot_s, nbins = 8)/7395
dissim(freq, theor)##0.040
#c12=0.030
# doce
cub_doce<-GEM(Formula(doce_s~0|0|0), family = "cub", data=dati)
summary(cub_doce)
makeplot(cub_doce)

theor<-fitted(cub_doce)[,1]
freq<-tabulate(dati$doce_s, nbins = 8)/7395
dissim(freq, theor)##0.076
#c12=0.077
# incora
cub_inc<-GEM(Formula(incora_s~0|0|0), family = "cub", data=dati)
summary(cub_inc)
makeplot(cub_inc)

theor<-fitted(cub_inc)[,1]
freq<-tabulate(dati$incora_s, nbins = 8)/7395
dissim(freq, theor)##0.098
#c12=0.085
# collab
cub_col<-GEM(Formula(collab_s~0|0|0), family = "cub", data=dati)
summary(cub_col)
makeplot(cub_col)

theor<-fitted(cub_col)[,1]
freq<-tabulate(dati$collab_s, nbins = 8)/7395
dissim(freq, theor)##0.065
#c12=0.054
# train
cub_train<-GEM(Formula(train_s~0|0|0), family = "cub", data=dati)
summary(cub_train)
makeplot(cub_doce)

theor<-fitted(cub_train)[,1]
freq<-tabulate(dati$train_s, nbins = 8)/7395
dissim(freq, theor)##0.052
#c12=0.052
# spazi
cub_spa<-GEM(Formula(spazi_s~0|0|0), family = "cub", data=dati)
summary(cub_spa)
makeplot(cub_doce)

theor<-fitted(cub_spa)[,1]
freq<-tabulate(dati$spazi_s, nbins = 8)/7395
dissim(freq, theor)##0.053
#c12=0.053
# doce
cub_qco<-GEM(Formula(qcorsi_s~0|0|0), family = "cub", data=dati)
summary(cub_qco)
makeplot(cub_qco)

theor<-fitted(cub_qco)[,1]
freq<-tabulate(dati$qcorsi_s, nbins = 8)/7395
dissim(freq, theor)##0.088
#c12=0.082

# ncorsi
cub_nco<-GEM(Formula(ncorsi_s~0|0|0), family = "cub", data=dati)
summary(cub_nco)
makeplot(cub_doce)

theor<-fitted(cub_nco)[,1]
freq<-tabulate(dati$ncorsi_s, nbins = 8)/7395
dissim(freq, theor)##0.101
#c12=0.102
dim(dati)
#SVG multicub coortr 2012

#rms per grafico assunzione di proporzionalità
# harrell regression strategies modelling
getHdata (support)
library(Hmisc)
sf <- as.integer (support $sfdm2 ) - 1
sf<- function (y)
 c( ' Y ≥ 1 ' =qlogis (mean (y ≥ 1)), ' Y ≥ 2 ' =qlogis (mean (y ≥ 2)),
    ' Y ≥ 3 ' =qlogis (mean (y ≥ 3)))
s <- summary(sfdm ∼ adlsc + sex + age + meanbp , fun=sf ,
             data =support)
plot (s, which =1:3 , pch =1:3 , xlab = ' logit ' , vnames = ' names ' ,
     main = ' ' , width.factor =1.5)
 
 









 
#####misura alternativa della capacità esplicativa 
### Lx-Lm/Lm
# Per ogni covariata X, 
#il modello Mx  è ottenuto da M omettendo
#la covariata X.  L(M) è il modello migliore usato come benchmark 
# L(M) e L(Mx) sono le loglik dei due modelli 
# ulteriore misura della capacità esplicativa
#E = (L(Mx) - L(Mx))/L(M)

# migliori modelli per l'intero campione
#dottorato 
#covar NO DICOTOM
effe<-Formula(dati$dot_s~dati$train_s|dati$qcorsi_s+dati$ncorsi_s+dati$doce_s+dati$spazi_s+dati$train_s+dati$collab_s+dati$incora_s)
###dicotm 7
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)     
#dicotom a 6
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)

bestcub_dot<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bestcub_dot)
BIC(bestcub_dot)
lm<-logLik(bestcub_dot)
lm
# valuto la capacità esplicativa delle covariate X
#di dot intero campione
## NO DICOTOM 
# Train
effe<-Formula(dati$dot_s~dati$train_s|dati$qcorsi_s+dati$ncorsi_s+dati$doce_s+dati$spazi_s+dati$train_s+dati$collab_s+dati$incora_s)
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)     
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)

effe<-Formula(dati$dot_s~0|dati$qcorsi_s+dati$ncorsi_s+dati$doce_s+dati$spazi_s+dati$collab_s+dati$incora_s)
effe<-Formula(dati$dot_s~dati$doce_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$collab_d+dati$incora_d)     
effe<-Formula(dati$dot_s~dati$doce_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$collab_d+dati$incora_d)
dot_ex<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(dot_ex)
BIC(dot_ex)

lx<-logLik(dot_ex)
lx

e_train= (lx - lm)/lm
e_train#0.0124 numeriche
###dicotomica 0.0020
# dico 6 0.0048 

e=(-10497.22+10368.21)/-10368.21
e

# qcorsi
effe<-Formula(dati$dot_s~dati$train_s|dati$qcorsi_s+dati$ncorsi_s+dati$doce_s+dati$spazi_s+dati$train_s+dati$collab_s+dati$incora_s)
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)     
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)

effe<-Formula(dati$dot_s~dati$train_s|dati$ncorsi_s+dati$doce_s+dati$spazi_s+dati$train_s+dati$collab_s+dati$incora_s)
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)     
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)
dot_ex<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(dot_ex)
BIC(dot_ex)

lx<-logLik(dot_ex)
lx

e_qcorsi= (lx - lm)/lm
e_qcorsi#0.006 numeriche
#dicotomica 7  0.0014
#dico 6 0.00406

e=(-10497.22+10368.21)/-10368.21
e


# ncorsi
effe<-Formula(dati$dot_s~dati$train_s|dati$qcorsi_s+dati$ncorsi_s+dati$doce_s+dati$spazi_s+dati$train_s+dati$collab_s+dati$incora_s)
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)     
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)

effe<-Formula(dati$dot_s~dati$train_s|dati$qcorsi_s+dati$doce_s+dati$spazi_s+dati$train_s+dati$collab_s+dati$incora_s)
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)     
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)
dot_ex<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(dot_ex)
BIC(dot_ex)

lx<-logLik(dot_ex)
lx

e_ncorsi= (lx - lm)/lm
e_ncorsi#0.003 numerica
#dicotomica 0.00082
# dico 6 0.0021

e=(-10497.22+10368.21)/-10368.21
e


# doce
effe<-Formula(dati$dot_s~dati$train_s|dati$qcorsi_s+dati$ncorsi_s+dati$doce_s+dati$spazi_s+dati$train_s+dati$collab_s+dati$incora_s)
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)     
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)

effe<-Formula(dati$dot_s~dati$train_s|dati$qcorsi_s+dati$ncorsi_s+dati$spazi_s+dati$train_s+dati$collab_s+dati$incora_s)
effe<-Formula(dati$dot_s~dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)     
effe<-Formula(dati$dot_s~dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)
dot_ex<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(dot_ex)
BIC(dot_ex)

lx<-logLik(dot_ex)
lx

e_doce= (lx - lm)/lm
e_doce#0.005 originale
# dicotom 0.0065
# dico 6 0.0092

e=(-10497.22+10368.21)/-10368.21
e


# spazi
effe<-Formula(dati$dot_s~dati$train_s|dati$qcorsi_s+dati$ncorsi_s+dati$doce_s+dati$spazi_s+dati$train_s+dati$collab_s+dati$incora_s)
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)     
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)

effe<-Formula(dati$dot_s~dati$train_s|dati$qcorsi_s+dati$ncorsi_s+dati$doce_s+dati$train_s+dati$collab_s+dati$incora_s)
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d)     
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d)
dot_ex<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(dot_ex)
BIC(dot_ex)

lx<-logLik(dot_ex)
lx

e_spazi= (lx - lm)/lm
e_spazi#0.004 numerica
# dicotom 0.0016
#dico 6 0.00289

e=(-10497.22+10368.21)/-10368.21
e

# collab
effe<-Formula(dati$dot_s~dati$train_s|dati$qcorsi_s+dati$ncorsi_s+dati$doce_s+dati$spazi_s+dati$train_s+dati$collab_s+dati$incora_s)
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)     
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)

effe<-Formula(dati$dot_s~dati$train_s|dati$qcorsi_s+dati$ncorsi_s+dati$doce_s+dati$spazi_s+dati$train_s+dati$incora_s)
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$incora_d)     
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$incora_d)
dot_ex<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(dot_ex)
BIC(dot_ex)

lx<-logLik(dot_ex)
lx

e_collab= (lx - lm)/lm
e_collab#0.0060 originale
#dico 7 0.00437
#dico 6 0.00695
e=(-10497.22+10368.21)/-10368.21
e

# incora
effe<-Formula(dati$dot_s~dati$train_s|dati$qcorsi_s+dati$ncorsi_s+dati$doce_s+dati$spazi_s+dati$train_s+dati$collab_s+dati$incora_s)
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)     
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d+dati$incora_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d+dati$incora_d)

effe<-Formula(dati$dot_s~dati$train_s|dati$qcorsi_s+dati$ncorsi_s+dati$doce_s+dati$spazi_s+dati$train_s+dati$collab_s)
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d)     
effe<-Formula(dati$dot_s~dati$doce_d+dati$train_d+dati$collab_d|dati$qcorsi_d+dati$ncorsi_d+dati$doce_d+dati$spazi_d+dati$train_d+dati$collab_d)
dot_ex<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(dot_ex)
BIC(dot_ex)

lx<-logLik(dot_ex)
lx

e_incora= (lx - lm)/lm
e_incora#0.021 numerica
#0.0100 dicotom
# dico 6 0.0150

e=(-10497.22+10368.21)/-10368.21
e

##### POM DOTT
#W<-select(sud, qcorsi_d, ncorsi_d, doce_d, spazi_d, train_d, collab_d ,incora_d)
sodddot<-as.factor(dati$dot_s)
class(sodddot)
######## pom no dicotom
pom<-polr(sodddot~qcorsi_s+ncorsi_s+doce_s+spazi_s+train_s+collab_s+incora_s, method="logistic", data = dati)
summary(pom)
### pom dicotom a 7
pom<-polr(sodddot~qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d, method="logistic", data = dati)
summary(pom)
######## pom dicotom a 6
pom<-polr(sodddot~qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d, method="logistic", data = dati)
summary(pom)

lm<-logLik(pom)
lm

# train
dot_ex<-polr(sodddot~qcorsi_s+ncorsi_s+doce_s+spazi_s+train_s+collab_s+incora_s, method="logistic", data = dati)
dot_ex<-polr(sodddot~qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d, method="logistic", data = dati)

dot_ex<-polr(sodddot~qcorsi_s+ncorsi_s+doce_s+spazi_s+collab_s+incora_s, method="logistic", data = dati)
dot_ex<-polr(sodddot~qcorsi_d+ncorsi_d+doce_d+spazi_d+collab_d+incora_d, method="logistic", data = dati)
summary(dot_ex)
BIC(dot_ex)

lx<-logLik(dot_ex)
lx

e_train= (lx - lm)/lm
e_train#0.0159 numerica
#dicotom a 7 0.0020
#dico 6 0.0054

e=(-10497.22+10368.21)/-10368.21
e



# qcorsi
dot_ex<-polr(sodddot~qcorsi_s+ncorsi_s+doce_s+spazi_s+train_s+collab_s+incora_s, method="logistic", data = dati)
dot_ex<-polr(sodddot~qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d, method="logistic", data = dati)

dot_ex<-polr(sodddot~ncorsi_s+doce_s+spazi_s+train_s+collab_s+incora_s, method="logistic", data = dati)
dot_ex<-polr(sodddot~ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d, method="logistic", data = dati)
summary(dot_ex)
BIC(dot_ex)

lx<-logLik(dot_ex)
lx

e_qcorsi= (lx - lm)/lm
e_qcorsi#0.0102 numerica
#☻dico 7 0.0017
# dico 6 0.0051

e=(-10497.22+10368.21)/-10368.21
e


# ncorsi
dot_ex<-polr(sodddot~qcorsi_s+ncorsi_s+doce_s+spazi_s+train_s+collab_s+incora_s, method="logistic", data = dati)
dot_ex<-polr(sodddot~qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d, method="logistic", data = dati)

dot_ex<-polr(sodddot~qcorsi_s+doce_s+spazi_s+train_s+collab_s+incora_s, method="logistic", data = dati)
dot_ex<-polr(sodddot~qcorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d, method="logistic", data = dati)
summary(dot_ex)
BIC(dot_ex)

lx<-logLik(dot_ex)
lx

e_ncorsi= (lx - lm)/lm
e_ncorsi#0.0051
#dico 7 0.0007 
# dico 6 0.00248
e=(-10497.22+10368.21)/-10368.21
e


# doce
dot_ex<-polr(sodddot~qcorsi_s+ncorsi_s+doce_s+spazi_s+train_s+collab_s+incora_s, method="logistic", data = dati)
dot_ex<-polr(sodddot~qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d, method="logistic", data = dati)

dot_ex<-polr(sodddot~qcorsi_s+ncorsi_s+spazi_s+train_s+collab_s+incora_s, method="logistic", data = dati)
dot_ex<-polr(sodddot~qcorsi_d+ncorsi_d+spazi_d+train_d+collab_d+incora_d, method="logistic", data = dati)
summary(dot_ex)
BIC(dot_ex)

lx<-logLik(dot_ex)
lx

e_doce= (lx - lm)/lm
e_doce#0.0080
#dico 7 0.0081
# dico 6 0.0123

e=(-10497.22+10368.21)/-10368.21
e

# spazi
dot_ex<-polr(sodddot~qcorsi_s+ncorsi_s+doce_s+spazi_s+train_s+collab_s+incora_s, method="logistic", data = dati)
dot_ex<-polr(sodddot~qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d, method="logistic", data = dati)

dot_ex<-polr(sodddot~qcorsi_s+ncorsi_s+doce_s+train_s+collab_s+incora_s, method="logistic", data = dati)
dot_ex<-polr(sodddot~qcorsi_d+ncorsi_d+doce_d+train_d+collab_d+incora_d, method="logistic", data = dati)
summary(dot_ex)
BIC(dot_ex)

lx<-logLik(dot_ex)
lx

e_spazi= (lx - lm)/lm
e_spazi#0.0065 numerica
#dico 7 0.0021
# dico 6 0.0037

e=(-10497.22+10368.21)/-10368.21
e


# collab
dot_ex<-polr(sodddot~qcorsi_s+ncorsi_s+doce_s+spazi_s+train_s+collab_s+incora_s, method="logistic", data = dati)
dot_ex<-polr(sodddot~qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d, method="logistic", data = dati)

dot_ex<-polr(sodddot~qcorsi_s+ncorsi_s+doce_s+spazi_s+train_s+incora_s, method="logistic", data = dati)
dot_ex<-polr(sodddot~qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+incora_d, method="logistic", data = dati)
summary(dot_ex)
BIC(dot_ex)

lx<-logLik(dot_ex)
lx

e_collab= (lx - lm)/lm
e_collab#0.0095 numerica
#dico 7 0.0056
#dico 6 0.00943
e=(-10497.22+10368.21)/-10368.21
e


# incora
dot_ex<-polr(sodddot~qcorsi_s+ncorsi_s+doce_s+spazi_s+train_s+collab_s+incora_s, method="logistic", data = dati)
dot_ex<-polr(sodddot~qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d+incora_d, method="logistic", data = dati)

dot_ex<-polr(sodddot~qcorsi_s+ncorsi_s+doce_s+spazi_s+train_s+collab_s, method="logistic", data = dati)
dot_ex<-polr(sodddot~qcorsi_d+ncorsi_d+doce_d+spazi_d+train_d+collab_d, method="logistic", data = dati)
summary(dot_ex)
BIC(dot_ex)

lx<-logLik(dot_ex)
lx

e_incora= (lx - lm)/lm
e_incora#0.0335
#dico 7 0.0123
# dico 6 0.0196

e=(-10497.22+10368.21)/-10368.21
e

#capacità esplicat
#sodd
### covar NO DICOTOMIZZ
effe<-with(dati,Formula(sodd~att_s+redd_s
                        |att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s))
#att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s
#dicotomizza a 7          
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))

#dicotom a 6
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))


bestcub_sodd<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(bestcub_sodd)
BIC(bestcub_sodd)

lm<-logLik(bestcub_sodd)
lm
# valuto la capacità esplicativa delle covariate X
#di sodd intero campione
## NO DICOTOM 
# att
effe<-with(dati,Formula(sodd~att_s+redd_s
                        |att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))


effe<-with(dati,Formula(sodd~redd_s
                        |stab_s+auton_s+know_s+redd_s+carrie_s+prof_s))

effe<-with(dati,Formula(sodd~prof_d
                        |stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))
effe<-with(dati,Formula(sodd~prof_d
                        |stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))


sodd_ex<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(sodd_ex)
BIC(sodd_ex)

lx<-logLik(sodd_ex)
lx

e_att= (lx - lm)/lm
e_att#0.047 numer
# dico 7 0.0088
# dico 6 0.0162
e=(-10497.22+10368.21)/-10368.21
e

# redd
effe<-with(dati,Formula(sodd~att_s+redd_s
                        |att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))

effe<-with(dati,Formula(sodd~att_s
                        |att_s+stab_s+auton_s+know_s+carrie_s+prof_s))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+carrie_d+prof_d))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+carrie_d+prof_d))

sodd_ex<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(sodd_ex)
BIC(sodd_ex)

lx<-logLik(sodd_ex)
lx

e_red= (lx - lm)/lm
e_red #0.0168 numerica
# dico 7 0.0035
# dico 6 0.00591
e=(-10497.22+10368.21)/-10368.21
e
 

# stab
effe<-with(dati,Formula(sodd~att_s+redd_s
                        |att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))

effe<-with(dati,Formula(sodd~att_s+redd_s
                        |att_s+auton_s+know_s+redd_s+carrie_s+prof_s))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+auton_d+know_d+redd_d+carrie_d+prof_d))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+auton_d+know_d+redd_d+carrie_d+prof_d))

sodd_ex<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(sodd_ex)
BIC(sodd_ex)

lx<-logLik(sodd_ex)
lx

e_stab= (lx - lm)/lm
e_stab #0.0102 numerica
#dico 7 0.0047
# dico 6 0.0083

e=(-10497.22+10368.21)/-10368.21
e

# auton
effe<-with(dati,Formula(sodd~att_s+redd_s
                        |att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))

effe<-with(dati,Formula(sodd~att_s+redd_s
                        |att_s+stab_s+know_s+redd_s+carrie_s+prof_s))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+know_d+redd_d+carrie_d+prof_d))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+know_d+redd_d+carrie_d+prof_d))

sodd_ex<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(sodd_ex)
BIC(sodd_ex)

lx<-logLik(sodd_ex)
lx

e_aut= (lx - lm)/lm
e_aut #0.0024 numeric
# dico 7 0.0012
#dico 6 0.00506
e=(-10497.22+10368.21)/-10368.21
e

# know
effe<-with(dati,Formula(sodd~att_s+redd_s
                        |att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))

effe<-with(dati,Formula(sodd~att_s+redd_s
                        |att_s+stab_s+auton_s+redd_s+carrie_s+prof_s))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+redd_d+carrie_d+prof_d))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+redd_d+carrie_d+prof_d))

sodd_ex<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(sodd_ex)
BIC(sodd_ex)

lx<-logLik(sodd_ex)
lx

e_know= (lx - lm)/lm
e_know #0.0016
#dico 7 0.00051
#dico 6 0.0011

e=(-10497.22+10368.21)/-10368.21
e

# carrie
effe<-with(dati,Formula(sodd~att_s+redd_s
                        |att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))


effe<-with(dati,Formula(sodd~att_s+redd_s
                        |att_s+stab_s+auton_s+know_s+redd_s+prof_s))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+prof_d))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+prof_d))

sodd_ex<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(sodd_ex)
BIC(sodd_ex)

lx<-logLik(sodd_ex)
lx

e_car= (lx - lm)/lm
e_car #0.0064 numer
# dico 7 0.0039
# dico 6 0.00611

e=(-10497.22+10368.21)/-10368.21
e


# prof
effe<-with(dati,Formula(sodd~att_s+redd_s
                        |att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d))

effe<-with(dati,Formula(sodd~att_s+redd_s
                        |att_s+stab_s+auton_s+know_s+redd_s+carrie_s))
effe<-with(dati,Formula(sodd~att_d+prof_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d))
effe<-with(dati,Formula(sodd~att_d
                        |att_d+stab_d+auton_d+know_d+redd_d+carrie_d))

sodd_ex<-fastCUB(effe,data=dati,m=8, maxiter=100,toler=1e-8,mix=TRUE,verbose=FALSE)
summary(sodd_ex)
BIC(sodd_ex)

lx<-logLik(sodd_ex)
lx

e_prof= (lx - lm)/lm
e_prof #0.0097 num
# dico 7 0.00641
# dico 6 0.0137
e=(-10497.22+10368.21)/-10368.21
e

### POM SODD
#capacità esplicativa pom sodd lavoro intero campione

soddlav<-as.factor(dati$sodd)
class(soddlav)
### POM NO DICOTOMIZZ
pom<-polr(soddlav~ att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s, method="logistic", data = dati)
summary(pom)
o<-predict(pom, type="p")
plot(o)

pom %>%
  ggpredict(c("prof_s")) %>%
  plot()+
  scale_x_discrete(limits=1:8)+
  labs(title = "sodd_s",y="Probabilità")+
  theme_bw()


pom %>%
  ggpredict(c("redd_s")) %>%
  plot()+
  scale_x_discrete(limits=1:8)+
  labs(title = "sodd_s",y="Probabilità")+
  theme_bw()

pom %>%
  ggpredict(c("carrie_s")) %>%
  plot()+
  scale_x_discrete(limits=1:8)+
  labs(title = "sodd_s",y="Probabilità")+
  theme_bw()


predict(pom)
we<-predict(pom,type=sodd
newdat <- cbind(dati$sodd ,predict(pom, type = "probs"))

ggplot(we, aes(x = so, y = Probability, colour = Level)) +
  geom_line()
##show first few rows
head(newdat)
### POM dicotomizz a 7
pom<-polr(soddlav~ att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d, method="logistic", data = dati)
summary(pom)
############ pom dicotom a 6
pom<-polr(soddlav~ att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d, method="logistic", data = dati)
summary(pom)


lm<-logLik(pom)
lm

# att
sodd_ex<-polr(soddlav~ att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s, method="logistic", data = dati)
sodd_ex<-polr(soddlav~ att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d, method="logistic", data = dati)

sodd_ex<-polr(soddlav~ stab_s+auton_s+know_s+redd_s+carrie_s+prof_s, method="logistic", data = dati)
sodd_ex<-polr(soddlav~ stab_d+auton_d+know_d+redd_d+carrie_d+prof_d, method="logistic", data = dati)
summary(sodd_ex)
BIC(sodd_ex)


lx<-logLik(sodd_ex)
lx

e_att= (lx - lm)/lm
e_att#0.089 num
#dico 7 0.0121
#dico 6 0.0258
e=(-10497.22+10368.21)/-10368.21
e

# stab
sodd_ex<-polr(soddlav~ att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s, method="logistic", data = dati)
sodd_ex<-polr(soddlav~ att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d, method="logistic", data = dati)

sodd_ex<-polr(soddlav~ att_s+auton_s+know_s+redd_s+carrie_s+prof_s, method="logistic", data = dati)
sodd_ex<-polr(soddlav~ att_d+auton_d+know_d+redd_d+carrie_d+prof_d, method="logistic", data = dati)
summary(sodd_ex)
BIC(sodd_ex)


lx<-logLik(sodd_ex)
lx

e_stab= (lx - lm)/lm
e_stab#0.0249 num
# dico 7 0.0064
# dico 6 0.0111
e=(-10497.22+10368.21)/-10368.21
e

# auton
sodd_ex<-polr(soddlav~ att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s, method="logistic", data = dati)
sodd_ex<-polr(soddlav~ att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d, method="logistic", data = dati)

sodd_ex<-polr(soddlav~ att_s+stab_s+know_s+redd_s+carrie_s+prof_s, method="logistic", data = dati)
sodd_ex<-polr(soddlav~ att_d+stab_d+know_d+redd_d+carrie_d+prof_d, method="logistic", data = dati)
summary(sodd_ex)
BIC(sodd_ex)


lx<-logLik(sodd_ex)
lx

e_aut= (lx - lm)/lm
e_aut#0.0056 num
#dico 7 0.0018
# dico 6 0.0078
e=(-10497.22+10368.21)/-10368.21
e

# know
sodd_ex<-polr(soddlav~ att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s, method="logistic", data = dati)
sodd_ex<-polr(soddlav~ att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d, method="logistic", data = dati)

sodd_ex<-polr(soddlav~ att_s+stab_s+auton_s+redd_s+carrie_s+prof_s, method="logistic", data = dati)
sodd_ex<-polr(soddlav~ att_d+stab_d+auton_d+redd_d+carrie_d+prof_d, method="logistic", data = dati)
summary(sodd_ex)
BIC(sodd_ex)


lx<-logLik(sodd_ex)
lx

e_kn= (lx - lm)/lm
e_kn#0.0037 num
# dico 7 0.0006
# dico 6 0.0017
e=(-10497.22+10368.21)/-10368.21
e


# redd
sodd_ex<-polr(soddlav~ att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s, method="logistic", data = dati)
sodd_ex<-polr(soddlav~ att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d, method="logistic", data = dati)

sodd_ex<-polr(soddlav~ att_s+stab_s+auton_s+know_s+carrie_s+prof_s, method="logistic", data = dati)
sodd_ex<-polr(soddlav~ att_d+stab_d+auton_d+know_d+carrie_d+prof_d, method="logistic", data = dati)
summary(sodd_ex)
BIC(sodd_ex)


lx<-logLik(sodd_ex)
lx

e_red= (lx - lm)/lm
e_red#0.00321 num
# dico 7 0.0041
# dico 6 0.0080 
e=(-10497.22+10368.21)/-10368.21
e


# carrie
sodd_ex<-polr(soddlav~ att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s, method="logistic", data = dati)
sodd_ex<-polr(soddlav~ att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d, method="logistic", data = dati)

sodd_ex<-polr(soddlav~ att_s+stab_s+auton_s+know_s+redd_s+prof_s, method="logistic", data = dati)
sodd_ex<-polr(soddlav~ att_d+stab_d+auton_d+know_d+redd_d+prof_d, method="logistic", data = dati)
summary(sodd_ex)
BIC(sodd_ex)

lx<-logLik(sodd_ex)
lx

e_car= (lx - lm)/lm
e_car#0.0155 num
# dico 7 0.00449
# dico 6 0.0089
e=(-10497.22+10368.21)/-10368.21
e


# prof
sodd_ex<-polr(soddlav~ att_s+stab_s+auton_s+know_s+redd_s+carrie_s+prof_s, method="logistic", data = dati)
sodd_ex<-polr(soddlav~ att_d+stab_d+auton_d+know_d+redd_d+carrie_d+prof_d, method="logistic", data = dati)

sodd_ex<-polr(soddlav~ att_s+stab_s+auton_s+know_s+redd_s+carrie_s, method="logistic", data = dati)
sodd_ex<-polr(soddlav~ att_d+stab_d+auton_d+know_d+redd_d+carrie_d, method="logistic", data = dati)
summary(sodd_ex)
BIC(sodd_ex)


lx<-logLik(sodd_ex)
lx

e_prof= (lx - lm)/lm
e_prof#0.0213 num
#dico 7 0.0101
#dico 6 0.0187
e=(-10497.22+10368.21)/-10368.21
e