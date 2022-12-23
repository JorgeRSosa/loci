load("DatosCOmpletos.RData")
library(readxl)
data_tesis <- read_excel("data tesis.xlsx",  sheet = "HORAS INPUTADO")
library(tsibble)
library(tsibbledata)
library(lubridate)
library(tidyverse)
library(viridis)
library(sugrrants)
library(janitor)
library(DDoutlier)
####################grafico de la evolucion de la potencia en el tiempo#################
data_tesis<-data_tesis %>% clean_names() %>% mutate(x_u_feff_date=date) %>% select(-date)
Chiller <- data_tesis %>% select(x_u_feff_date,time,analizador_chiller_potencia_activa_kw) %>%
  mutate(Fecha=as.Date(x_u_feff_date)) %>% mutate(mes=month(Fecha)) %>% filter(mes%in%c(9,10,11))
str(Chiller)
class(Chiller$x_u_feff_date)
Chiller_fc<- Chiller %>% frame_calendar(x = time, y = vars(analizador_chiller_potencia_activa_kw), date =Fecha, calendar = "monthly")
str(Chiller_fc)
p1 <- Chiller_fc%>%  ggplot(aes(x = .time, y = .analizador_chiller_potencia_activa_kw, group = Fecha)) +  geom_line()

prettify(p1)

###########################################PARTICIONAR LA DATA########################################################
#0<-inlier
#1<-outlier
##################Quito la ultima fecha pues tiene datos incompletos######################
data<-data_tesis %>% filter(x_u_feff_date!=max(data_tesis$x_u_feff_date))
###################################Selecciono unicamente las fechas##################
fechas_et<- read_excel("data tesis.xlsx",  sheet = "FECHAS_ETIQUETA")
fechas_et<-fechas_et %>% clean_names() %>% mutate(x_u_feff_date=date) %>% select(-date)
fechas_et<-fechas_et %>% filter(x_u_feff_date!=max(fechas_et$x_u_feff_date)) %>% select(-observacion)
#######################################
# Marca info modelamiento / validación (50% / 50%)

library(caTools)
library(ROSE)
library(data.table)
set.seed(12345)
#cargar los datos funcionales guardados en el archivo presentacion de datos.R
#suponer que nuestro fdata se llama dataf

muestra <- sample.split(setDF(fechas_et),SplitRatio=0.5)
setDT(fechas_et)[,.N,by=marca]
mod <- setDT(subset(setDF(fechas_et), muestra == TRUE))
setDT(mod)[,.N,by=marca]
val <- setDT(subset(setDF(fechas_et), muestra == FALSE))
setDT(val)[,.N,by=marca]
#######################################################################################################
test<-data %>% filter(x_u_feff_date%in%val$x_u_feff_date) 
test<-merge(test,val,by="x_u_feff_date")

################################################trabajando con el train#######################################################
###########################Establecer Alpha###########################
alpha<-seq(0.1,1,0.05)
##############Aplico el algoritmo loci para saber con que alpha trabajar###############################33
dtnumeric<-data %>% filter(x_u_feff_date%in%mod$x_u_feff_date)   %>% select(-x_u_feff_date,-time)
#clase<-c()
# for(k in 1:length(alpha)){
#   print(k)
#   clase<-cbind(clase,LOCI(dtnumeric,alpha[k])$class)
# }
# colnames(clase)<-paste0("alpha ",alpha)
# clase<-data.frame(clase)

#sum(LOCI(dtnumeric,0.75,1000)$class=="Outlier")
#save(clase,file="clase.RData")
load("clase.RData")
apply(clase,2,function(x)sum(x=="Outlier"))

a <- LOCI(dtnumeric,0.75)
################################establecemos la cantidad de radios########################
#el numero de vecinos de cada vecindad
radios<-round(seq(20,nrow(dtnumeric),length.out = 100),0)
score<- c()
# for (i in 1:length(radios) ){
#   print(i)
#   a<- LOCI(dtnumeric,0.75,radios[i])
#   score<-cbind(score,a$MDEF/a$norm_MDEF)
# }
#score<-data.frame(score)
#names(score)<-paste0("r_",radios)
##########################################Selecciono el maximo valor del mdef##############
#save(score,file="matriz_score_train.Rdata")
load("matriz_score_train.Rdata")
############################################Selecciono el radio con mejor desempeño#############
clase<-ifelse(score[,1]>3,1,0)
for (i in 2:ncol(score)) {
  clase<-cbind(clase,ifelse(score[,i]>3,1,0))
}
clase<-data.frame(clase)
names(clase)<-paste0('r_',radios)
#################################################################
dttrain<-data %>% filter(x_u_feff_date%in%mod$x_u_feff_date)%>% select(x_u_feff_date,time)
clase<-cbind(dttrain,clase)
####################################################################
anomalias<-list()
library(tidyverse)
for(i in 3:ncol(clase)){
  anomalias[[i-2]]<-clase[,c(1,i)] [clase[,c(i)]==1,] %>% unique()
}
##################################################################
mod2<-list()
for(i in 1:length(anomalias)){
  mod2[[i]]<- mod %>% mutate(evaluacion=ifelse(x_u_feff_date%in%anomalias[[i]]$x_u_feff_date,1,0))
}
names(mod2)<-radios
# save.image(file="DatosCOmpletos.RData")
#load("DatosCOmpletos.RData")
############################################################
#mc <- table(res, val$var_dep)

mc<-lapply(mod2, function(x)table(x$evaluacion,x$marca))
mc[[1]][1,1] # Verdaderos positivos
mc[[1]][2,2] # Verdaderos negativos
mc[[1]][1,2] # Falsos positivos
mc[[1]][2,1] # Falsos negativos
###########################exactitud#################################
mc$`3217`
(mc$`3217`[1,1]+mc$`3217`[2,2])/sum(mc$`3217`)
############################Presicion
(mc$`3217`[1,1] /(mc$`3217`[1,1] +mc$`3217`[1,2]))
######################################exactitud en general################################
acc<-lapply(mc,function(x)(x[1,1]+x[2,2])/sum(x))
pres<-lapply(mc, function(x)(x[1,1] /(x[1,1] +x[1,2])))
acc<-unlist(acc)
pres<-unlist(pres)
which(acc%in%max(acc))
which(pres%in%max(pres))
mc[[62]]
mc[[63]]
acc[62]
acc[63]
pres[62]
pres[63]
radios[which(acc%in%max(acc))]
mc
###############################################escojemos el radio más pequeño y que mejor clasifica a los datos######
radel<-radios[which(acc%in%max(acc))][1]
#################################Eliminamos los outliers###################################
inlier<-mod%>% filter(marca==0)
#########################data sin outliers###################################3
datatrain<-data%>% filter(x_u_feff_date%in%inlier$x_u_feff_date) 
###################################generamos los datos de entrenamiento bootstrap#################################################################
# train_list<-list()
# for(i in 1:500){
#   mf<-inlier[sample(1:nrow(inlier),replace = T),]
#   train<-c()
#   for(j in 1:nrow(mf)){
#     train<-rbind(train,datatrain %>% filter(x_u_feff_date%in%mf$x_u_feff_date[j]))
#   }
#   
#   train_list[[i]]<-train
# }
save(train_list,file="datosentrenamiento.RData",version = 2)
load("datosentrenamiento.RData")
#####obtengo unicamente las filas numericas de mi train list
train_list_num<-lapply(train_list,select,-x_u_feff_date,-time)
# lloci<-lapply(train_list_num,LOCI,alpha=0.75,nn=radel,k=3)
# library(DDoutlier)
# system.time(LOCI(train_list_num[[1]],0.75,radel))
