library(DDoutlier)
library(tidyverse)
load("datosentrenamiento.RData")
select<-dplyr::select
train_list_num<-lapply(train_list,select,-x_u_feff_date,-time)
# scores<-list()
# for(i in 1:length(train_list_num)){
#   print(i)
#   scores[[i]]<-LOCI(train_list_num[[i]],0.75,3217)
#   save(scores,file="./tesis/resultados.RData")
# }
##########################obtengo la cantidad de inliers###########################################
load("resultados.RData")
cres<-lapply(scores, function(x)x[["class"]])
clasres<-unlist(cres)
length(clasres)-sum(clasres=="Inlier")
############################calculo el score############################
lsce<-lapply(scores, function(x)x[["MDEF"]]/x[["norm_MDEF"]])
sce<-unlist(lsce)
sum(sce>3)
hist(sce,main = "")
################################################################
dataloci<-cbind(dttrain,score$r_3217)
# save(dataloci,file="resultado/lociscore.RData")
########################Evaluamos el entrenamiento##############################
quantile(sce, c(.50, .75, .90,.99)) 
q_99<-cbind(dataloci,limite=quantile(sce, c(.99)) )
#########################################
names(q_99)<-c("fecha","time","score","limite")
######################################################
q_99<-q_99 %>% mutate(clase=ifelse(score>limite,1,0))
#######################################################
anom_q99<-q_99[q_99$clase==1,] %>% select(fecha,clase) %>% unique()
#######################################################
q_99<-q_99 %>% mutate(clase_res=as.factor(ifelse(fecha%in%anom_q99$fecha,1,0)))
sum(as.numeric(q_99$clase_res))
########################################################
library(ggplot2)
q_99$fecha<-as.factor(q_99$fecha)
##############grafica de evolucion en el tiempo
ggplot(q_99) +geom_line(aes(x=time,y=limite))+
  geom_line(aes(x=time,y=score,color=clase_res))+
  scale_color_manual(values=c("#27AE60", "#FF1800"))+facet_wrap(~fecha,ncol=20)
#################################
anom_q99<-anom_q99 %>% mutate(fecha=as.Date(as.character(fecha)))
mod<-mod %>% mutate(marca_2=ifelse(as.Date(x_u_feff_date)%in%anom_q99$fecha,1,0))
sum(mod$marca_2)
confusion<-table(mod$marca,mod$marca_2)
confusion

#Elemento de grafica para calcular el área bajo la curva AUC
library(plotROC)
p1 <- ggplot(mod, aes(d=marca,m=marca_2)) + geom_roc()+ style_roc() #Grafica Curva ROC basica para colocar el area bajo la curva
#Curva ROC
logroc<-ggplot(mod, aes(d=marca,m=marca_2)) +
  theme_bw()+
  geom_roc(n.cuts = 0, colour="#3AA717") + 
  theme(axis.text = element_text(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Curva roc LOCI límite 1.5")+
  scale_x_continuous("\n1 - Especifidad (FPF)", breaks = seq(0, 1, by = .2))+
  scale_y_continuous("Sensibilidad (TPF)\n", breaks = seq(0, 1, by = .2)) +
  geom_abline(intercept=0, slope=1, colour="blue", linetype="dashed") +
  annotate("text", x=0.6, y=0.45, parse=TRUE,
           label=paste0("AUC: ",round(calc_auc(p1)$AUC,3)), colour="blue")+
  ggExtra::removeGridX()+
  ggExtra::removeGridY()

logroc
###########################PRIMER LIMITE PAPER###################
q_99<-cbind(dataloci,limite=1.5)
#########################################
names(q_99)<-c("fecha","time","score","limite")
######################################################
q_99<-q_99 %>% mutate(clase=ifelse(score>limite,1,0))
#######################################################
anom_q99<-q_99[q_99$clase==1,] %>% select(fecha,clase) %>% unique()
#######################################################
q_99<-q_99 %>% mutate(clase_res=as.factor(ifelse(fecha%in%anom_q99$fecha,1,0)))
sum(as.numeric(q_99$clase_res))
########################################################
library(ggplot2)
q_99$fecha<-as.factor(q_99$fecha)
# ggplot(q_99) +geom_line(aes(x=time,y=limite))+
#   geom_line(aes(x=time,y=score,color=clase_res))+
#   scale_color_manual(values=c("#27AE60", "#FF1800"))+facet_wrap(~fecha,ncol=20)
#################################
anom_q99<-anom_q99 %>% mutate(fecha=as.Date(as.character(fecha)))
mod<-mod %>% mutate(marca_2=ifelse(as.Date(x_u_feff_date)%in%anom_q99$fecha,1,0))
sum(mod$marca_2)
confusion2<-table(mod$marca,mod$marca_2)
confusion2

#Elemento de grafica para calcular el área bajo la curva AUC
library(plotROC)
p1 <- ggplot(mod, aes(d=marca,m=marca_2)) + geom_roc()+ style_roc() #Grafica Curva ROC basica para colocar el area bajo la curva
#Curva ROC
logroc2<-ggplot(mod, aes(d=marca,m=marca_2)) +
  theme_bw()+
  geom_roc(n.cuts = 0, colour="#3AA717") + 
  theme(axis.text = element_text(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Curva roc LOCI límite 1.5")+
  scale_x_continuous("\n1 - Especifidad (FPF)", breaks = seq(0, 1, by = .2))+
  scale_y_continuous("Sensibilidad (TPF)\n", breaks = seq(0, 1, by = .2)) +
  geom_abline(intercept=0, slope=1, colour="blue", linetype="dashed") +
  annotate("text", x=0.6, y=0.45, parse=TRUE,
           label=paste0("AUC: ",round(calc_auc(p1)$AUC,3)), colour="blue")+
  ggExtra::removeGridX()+
  ggExtra::removeGridY()

logroc2

##############################################
############################Segundo limite del paper 2.5#########################
q_99<-cbind(dataloci,limite=2.5)
#########################################
names(q_99)<-c("fecha","time","score","limite")
######################################################
q_99<-q_99 %>% mutate(clase=ifelse(score>limite,1,0))
#######################################################
anom_q99<-q_99[q_99$clase==1,] %>% select(fecha,clase) %>% unique()
#######################################################
q_99<-q_99 %>% mutate(clase_res=as.factor(ifelse(fecha%in%anom_q99$fecha,1,0)))
sum(as.numeric(q_99$clase_res))
########################################################
library(ggplot2)
q_99$fecha<-as.factor(q_99$fecha)
# ggplot(q_99) +geom_line(aes(x=time,y=limite))+
#   geom_line(aes(x=time,y=score,color=clase_res))+
#   scale_color_manual(values=c("#27AE60", "#FF1800"))+facet_wrap(~fecha,ncol=20)
#################################
anom_q99<-anom_q99 %>% mutate(fecha=as.Date(as.character(fecha)))
mod<-mod %>% mutate(marca_2=ifelse(as.Date(x_u_feff_date)%in%anom_q99$fecha,1,0))
sum(mod$marca_2)
confusion3<-table(mod$marca,mod$marca_2)
confusion3

#Elemento de grafica para calcular el área bajo la curva AUC
library(plotROC)
p1 <- ggplot(mod, aes(d=marca,m=marca_2)) + geom_roc()+ style_roc() #Grafica Curva ROC basica para colocar el area bajo la curva
#Curva ROC
logroc3<-ggplot(mod, aes(d=marca,m=marca_2)) +
  theme_bw()+
  geom_roc(n.cuts = 0, colour="#3AA717") + 
  theme(axis.text = element_text(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Curva roc LOCI límite 2.5")+
  scale_x_continuous("\n1 - Especifidad (FPF)", breaks = seq(0, 1, by = .2))+
  scale_y_continuous("Sensibilidad (TPF)\n", breaks = seq(0, 1, by = .2)) +
  geom_abline(intercept=0, slope=1, colour="blue", linetype="dashed") +
  annotate("text", x=0.6, y=0.45, parse=TRUE,
           label=paste0("AUC: ",round(calc_auc(p1)$AUC,3)), colour="blue")+
  ggExtra::removeGridX()+
  ggExtra::removeGridY()

logroc3

library(ROCit)
ROC <- rocit(score=mod$marca_2, class=mod$marca)
# plot(ROC)
# ksplot(ROC)
##############################Loci entrenamiento################################33
names(mod2$`3217`)
str(mod2$`3217`)
confusion2<-table(mod2$`3217`$marca,mod2$`3217`$evaluacion)
confusion2

##########################Curva roc##########################
#Elemento de grafica para calcular el área bajo la curva AUC
p2<- ggplot(mod2$`3217`, aes(d=marca,m=evaluacion)) + geom_roc()+ style_roc() #Grafica Curva ROC basica para colocar el area bajo la curva
#Curva ROC
logroc2<-ggplot(mod2$`3217`, aes(d=marca,m=evaluacion)) +
  theme_bw()+
  geom_roc(n.cuts = 0, colour="#3AA717") + 
  theme(axis.text = element_text(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Curva roc LOCI tradicional")+
  scale_x_continuous("\n1 - Especifidad (FPF)", breaks = seq(0, 1, by = .2))+
  scale_y_continuous("Sensibilidad (TPF)\n", breaks = seq(0, 1, by = .2)) +
  geom_abline(intercept=0, slope=1, colour="blue", linetype="dashed") +
  annotate("text", x=0.6, y=0.45, parse=TRUE,
           label=paste0("AUC: ",round(calc_auc(p2)$AUC,3)), colour="blue")+
  ggExtra::removeGridX()+
  ggExtra::removeGridY()

logroc2
######################################testeo con loci bootstrap##############################
load("testeo.RData")
sc_test<-restest$MDEF/restest$norm_MDEF
dat_test<-data.frame(fecha=test$x_u_feff_date,time=test$time,score=sc_test,limite=quantile(sce, c(.99)))

dat_test<-dat_test %>% mutate(evaluacion=ifelse(score>limite,1,0))
#############################################################################
anom_test<-dat_test[dat_test$evaluacion==1,] %>% select(fecha,evaluacion) %>% unique()
#######################################################
dat_test<-dat_test %>% mutate(clase_res=as.factor(ifelse(fecha%in%anom_test$fecha,1,0)))
dat_test<-merge(dat_test,val,by.x="fecha",by.y="x_u_feff_date")
dat_test<-dat_test %>% mutate(tet=ifelse(marca==clase_res,"Bien etiquetado","Mal etiquetado"))
unique(dat_test %>% select(fecha,marca))
###########################Grafico testeo##################################
dat_test$fecha<-as.factor(dat_test$fecha)

dat_test$tipo_etiquetado<-as.factor(dat_test$tet)
#######################grafica de evolucion en el tiempo
ggplot(dat_test) +geom_line(aes(x=time,y=limite))+
  geom_line(aes(x=time,y=score,color=clase_res, linetype = tipo_etiquetado))+
  scale_color_manual(values=c("#27AE60", "#FF1800"))+facet_wrap(~fecha,ncol=20)
####################construccion de la matriz de confusion
val<-val %>% mutate(marca_2=ifelse(x_u_feff_date%in%anom_test$fecha,1,0))
confusion<-table(val$marca,val$marca_2)
confusion
############################################################################
p3<- ggplot(val, aes(d=marca,m=marca_2)) + geom_roc()+ style_roc() #Grafica Curva ROC basica para colocar el area bajo la curva
#Curva ROC
logroc<-ggplot(val, aes(d=marca,m=marca_2)) +
  theme_bw()+
  geom_roc(n.cuts = 0, colour="#3AA717") + 
  theme(axis.text = element_text(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Curva roc LOCI limite 2.5")+
  scale_x_continuous("\n1 - Especifidad (FPF)", breaks = seq(0, 1, by = .2))+
  scale_y_continuous("Sensibilidad (TPF)\n", breaks = seq(0, 1, by = .2)) +
  geom_abline(intercept=0, slope=1, colour="blue", linetype="dashed") +
  annotate("text", x=0.6, y=0.45, parse=TRUE,
           label=paste0("AUC: ",round(calc_auc(p3)$AUC,3)), colour="blue")+
  ggExtra::removeGridX()+
  ggExtra::removeGridY()

logroc
############################LOCI limite 1.5#############################
######################################testeo con loci bootstrap##############################
sc_test<-restest$MDEF/restest$norm_MDEF
dat_test<-data.frame(fecha=test$x_u_feff_date,time=test$time,score=sc_test,limite=1.5)

dat_test<-dat_test %>% mutate(evaluacion=ifelse(score>limite,1,0))
#############################################################################
anom_test<-dat_test[dat_test$evaluacion==1,] %>% select(fecha,evaluacion) %>% unique()
#######################################################
dat_test<-dat_test %>% mutate(clase_res=as.factor(ifelse(fecha%in%anom_test$fecha,1,0)))
val<-val %>% mutate(tet=ifelse(marca==marca_2,"Bien etiquetado","Mal etiquetado"))
dat_test<-merge(dat_test,val,by.x="fecha",by.y="x_u_feff_date")
unique(dat_test %>% select(fecha,marca))
###########################Grafico testeo##################################
dat_test$fecha<-as.factor(dat_test$fecha)
dat_test$tipo_etiquetado<-as.factor(dat_test$tet)
###########################grafica de evolucion en el tiempo
# ggplot(dat_test) +geom_line(aes(x=time,y=limite))+
#   geom_line(aes(x=time,y=score,color=clase_res, linetype = tipo_etiquetado))+
#   scale_color_manual(values=c("#27AE60", "#FF1800"))+facet_wrap(~fecha,ncol=20)
#####################contruccion de la matriz de confusion
val<-val %>% mutate(marca_2=ifelse(x_u_feff_date%in%anom_test$fecha,1,0))
confusion2<-table(val$marca,val$marca_2)
confusion2
############################################################################
p4<- ggplot(val, aes(d=marca,m=marca_2)) + geom_roc()+ style_roc() #Grafica Curva ROC basica para colocar el area bajo la curva
#Curva ROC
logroc2<-ggplot(val, aes(d=marca,m=marca_2)) +
  theme_bw()+
  geom_roc(n.cuts = 0, colour="#3AA717") + 
  theme(axis.text = element_text(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Curva roc LOCI limite 2.5")+
  scale_x_continuous("\n1 - Especifidad (FPF)", breaks = seq(0, 1, by = .2))+
  scale_y_continuous("Sensibilidad (TPF)\n", breaks = seq(0, 1, by = .2)) +
  geom_abline(intercept=0, slope=1, colour="blue", linetype="dashed") +
  annotate("text", x=0.6, y=0.45, parse=TRUE,
           label=paste0("AUC: ",round(calc_auc(p4)$AUC,3)), colour="blue")+
  ggExtra::removeGridX()+
  ggExtra::removeGridY()

logroc2
###############################################################
###################################LOCI limite 2.5############################
######################################testeo con loci bootstrap##############################
sc_test<-restest$MDEF/restest$norm_MDEF
dat_test<-data.frame(fecha=test$x_u_feff_date,time=test$time,score=sc_test,limite=2.5)
#dat_test<-data.frame(fecha=test$x_u_feff_date,time=test$time,score=sc_test,limite=2.5)

dat_test<-dat_test %>% mutate(evaluacion=ifelse(score>limite,1,0))
#############################################################################
anom_test<-dat_test[dat_test$evaluacion==1,] %>% select(fecha,evaluacion) %>% unique()
#######################################################
dat_test<-dat_test %>% mutate(clase_res=as.factor(ifelse(fecha%in%anom_test$fecha,1,0)))
val<-val %>% mutate(tet=ifelse(marca==marca_2,"Bien etiquetado","Mal etiquetado"))
dat_test<-merge(dat_test,val,by.x="fecha",by.y="x_u_feff_date")
unique(dat_test %>% select(fecha,marca))
###########################Grafico testeo##################################
dat_test$fecha<-as.factor(dat_test$fecha)
dat_test$tipo_etiquetado<-as.factor(dat_test$tet)
###################gráfica de evolicion en el tiempo#############################
# ggplot(dat_test) +geom_line(aes(x=time,y=limite))+
#   geom_line(aes(x=time,y=score,color=clase_res, linetype = tipo_etiquetado))+
#   scale_color_manual(values=c("#27AE60", "#FF1800"))+facet_wrap(~fecha,ncol=20)
##############construccion matriz de confusion##############################
val<-val %>% mutate(marca_2=ifelse(x_u_feff_date%in%anom_test$fecha,1,0))
confusion3<-table(val$marca,val$marca_2)
confusion3
############################################################################
p5<- ggplot(val, aes(d=marca,m=marca_2)) + geom_roc()+ style_roc() #Grafica Curva ROC basica para colocar el area bajo la curva
#Curva ROC
logroc3<-ggplot(val, aes(d=marca,m=marca_2)) +
  theme_bw()+
  geom_roc(n.cuts = 0, colour="#3AA717") + 
  theme(axis.text = element_text(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Curva roc LOCI limite 2.5")+
  scale_x_continuous("\n1 - Especifidad (FPF)", breaks = seq(0, 1, by = .2))+
  scale_y_continuous("Sensibilidad (TPF)\n", breaks = seq(0, 1, by = .2)) +
  geom_abline(intercept=0, slope=1, colour="blue", linetype="dashed") +
  annotate("text", x=0.6, y=0.45, parse=TRUE,
           label=paste0("AUC: ",round(calc_auc(p5)$AUC,3)), colour="blue")+
  ggExtra::removeGridX()+
  ggExtra::removeGridY()

logroc3
######################################testeo loci#############################################
# dat_test_loci<-data.frame(fecha=test$x_u_feff_date,time=test$time,score=sc_test)
# dat_test_loci<-dat_test_loci %>% mutate(eval=ifelse(score>3,1,0))
# ###################################################################################
# anom_test_loci<-dat_test_loci[dat_test_loci$eval==1,] %>% select(fecha,eval) %>% unique()
# ################################################################################
# #######################################################
# dat_test_loci<-dat_test_loci%>% mutate(clase_res=as.factor(ifelse(fecha%in%anom_test_loci$fecha,1,0)))
# ###########################Grafico testeo##################################
# dat_test_loci$fecha<-as.factor(dat_test_loci$fecha)
# ggplot(dat_test_loci) +geom_line(aes(x=time,y=3))+
#   geom_line(aes(x=time,y=score,color=clase_res))+
#   scale_color_manual(values=c("#27AE60", "#FF1800"))+facet_wrap(~fecha,ncol=20)
# val2<-val %>% mutate(marca_2=ifelse(x_u_feff_date%in%anom_test_loci$fecha,1,0))
# sum(val2$marca_2)
# confusion4<-table(val2$marca,val2$marca_2)
# confusion4
# ###########################exactitud#################################
# (confusion4[1,1]+confusion4[2,2])/sum(confusion4)
# ############################Presicion
# (confusion4[1,1] /(confusion4[1,1] +confusion4[1,2]))
# ###############################################################################
# p4<- ggplot(val2, aes(d=marca,m=marca_2)) + geom_roc()+ style_roc() #Grafica Curva ROC basica para colocar el area bajo la curva
# #Curva ROC
# logroc4<-ggplot(val2, aes(d=marca,m=marca_2)) +
#   theme_bw()+
#   geom_roc(n.cuts = 0, colour="#3AA717") + 
#   theme(axis.text = element_text(colour = "black"),
#         plot.title = element_text(hjust = 0.5))+
#   ggtitle("Curva roc metodo LOCI")+
#   scale_x_continuous("\n1 - Especifidad (FPF)", breaks = seq(0, 1, by = .2))+
#   scale_y_continuous("Sensibilidad (TPF)\n", breaks = seq(0, 1, by = .2)) +
#   geom_abline(intercept=0, slope=1, colour="blue", linetype="dashed") +
#   annotate("text", x=0.6, y=0.45, parse=TRUE,
#            label=paste0("AUC: ",round(calc_auc(p4)$AUC,3)), colour="blue")+
#   ggExtra::removeGridX()+
#   ggExtra::removeGridY()
# 
# logroc4
