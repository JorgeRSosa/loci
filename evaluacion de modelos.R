##########################Entrenamiento loci-bootstrap###########\
confusion
#################tasa de acierto en positivos, sensibilidad###############
tpr<-confusion[1,1]/(confusion[1,1]+confusion[1,2])
tpr
#################tasa de acierto en negativos, especifidad###############
tnr<-confusion[2,2]/(confusion[2,2]+confusion[2,1])
tnr
###############precision global o tasa de aciertos, accuracy###############
#se usa en caso que las clases no se encuentren balanceadas
acc<-(confusion[1,1]+confusion[2,2])/sum(confusion)
acc
##########################presicion balanceada#######################
ba<-(tpr+tnr)/2
ba
######################### entrenamiento Loci##############################
confusion2
#################tasa de acierto en positivos, sensibilidad###############
tpr2<-confusion2[1,1]/(confusion2[1,1]+confusion2[1,2])
tpr2
#################tasa de acierto en negativos, especifidad###############
tnr2<-confusion2[2,2]/(confusion2[2,2]+confusion2[2,1])
tnr2
###############precision global o tasa de aciertos, accuracy###############
#se usa en caso que las clases no se encuentren balanceadas
acc2<-(confusion2[1,1]+confusion2[2,2])/sum(confusion2)
acc2
##########################presicion balanceada#######################
ba2<-(tpr2+tnr2)/2
ba2
############################Testeo loci-bootstrap#############
confusion3
#################tasa de acierto en positivos, sensibilidad###############
tpr3<-confusion3[1,1]/(confusion3[1,1]+confusion3[1,2])
tpr3
#################tasa de acierto en negativos, especifidad###############
tnr3<-confusion3[2,2]/(confusion3[2,2]+confusion3[2,1])
tnr3
###############precision global o tasa de aciertos, accuracy###############
#se usa en caso que las clases no se encuentren balanceadas
acc3<-(confusion3[1,1]+confusion3[2,2])/sum(confusion3)
acc3
##########################presicion balanceada#######################
ba3<-(tpr3+tnr3)/2
ba3
