mconfusion <-rbind(data.frame(data.frame(confusion),tipo="Bootstrap LOCI"),
                   data.frame(data.frame(confusion2),tipo="LOCI Lim=1.5"),
                   data.frame(data.frame(confusion3),tipo="LOCI Lim=2.5"))
names(mconfusion)<-c("predicho","real","valores","tipo")
mconfusion$real<-factor(mconfusion$real,levels = c(0,1))
mconfusion$tipo<-as.factor(mconfusion$tipo)
mconfusion$predicho<-factor(mconfusion$predicho,levels = c(1,0))
ggplot(data = mconfusion,
       mapping = aes(x = real,
                     y = predicho)) +
  geom_tile(aes(fill = valores)) +
  geom_text(aes(label = sprintf("%1.0f", valores)), vjust = 1) +
  scale_fill_gradient(low = "#1E8449",
                      high = "#FDFEFE")+theme_bw()+theme(legend.position = "none")+
  facet_wrap(~tipo,ncol=3)


       