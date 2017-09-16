library("tseries")
unloadNamespace("TimeSeriesStuffforClaire")
library("TimeSeriesStuffforClaire")
data("signals")
library(ggplot2)
library(reshape2)

table(signals$larve,signals$run)

plyr::ddply(signals[signals$run==1,],
            ~larve,
            function(dd){sum(dd$light1)})

signals<-signals[is.element(signals$larve,c(9:16)),]
signals$csplus1<-signals$csplus1*(1-signals$light1)
signals$csplus2<-signals$csplus2*(1-signals$light2)
exclude=(signals$test!=2)|(signals$run==1&signals$larve==2)|(signals$run==4&signals$larve==11)
Order<-c(0,2,1)
coeffsy1<-plyr::ddply(signals,
                    ~larve+run,
                    function(dd){
  #Y=irts(time=dd$time,value=dd$y1)
  as.data.frame(as.list(
    try(arima(x = dd$y1,order = Order,xreg = as.matrix(dd[-c(1:6,11:12)]))$coef)))
})
coeffsy2<-plyr::ddply(signals,
                      ~larve+run,
                      function(dd){
                        #Y=irts(time=dd$time,value=dd$y1)
                        as.data.frame(as.list(
                          try(arima(x = dd$y2,order = Order,xreg = as.matrix(dd[-c(1:6,11:12)]))$coef)))
                      })
coeffs<-rbind(cbind(coeffsy1[nchar(names(coeffsy1))<15],y="y1"),cbind(coeffsy2[nchar(names(coeffsy2))<15],y="y2"))


coeffs$error<-is.na(coeffs$csplus1)
sum(coeffs$error)
nrow(coeffs)
oeifg<-plyr::ddply(coeffs[!coeffs$error,],~run+y,function(dd){
  plyr::ldply(dd[-match(c("run","larve","error","y"),names(coeffs))],function(x){try((function(){yy=t.test(x,level=.99);yy<-c(yy$conf.int,yy$estimate);names(yy)<-c("BI","BS","estimate");yy})())},.id="Effect")
})

oeifg$error<-is.na(oeifg$estimate)
names(oeifg)[match("V1",names(oeifg))]<-"error"
oeifg[(!oeifg$error)&oeifg$.id!="ma1"&(oeifg$BI>0|oeifg$BS<0),]


oeifg$pos<-as.factor((oeifg$BI>0|oeifg$BS<0)*sign(oeifg$BI))


ggplot(oeifg,aes(x=run,y=estimate,ymin=BI,ymax=BS,color=pos))+
  geom_pointrange(fatten=.4,size=.6)+facet_wrap(Effect~y)+
  geom_hline(yintercept = 0, linetype=2)
