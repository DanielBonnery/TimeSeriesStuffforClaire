library("tseries")
data("signals")
library(ggplot2)
library(reshape2)


plyr::ddply(signals[signals$run==1,],
            ~larve,
            function(dd){sum(dd$light1)})

signals<-signals[is.element(signals$larve,c())]
signals$csplus1<-signals$csplus1*(1-signals$light1)
signals$csplus2<-signals$csplus2*(1-signals$light2)
exclude=(signals$test!=2)|(signals$run==1&signals$larve==2)|(signals$run==4&signals$larve==11)
coeffsy1<-plyr::ddply(signals,
                    ~larve+run,
                    function(dd){
  #Y=irts(time=dd$time,value=dd$y1)
  as.data.frame(as.list(
    try(arima(x = dd$y1,order = c(0,1,1),xreg = as.matrix(dd[-c(1:6,11:12)]))$coef)))
})
coeffsy2<-plyr::ddply(signals,
                      ~larve+run,
                      function(dd){
                        #Y=irts(time=dd$time,value=dd$y1)
                        as.data.frame(as.list(
                          try(arima(x = dd$y2,order = c(0,1,1),xreg = as.matrix(dd[-c(1:6,11:12)]))$coef)))
                      })
coeffs<-rbind(cbind(coeffsy1,y="y1"),cbind(coeffsy2[-3],y="y2"))


coeffs[[4]]<-!is.na(coeffs[[3]])
names(coeffs)[[4]]<-"error"
coeffs[[3]]<-!is.na(coeffs[[3]])
names(coeffs)[[3]]<-"error2"
sum(coeffs$error)
nrow(coeffs)
oeifg<-plyr::ddply(coeffs[!coeffs$error,],~run+y,function(dd){
  plyr::ldply(dd[-c(1:4,12)],function(x){try((function(){yy=t.test(x,level=.99);yy<-c(yy$conf.int,yy$estimate);names(yy)<-c("BI","BS","estimate");yy})())})
})

oeifg$V1<-!is.na(oeifg$V1)
names(oeifg)[match("V1",names(oeifg))]<-"error"
oeifg[(!oeifg$error)&oeifg$.id!="ma1"&(oeifg$BI>0|oeifg$BS<0),]

reshape2::dcast(coeffs)

oeifg$pos<-as.factor((oeifg$BI>0|oeifg$BS<0)*sign(oeifg$BI))

ggplot(oeifg,mapping = )

ggplot(oeifg,aes(x=run,y=estimate,ymin=BI,ymax=BS,color=pos))+
  geom_pointrange()+facet_wrap(.id~y)
