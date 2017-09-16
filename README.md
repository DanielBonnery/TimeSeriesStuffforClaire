# Times Series Stuff for Claire 

## 1. How to install the package

```r
devtools::install_github("DanielBonnery/TimeSeriesStuffforClaire")
```


## 2. Reproduction of the computations made on CPS data. 


```r
library("TimeSeriesStuffforClaire")
demo("ComputeStats")
```

```
FALSE 
FALSE 
FALSE 	demo(ComputeStats)
FALSE 	---- ~~~~~~~~~~~~
FALSE 
FALSE > library("tseries")
FALSE 
FALSE > unloadNamespace("TimeSeriesStuffforClaire")
FALSE 
FALSE > library("TimeSeriesStuffforClaire")
```

```
FALSE 
FALSE Attaching package: 'TimeSeriesStuffforClaire'
```

```
FALSE The following object is masked _by_ '.GlobalEnv':
FALSE 
FALSE     signals
```

```
FALSE 
FALSE > data("signals")
FALSE 
FALSE > library(ggplot2)
FALSE 
FALSE > library(reshape2)
FALSE 
FALSE > table(signals$larve,signals$run)
FALSE     
FALSE        1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
FALSE   1  673 472 473 473 473 459 456 459 459 678 459 459 473 473 678
FALSE   2  677 458 458 458 458 472 472 472 472 677 472 472 458 458 677
FALSE   3  677 472 472 472 472 458 458 458 458 677 458   0   0   0   0
FALSE   4  677 236 472 472 472 458 458 458   0 677 458 458 472 472 677
FALSE   5  677 458 458 458   0   0 472 472 472 677 472 472 458 458 677
FALSE   6    0 458 458 458 458 472 472 472 472 677 472 472 458 458 677
FALSE   7  677 472 472 472 472 458 458 458 458 677 458 458 458 472 677
FALSE   8  900 476 476 476 458 458 458   0 472 896 463 463 450 450 904
FALSE   9  891 463 463 463 454 454 454 467 467 913 467 467 454 454 909
FALSE   10 904 467 467   0 458 463 463 458 467 904 450 467 467   0   0
FALSE   11 913 454 454 472 472 480 480 458 836 900 472 472 467 467 896
FALSE   12 900 458 458 463 463 476 476 467   0 909 450 450 472 472 913
FALSE   14 904 472 472 458 458 458 458 467 467 913 450 450 472 472 904
FALSE   15 896 476 476 476 454 454 476 476 476 891 467 467 450 450 913
FALSE   16 904 463 463 463 454 454 467 467 454 896 458 458 480 450 891
FALSE   17 891 935 935   0   0   0   0   0   0 891   0   0   0   0 891
FALSE   18 909 463 463 480 480 480 450   0   0 891 450 450 450 467 909
FALSE   19 887 458 458 458 454 454 454 463 463 896 476 476 463 463 904
FALSE 
FALSE > plyr::ddply(signals[signals$run==1,],
FALSE +             ~larve,
FALSE +             function(dd){sum(dd$light1)})
FALSE    larve V1
FALSE 1      1  0
FALSE 2      2  0
FALSE 3      3  0
FALSE 4      4  0
FALSE 5      5  0
FALSE 6      7  0
FALSE 7      8 21
FALSE 8      9 22
FALSE 9     10 22
FALSE 10    11 22
FALSE 11    12 22
FALSE 12    14 22
FALSE 13    15 22
FALSE 14    16 22
FALSE 15    17 21
FALSE 16    18 22
FALSE 17    19 22
FALSE 
FALSE > signals<-signals[is.element(signals$larve,c(9:16)),]
FALSE 
FALSE > signals$csplus1<-signals$csplus1*(1-signals$light1)
FALSE 
FALSE > signals$csplus2<-signals$csplus2*(1-signals$light2)
FALSE 
FALSE > exclude=(signals$test!=2)|(signals$run==1&signals$larve==2)|(signals$run==4&signals$larve==11)
FALSE 
FALSE > Order<-c(0,2,1)
FALSE 
FALSE > coeffsy1<-plyr::ddply(signals,
FALSE +                     ~larve+run,
FALSE +                     function(dd){
FALSE +   #Y=irts(time=dd$time,value=dd$y1)
FALSE +   as.data.frame(as.list(
FALSE +     try(arima(x = dd$y1,order = Order,xreg = as.matrix(dd[-c(1:6,11:12)]))$coef)))
FALSE + })
FALSE 
FALSE > coeffsy2<-plyr::ddply(signals,
FALSE +                       ~larve+run,
FALSE +                       function(dd){
FALSE +                         #Y=irts(time=dd$time,value=dd$y1)
FALSE +                         as.data.frame(as.list(
FALSE +                           try(arima(x = dd$y2,order = Order,xreg = as.matrix(dd[-c(1:6,11:12)]))$coef)))
FALSE +                       })
FALSE 
FALSE > coeffs<-rbind(cbind(coeffsy1[nchar(names(coeffsy1))<15],y="y1"),cbind(coeffsy2[nchar(names(coeffsy2))<15],y="y2"))
FALSE 
FALSE > coeffs$error<-is.na(coeffs$csplus1)
FALSE 
FALSE > sum(coeffs$error)
FALSE [1] 19
FALSE 
FALSE > nrow(coeffs)
FALSE [1] 202
FALSE 
FALSE > oeifg<-plyr::ddply(coeffs[!coeffs$error,],~run+y,function(dd){
FALSE +   plyr::ldply(dd[-match(c("run","larve","error","y"),names(coeffs))],function(x){try((function(){yy=t.test(x,level=.99);yy<-c(yy$conf.int,yy$estimate);names(yy)<-c("BI","BS","estimate");yy})())},.id="Effect")
FALSE + })
FALSE 
FALSE > oeifg$error<-is.na(oeifg$estimate)
FALSE 
FALSE > names(oeifg)[match("V1",names(oeifg))]<-"error"
FALSE 
FALSE > oeifg[(!oeifg$error)&oeifg$.id!="ma1"&(oeifg$BI>0|oeifg$BS<0),]
FALSE [1] run      y        Effect   BI       BS       estimate error   
FALSE <0 rows> (or 0-length row.names)
FALSE 
FALSE > oeifg$pos<-as.factor((oeifg$BI>0|oeifg$BS<0)*sign(oeifg$BI))
FALSE 
FALSE > ggplot(oeifg,aes(x=run,y=estimate,ymin=BI,ymax=BS,color=pos))+
FALSE +   geom_pointrange(fatten=.4,size=.6)+facet_wrap(Effect~y)+
FALSE +   geom_hline(yintercept = 0, linetype=2)
```

![plot of chunk r2](figure/r2-1.png)

