usepackage("tseries")
data("signals")
plyr::dlply(Y,~l+r,function(dd){
  Y=as.irts(time=dd$t,value=dd$Y)
arima(x = Y,order = ,seasonal = ,xreg = as.matrix(dd[c("Csplus1","Csminus1","Csplus2","Csminus2","newodor1","newodor2","shock1","shock2")]))
})