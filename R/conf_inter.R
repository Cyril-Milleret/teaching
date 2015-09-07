
conf_inter<- function(x, y, se=SE, stder=TRUE, low_ci=lowci, up_ci=upci, z=0.1,col="black",lty=1, lwd=1){


  if(stder==TRUE){

    if(length(SE)!=length(x) | length(y)!=length(x))
      stop("x, y and SE have not the same length")

    for ( i in 1:length(x)){
      segments(x[i] , y[i] + (1.96*SE[i]) ,x[i] , y[i] -(1.96*SE[i]) ,col=col  ,lty = lty, lwd= lwd  )
      segments(x[i]-z, y[i] + (1.96*SE[i]), x[i]+z, y[i] + (1.96*SE[i]))
      segments(x[i]-z, y[i] - (1.96*SE[i]), x[i]+z, y[i] - (1.96*SE[i]))
    }
  }

  if(stder==FALSE){
    if(length(lowci)!=length(x) | length(y)!=length(x) | length(upci)!=length(x))
      stop("x, y and SE have not the same length")


    for ( i in 1:length(x)){
      segments(x[i] , lowci[i] ,x[i] , upci[i] ,col=col, lty =lty, lwd= lwd  )
      segments(x[i]-z, lowci[i], x[i]+z, lowci[i])
      segments(x[i]-z, upci[i], x[i]+z, upci[i])
    }
  }
}

