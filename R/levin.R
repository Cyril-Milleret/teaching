levin<-function(species,stand=TRUE){
 x <- 1/(sum((species/100)^2))
  if(stand==TRUE){
  x<-(x-1)/(length(species)-1)
}
return(x)
}


spec<-c(10,40,0,50,0,0)
levin(spec,stand=T)
