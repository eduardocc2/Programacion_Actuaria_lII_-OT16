completos<- function(directorio,id=1:332){
  setwd("~/Desktop/Programacion III/Programacion_Actuaria_lII_-OT16/specdata")
  #me fija la base del directorio
  
  superior<-max(id)
  inferior<-min(id)
  
  x<-c(id)
  
  
 
  
  
  nobes<- vector("numeric", length(id))
  
  k=1
  for(i in id){
    contador<-0
    yo<- x[i]
    yo
    u4=0
    if(i<10){
      u4<- paste("00",yo,".csv",sep="")
    }else if(i<100) {
      u4<- paste("0",i, ".csv",sep="")
    } else {
      u4<- paste(i,".csv",sep="")
    }
    
    
    direc<- read.csv(file=u4)
    complete.cases(direc)
    direcp<- direc[complete.cases(direc),]
    #direcp
    

    contador<- contador + nrow(direcp)
    nobes[k]<- contador

    datos<-data.frame(ID=id,nobes)
    k<- k+1
  } #termina el for
  
  
datos
}