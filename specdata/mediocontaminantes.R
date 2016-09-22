mediocontaminantes<- function(directorio,contaminante,id=1:332){
  setwd("~/Desktop/Programacion III/Programacion_Actuaria_lII_-OT16/specdata")
  #me fija la base del directorio
  
  superior<-max(id)
  inferior<-min(id)
  
  x<-c(id)
  
  contador<-0
  suma<-0
  resultado<- 0
  
  
  k=1
  for(i in id){
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
  
  if(contaminante=="sulfate"){#condicionContaminante
    y<- direcp[,2]
  } else if(contaminante =="nitrate"){
    y<- direcp[,3]
  }
  
  contador<- contador + nrow(direcp)
  suma<- suma +sum(y)
  
  #contador[k]<-nrow(direcp)
  #suma[k]<- sum(y)  
  k<- k+1
  } #termina el for
  
  
  #(suma*contador)/sum(contador)
  resultado<-suma/contador
  resultado
}