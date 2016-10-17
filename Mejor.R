
mejor <- function(Abreviatura,resultado){
  ####Inicio
  ##Pruebas
  #Abreviatura <- "TX"
  #resultado <- "Ataque"
  setwd("~/Desktop/Programacion III/Programacion_Actuaria_lII_-OT16/Proyecto 2")
  direc <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  direcp <-direc[complete.cases(direc),]
  ######
  
  #Parte de los ataques
  y <- direcp[,2] #Nombre de los direcpospitales
  a <- direcp[,7]      #Abreviaturas
  direcp <- data.frame(direcp,4706)
  if(resultado=="Ataque"){
    k <- direcp[,11]  
  }else if(resultado=="Falla"){
    k <- direcp[,17]
  }else if(resultado=="Neumonia"){
    k <- direcp[,23]
  }
  
  
  
  perro <- cbind(y,a,k) #perro es el conjunto del direcpospital con la causa de muerte y abreviatura
  
  #Abreviatura <- "TX"
  #resultado <- "car"
  cfr <- 0
  for(fr in 1:4706){
    if(Abreviatura != perro[fr,2]){
      cfr <- cfr+1 
      
    }
  }
  
  if(cfr== 4706)stop("estado invalido")
  
  
  if( resultado != "Ataque" && resultado != "Falla" && resultado != "Neumonia")stop("resultado invalido")
  
  
  
  
  
  
  
  
  #Segundo filtro
  k2 <- vector("numeric",0) #valores
  y2 <- vector("character",0) #nombre de los direcpospitales
  a2 <- vector("character",0) #abreviatura (la misma para todos los valores)
  
  contador <- 0
  for(j in 1:4706){
    if(Abreviatura==perro[j,2]){
      contador <- contador+1
      length(y2) <- length(y2)+1 #representa el nombre de los direcpospitales que vamos a incluir
      length(k2) <- length(k2)+1 #representa el valor de las tasas
      length(a2) <- length(a2)+1
      y2[contador] <- perro[j,1]
      k2[contador] <- perro[j,3]
      a2[contador] <- Abreviatura
      
    }
    
  }
  
  k3 <- as.numeric(k2) #convertimos el vector de caracteres a numerico
  gato <- cbind(y2,a2,k2) #base filtrada pero con NA
  k4 <- k3[complete.cases(k3)] #valores correspondietes
  
  x <- min(k4) #Valor a buscar
  x1 <- as.integer(x)
  
  if(x==x1){
    xx <- paste(x,".0",sep = "")  
  }else{
    xx <- as.character(x)
  }
  
  
  
  #compara <- function(x){  ##este for buscara el nombre del hospital
  n <- nrow(gato)
  yo <- vector("character",0)
  contador1 <- 0
  for(i in 1:n){
    if(xx == gato[i,3]){
      contador1 <- contador1 + 1
      length(yo) <- length(yo) + 1
      yo[contador1] <- gato[i,1]
    }
  }
  
  
  
  tu <- order(yo,na.last = T,decreasing = FALSE) #Número de coordenada del primer resultado
  lugar <- tu[1]
  resultadofinal <- yo[lugar]
  resultadofinal
  
  
  
  
}   

mejor
