rankingcompleto <- function(resultado,rank){
  ##Pruebas
  #Abreviatura <- "TX"
  resultado <- "Ataque"
  rank <- "peor"
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
  
  
  
  perro <- cbind(y,a,k) #perro es el conjunto del direcpospital con la causa de muerte 
  
  #validación
  cfr <- 0
  for(fr in 1:4706){
    if(Abreviatura != perro[fr,2]){
      cfr <- cfr+1 
      
    }
  }
  #Validación
  if(cfr== 4706)stop("estado invalido")
  
  
  if( resultado != "Ataque" && resultado != "Falla" && resultado != "Neumonia")stop("resultado invalido")
  
  
  k3 <- as.numeric(k) #convertimos el vector de caracteres a numerico
  #gato <- cbind(y2,a2,k2) #base filtrada pero con NA
  k4 <- k3[complete.cases(k3)] #valores correspondietes
  siria <- sort(k4,decreasing = F) #Ordenar en orden decreciente sin los NA
  suc <- cbind(1,siria)
  delta <-  nrow(suc) #numero de renglones
  gama <- c(1:delta) #Crear el número del rank
  landa <- nrow(perro)
  suca <- cbind(siria,gama)
  
  
  #Terminar el filtro con el número de rank
  k7 <- vector("numeric",0) #valores
  y7 <- vector("character",0) #nombre de los direcpospitales
  a7 <- vector("character",0) #abreviatura (la misma para todos los valores)
  estado <- vector("character",0)
  
  contadorfalso <-0
  
  for(lg in 1:delta){
    contadorfalso <- contadorfalso +1  
    
    x <- siria[lg] #Valor a buscar
    x1 <- as.integer(x)
    if(x==x1){
      xx <- paste(x,".0",sep = "")  
    }else{
      xx <- as.character(x)
    }
    k77 <- vector("numeric",0) #valores
    y77 <- vector("character",0) #nombre de los direcpospitales
    a77 <- vector("character",0) 
    estado7 <- vector("character",0)
    
    
    contador7 <- 0
    for(sr in 1:landa){
      
      
      if(xx==perro[sr,3]){
        contador7 <- contador7+1
        
        length(y77) <- length(y77)+1 #representa el nombre de los hospitales que vamos a incluir
        length(k77) <- length(k77)+1 #representa el valor de las tasas
        length(a77) <- length(a77)+1
        length(estado7) <- length(estado7)+1
        y77[contador7] <- perro[sr,1] #nombre del hospital
        k77[contador7] <- perro[sr,3] #valor de la tasa
        a77[contador7] <- gama[lg]
        estado7[contador7] <- perro[sr,2]
        
      }
    }
    
    
    length(y7) <- length(y7)+1 #representa el nombre de los hospitales que vamos a incluir
    length(k7) <- length(k7)+1 #representa el valor de las tasas
    length(a7) <- length(a7)+1
    length(estado) <- length(estado)+1
    
    tu <- order(y77,decreasing = FALSE) #Número de coordenada del primer resultado
    ws <- length(tu)
    lugar <- tu[1:contador7]
    resultadofinal <- y77[lugar]
    y7[contadorfalso] <- resultadofinal
    
    
    for(qw in 1:ws){
      if(contadorfalso>1 && y7[contadorfalso-(qw-1)]==y7[contadorfalso]){
        lugar <- tu[qw]
        resultadofinal <- y77[lugar]
        y7[contadorfalso] <- resultadofinal
      }
    }
    k7[contadorfalso] <- k77
    a7[contadorfalso] <- a77
    estado[contadorfalso] <- estado7
    
    
    
  } 
  
  sucar <- cbind(y7,k7,estado,a7)
  ###Ya esta la base filtrada
  if(rank=="mejor"){
    xc <- 1
  }else if(rank=="peor"){
    xc <- delta
  }else if(rank>delta){
    stop("NA")
  }else{ 
    xc <- rank #Valor a buscar
  }
  
  #compara <- function(x){  ##este for buscara el nombre del hospital
  yorc <- sucar[xc,1]
  print(yorc)
  View(sucar)
  
  
}