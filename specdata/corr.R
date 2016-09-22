corre <- function(directorio,horizonte=0){
  setwd("~/Desktop/Programacion III/Programacion_Actuaria_lII_-OT16/specdata")
  #me fija la base del directorio
  
    x<-1:332
    x
    k=1
    nobes <- vector("numeric", 1)
    #horizonte <- 150
    
    
    for(i in 1:332){
      
      
      
      yo <- x[k]
      yo
      
      if (i < 10){
        h4 <- paste("00",i,".csv",sep = "")
      } else if(i<100){
        h4 <- paste("0",i,".csv", sep="")
      } else {
        h4 <- paste(i,".csv",sep="")
      }
      
      direc <- read.csv(file= h4)
      complete.cases(direc)
      direcp <-direc[complete.cases(direc),]
      #direcp
      contador <- nrow(direcp)
      dx <- direcp[,2]
      dy <- direcp[,3]
      
      
      if(contador>horizonte){
        #perro <- cor.test(dx,dy)
        perro <- cor(dx,dy)
        k<- k+1
        length(nobes) <- length(nobes)+1
        nobes[k] <- perro
      }else{
        k <- k
      }
      
      
      
    }
    
    nobes
    
  }
  