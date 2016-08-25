#source("C:/Users/MIGUEL ANGEL CORDERO/Dropbox/EM/Manuales/R/Paquetes.R")
rm(list=ls()) #para borrar las variables declaradas anteriormente
cat("\014")#Para borrar la consola
setwd(getwd())

semilla <- function(valor){
  set.seed(valor)
}

genera.base<-function(datos,estrato,tamanio,tdominio,dominio){
  names(datos)<-tolower(names(datos))
  #Insertar el nombre dela variable que estratifica####
  names(datos)[names(datos)==estrato]<-"estrato"
  names(datos)[names(datos)==tamanio]<-"ln"
  names(datos)[names(datos)==tdominio]<-"dominio"
  
  #Selecciono unicamente al municipio de interés si es necesario, si no, se omite (o se comenta)
  if(dominio!="0 Completo"){
    datos <- datos[datos$dominio==dominio,]
  }
  return(datos)
}

genera.tamanio<-function(tamanios){
  names(tamanios)<-c("estrato","nh")
  return(tamanios)
}

muestras <- function(Base,tamanios,variables,int_ratio,nsim,partidos,esquema,dominio,semilla,export=FALSE){
  set.seed(semilla)
  
  variables<-tolower(variables[variables!=""])
  int_ratio<-tolower(int_ratio)
  partidos<-tolower(partidos)
  
  #Aquí seleccionamos de la base variables, qué columna contiene los parámetros de interés
  var_muestreo <- c("seccion","tiposeccion","cve_entidad","mun","nom_mun",
                    "dominio","estrato","ln")
  
  #Obtenemos el tamaño poblacional
  N <- length(Base$seccion) #tamaño de la población
  
  #Generamos Nh  y nh####
  Nh <- as.data.frame(table(Base$estrato))
  L <- dim(Nh)[1]
  colnames(Nh)<-c("estrato","Nh")
  Base$Nh<-NA
  Base$nh_<-NA
  Base$tamanio<-NA
  names(Base)
  
  for (i in 1:L) {
    Base$Nh[Base$estrato==Nh[i,1]] <- Nh[i,2]
    Base$nh_[Base$estrato==Nh[i,1]] <- tamanios[tamanios$estrato==Nh[i,1],2]
    Base$tamanio[Base$estrato==Nh[i,1]] <- sum(Base[Base$estrato==Nh[i,1],]$ln,na.rm = TRUE)
  }
  
  if(esquema=="PPT Sistematico"){
    Base$pik <- Base$ln/Base$tamanio
  }else if (esquema=="Sistematico simple"){
    Base$pik<-1/Base$Nh
  }
  
  Base<-Base[order(Base$estrato),]
  Base<- Base[,c(var_muestreo,as.character(variables),"Nh","pik")]
  
  ##Obtenemos los valores poblacionales de las variables de interes####
  val_pob <- NA
  i<-1
  for(aux in variables){
    val_pob[i]<-sum(Base[,aux],na.rm = TRUE)/sum(Base[,variables[length(variables)]],na.rm = TRUE)
    i<-i+1
  }  
  names(val_pob)<-variables  
  
  ####Simulación de muestras para obtener DEFF, IC de var#####
  sim_results <- list()
  i=1
  for(aux in variables){
    sim_results[[aux]]<-data.frame(rat_1=rep(NA,nsim), LI=rep(NA,nsim),LS=rep(NA,nsim),se=rep(NA,nsim),deff=rep(NA,nsim))
    i<-i+1
  }
  
  #aux=as.name("pan_11_l_g")
  #i=1
  for(i in 1:nsim){
    m<- strata(Base,stratanames = "estrato",size=tamanios$nh,method="systematic",
               pik = Base$pik )
    m$fexp <- 1/m$Prob
    mimuestra <- getdata(Base,m)
    disenio <- svydesign(id=~1,data=mimuestra,weights=~fexp,strata =~estrato ,description=TRUE,fpc=~Nh)
    
    for(aux in variables){
      v1<-substitute(~i,list(i = as.name(aux)))
      t1<-substitute(~i,list(i = as.name(variables[length(variables)])))
      rat_1 <-svyratio(as.formula(v1),as.formula(t1),disenio,na.rm=TRUE,deff=TRUE)
      se_1<-sqrt(rat_1$var)
      deff1 <- attributes(rat_1)$deff
      IC_rat_1 <- confint(rat_1,deff=TRUE,level = 0.95)
      sim_results[[aux]][i,] <-c(rat_1$ratio ,IC_rat_1 [1],IC_rat_1 [2],se_1,deff1) 
    }
  }
  
  #Intervalos de confianza para estimadores de razon de interés#
  x<-0
  y<-0
  ii<-1
  medias<-val_pob[int_ratio]
  distance<-max(medias)-min(medias)
  cota.max<-max(medias)+distance*.35
  cota.min<-min(medias)-distance*.35
  if(cota.max>=1)
    cota.max=1
  if(cota.min<=0)
    cota.min=0
  for (aux in int_ratio) {
    valores<- sim_results[[aux]]
    Media <- val_pob[aux]
    med_muestral<-valores$rat_1
    l_inf<-valores$LI
    l_sup<-valores$LS
    # yticks_val <- pretty_breaks(n=50)((cota.max+cota.min)/2)
    yticks_val <- seq(cota.min,cota.max,(cota.max-cota.min)/10)
    if(ii==1){
      plot(x,y,yaxt="n",col="white",xlim=c(0,nsim),ylim = c(cota.min,cota.max),
           #main=paste("Simulación de Intervalos de Confianza al 95% para Presidente Municipal"),
           main=paste("Intervalo de Confianza al 95% para ",partidos," Dominio: ",dominio,sep=""),
           xlab="Número de muestra",ylab="Porcentaje",add=TRUE)
      axis(2, at=yticks_val, lab=percent(yticks_val))
      ii<-ii+1
    }
    abline(h=Media, col="deeppink3",add=TRUE)
    text(nsim/20,Media,aux,srt=1,pos=1)
    for(j in 1:nsim){
      points(j,med_muestral[j],col="deeppink4",pch=".",cex=2.5)
      polygon(c(j,j),c(l_inf[j],l_sup[j]),border="deeppink3",add=TRUE)
    }
  }
  grafico<-recordPlot()
  
  #Generamos el archivo con los resultados de la simulación####
  
  resultados<-data.frame(variable=NA,rat_1=NA,LI=NA,LS=NA,se=NA,deff=NA,varpob=NA)
  i=1
  for(aux in sim_results){
    resultados[i,1]<-names(sim_results)[i]
    resultados[i,2:6]<-round(colMeans(aux),3)
    resultados[i,7]<-round(val_pob[i],3)
    i=i+1
  }
  
  if(export==TRUE){
    write.csv(resultados,paste("resultados_",dominio,".csv",sep=""),row.names = FALSE,na="")
    write.csv(mimuestra,paste("Muestra_",dominio,".csv",sep=""),row.names = FALSE,na="")
  }
  res<- list()
  res[["resultados"]]<-resultados
  res[["muestra"]]<-mimuestra
  res[["intervalos"]]<-grafico
  return(res)
  
}

# #Ingresar el esquema de muestreo: 1 ppt sistemático 2: swor systemathic
# esquema <- "PPT Sistematico"
# #print("Ingrese el dominio según la base de datos: 1, 2, 3 para algún distrito federal , 0 Estatal")
# tdominio <- "dominio"
# #print("Ingrese el dominio según la base de datos: 1, 2, 3 para algún distrito federal , 0 Estatal")
# dominio <- "0 Completo"
# # dominio <- 1
# #Aquí ponemos el nombre del estrato de interés (debe estar declarado en el marco muestral)
# estrato <- "estrato"
# #Aquí ponemos el nombre del estrato de interés (debe estar declarado en el marco muestral)
# partidos <- "GOB11"
# #Aquí ponemos el nombre del estrato de interés (debe estar declarado en el marco muestral)
# tamanio <- "ln"
# #generamos la base con los tamanios de muestra para cada estrato####
# tamanios <- read.csv("Ejemplo/tamanios_estratos.csv")[order(read.csv("Ejemplo/tamanios_estratos.csv")$estrato),]
# #generamos la base con las variables de los partidos
# variables <-read.csv("Ejemplo/variables_partidos.csv",header = TRUE,na.strings = NA) [,1]
# #leer la base de datos####
# datos <- read.csv("Ejemplo/EVE18NayaritMM20160819.csv")
# 
# Base <- genera.base(datos,estrato,tamanio,tdominio,dominio)
# tamanios<-genera.tamanio(tamanios = tamanios)
# int_ratio<-c("pan_11_l_g","pri.pv.na_11_l_g")
# nsim<-50
# 
# muestras(Base,tamanios,variables,int_ratio,nsim,partidos,esquema,dominio,export=FALSE)["resultados"]
# 


