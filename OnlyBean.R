# Tabla de Tim 

#FPU name or code ///  GCM name or code ///  Harvested area in FPU ///  Production in FPU in 2000 (or whatever base year) /// Production in FPU in 2050 (or whatever final year)


#cargar librerias----
library(reshape)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)

#Definir directorio de trabajo-------------
#Dirreción graficos
grd<-"C:/Users/CEGONZALEZ/Documents/BIDCarlos/"

# cargar los datos base
options(scipen = 999)


# procesamiento para la tabla de solo fijol-----------------
#Especificar cultivo
cultivos = c("frijol")
cultivos.en = c("bean")
treat = c('riego','secano')  #riego o secano

grd1<-c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/BID_Evaluacion_Economica/")
grd2<-c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/BID_Evaluacion_Economica/12-Resultados/")
grd3<-c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/BID_Evaluacion_Economica/08-Cells_toRun/matrices_cultivo/")
grd4<-c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/BID_Evaluacion_Economica/Resultados_agregados/")

for (c in 1:length(cultivos)) {
  
  for (t in 1:length(treat)) {
    #Cargar lista de GCM's
    models <- read.csv(paste(grd1,"ModelosGCM.csv",sep=""),header=T,stringsAsFactors=F)
    models <- rbind(models,'WFD')
    
    #Cargar información de latitud, longitud, area de spam, fpu, etc.
    load(paste(grd3,cultivos.en[c],'_',treat[t],'.RDat',sep=''))
    
    #Ciclo por GCM y WFD
    for ( j in 1:dim(models)[1])  {
      
      #Cargar información de los resultados de los modelos de cultivo
      load(paste(grd2,cultivos[c],'/',cultivos[c],'-ENTREGA4','/','_',cultivos[c],'_',treat[t],'_',models[j,],'.RDat',sep=''))
      
      #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
      rend<-matrix(nrow=length(Run), ncol=24)
      for (i in 1:length(Run))
      {rend[i,]<- Run[[i]] [,"HWAH"][1:24]  #solo incluir años 1 a 24
      } 
      
      #descartar pixeles con demasiadas fallas
      rend[rend==-99] = 0  #convertir -99 a 0
      
      #find areas where consistently failing and discard these from aggregation
      zeros.wfd.r = apply(rend,1,function(x) sum(x==0,na.rm=T))
      ind.falla = which(zeros.wfd.r>=13)
      
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("Rend_20",22:45)
      
      #Crear un data frame con toda la información que necesitamos Longitud, latitud, ID, Area, FPU, y rendientos de 2022 a 2045
      eval(parse(text=paste('md<-data.frame(long=crop_',treat[t],'[,"x"],lat=crop_',treat[t],'[,"y"],Area=crop_',treat[t],'[,"',treat[t],'.area"],FPU=crop_',treat[t],'[,"New_FPU"], rend)',sep='')))
      
      #Agregar columnas de producción de 2022 a 2046
      md[,paste0("Prod_20",22:45)]<-md[,"Area"]*md[,paste0("Rend_20",22:45)]
      md[,'ones'] = 1
      #Eliminar las columnas de los rendimientos
      md<-md[,!names(md) %in% (paste0("Rend_20",22:45))]
      
      # Descartar pixeles con más de 13 años con fallas en la linea base
      if(sum(ind.falla) == 0)
      {
        md<-md
      } else {
        md<-md[-ind.falla,]
      }
      
      #Agregar producción y area a nivel de fpu
      md_fpu<-aggregate(md[,c("ones","Area",paste0("Prod_20",22:45))],by=list(md[,"FPU"]),FUN= function(x) {sum(x, na.rm=TRUE)} )
      
      #Agregar Rendimientos a nivel de fpu (rendimiento ponderado)
      md_fpu[,paste0("Rend_fpu_20",22:45)]<-md_fpu[,paste0("Prod_20",22:45)]/md_fpu[,"Area"]
      
      
      #exportar datos de produccion y rendimiento 
      write.csv(md_fpu,paste(grd,cultivos[c],'_',treat[t],'_',models[j,],'_FPU.csv',sep=''),row.names=T)
      
  
    }
    
    print(c)
  }
  
}


# agregar los datos de los de frijol-------------------
setwd("C:/Users/CEGONZALEZ/Documents/BIDCarlos/")

grd1<-c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/BID_Evaluacion_Economica/")
grd2<-c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/BID_Evaluacion_Economica/12-Resultados/")
grd3<-c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/BID_Evaluacion_Economica/08-Cells_toRun/matrices_cultivo/")
grd4<-c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/BID_Evaluacion_Economica/Resultados_agregados/")



#Lista de tipos de sistemas
sist<-c("riego","secano")
sist.en<-c("air","arf")

#Cargar lista de modelos
models <- read.csv(paste(grd1,"ModelosGCM.csv",sep=""),header=T,stringsAsFactors=F)
models = rbind(models,'WFD')

#Lista de cultivos
crops<-c("frijol")
crops.en<-c("bean")
crops.enj<-c("jbean")

c=1
tables2 <- lapply(1:length(sist), function(s){
  
  tables <- lapply(1:nrow(models), function(m){
    
    z <- read.csv(paste(crops[c],'_',sist[s],'_',models[m,],'_','FPU.csv',sep=''),header=T, stringsAsFactors = F)
    z$GCM <- paste(models[m,], sep='')
    return(z)
    
  })
  tables <- do.call(rbind, tables)
  tables$Sytem <- sist.en[s]
  return(tables)
  
})
tables2 <- do.call(rbind, tables2)




# grabar de dato-----
write.csv(tables2,paste(grd,"DataBeanFPU.csv",sep=''),row.names=T)

# leer dato como base de datos general------
bean<- read.csv(paste(grd,"DataBeanFPU.csv",sep=''))
bean$X<- NULL
bean$X.1<- NULL

names(bean)[1]<- "FPU"
names(bean)[2]<- "No.Pix"

beanSUR<- bean[which(bean$FPU=="SAL_ARG"|bean$FPU=="PAR_ARG"),]

beanSUR<- beanSUR[,c(1:4,27:28,51:53)]
rownames(beanSUR)<- 1:nrow(beanSUR)
beanSUR<- beanSUR[, c("FPU","No.Pix","GCM","Sytem","Area","Prod_2022","Prod_2045","Rend_fpu_2022","Rend_fpu_2045")]

write.csv(beanSUR,paste(grd,"DataBeanFPUSUR2.csv",sep=''),row.names=T)





# Área modelada

a_model <- read.csv('C:/Users/CEGONZALEZ/Documents/BIDCarlos/DataBeanFPU.csv')
a_model <- a_model[,c("Group.1", "Area", "Sytem")]
colnames(a_model) <- c("FPU", "Area", "System")

# Área base
a_base <- read.csv('C:/Users/CEGONZALEZ/Documents/BIDCarlos/AREAGAMSBEAN.csv')

all_data <- merge(a_model, a_base, by=c("FPU", "System"))
all_data <- unique(all_data); rownames(all_data) <- 1:nrow(all_data)

library(ggplot2)


plot(all_data$Area, all_data$Area_base, ty='p')
abline(0,1)

weird_cases <- all_data[all_data$Area >= 800000,]
arg_cases <- all_data[grep(pattern = "ARG", x = all_data$FPU),]
View(weird_cases)

all_data

#Grafico1
todo<- ggplot(all_data, aes(x=Area, y=Area_base, colour=System)) + geom_point() 
todo<- todo + geom_smooth(method="lm", se=TRUE, level=0.95)  
todo<- todo + theme(legend.position="bottom",legend.text=element_text(size=12),
                                                        legend.key.size = unit(1, "cm"),text = element_text(size=12))
todo<- todo + labs(y="Area Base of IMPACT", x="Area Crops_Model")
todo<- todo + ggtitle(label ="Bean")
todo<- todo + theme(axis.title.x=element_text(size=14, face='bold'))+ 
  theme(legend.title = element_text(size=12, face = 'bold'))
todo<- todo + theme(axis.title.y=element_text(size=14, face='bold')) +
  theme(legend.text = element_text(size=12))
todo<- todo + theme(plot.title = element_text(lineheight=.8, face="bold")) + ylim(0,1250000)
  


tiff(filename=paste(grd,"ComparativeAreasModelAreasIMPACT.tiff"), 
     width = 10, height = 10, units = 'in', res = 300)
todo

dev.off()



#Grafico2
todo2<- ggplot(all_data, aes(x=Area, y=Area_base, colour=System)) + geom_point() 
todo2<- todo2 + geom_smooth(method="lm", se=TRUE, level=0.95)  
todo2<- todo2 + theme(legend.position="bottom",legend.text=element_text(size=12),
                    legend.key.size = unit(1, "cm"),text = element_text(size=12))
todo2<- todo2 + labs(y="Area Base of IMPACT", x="Area Crops_Model")
todo2<- todo2 + ggtitle(label ="Bean < 250 ha (000)")
todo2<- todo2 + theme(axis.title.x=element_text(size=14, face='bold'))+ 
  theme(legend.title = element_text(size=12, face = 'bold'))
todo2<- todo2 + theme(axis.title.y=element_text(size=14, face='bold')) +
  theme(legend.text = element_text(size=12))
todo2<- todo2 + theme(plot.title = element_text(lineheight=.8, face="bold")) +xlim(0, 250000) + ylim(0,250000)



tiff(filename=paste(grd,"ComparativeAreasModelAreasIMPACT250.tiff"), 
     width = 10, height = 10, units = 'in', res = 300)
todo2

dev.off()




#Grafico2

todo2<- ggplot(all_data, aes(x=Area, y=Area_base, colour=System)) + geom_point() + 
  geom_smooth(method="lm", se=TRUE, level=0.95) + xlim(0, 250000) + ylim(0,250000) + 
  geom_text(x = 150000, y = 150000, label = lm_eqn(all_data), parse = TRUE)

lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}



