# produccion de frijol--------------
# carlos Eduardo 


#cargar librerias----
library(reshape)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)


#Definir directorio de trabajo-------------
setwd("C:/Users/CEGONZALEZ/Documents/IMPACT3-Model-ver3.2/OutputFiles/Aggregation/")
#Dirreción graficos
grd<-"C:/Users/CEGONZALEZ/Documents/BIDCarlos/"

options(scipen=99)
options(digits=2)

#Cargar marco de datos principal---------------
#md<-read.csv("Resultados_StCty_31_08_16_new.csv",header=T) # datos por regiones 
s<- read.csv("Resultados_Ciat_StCty_31_08_16_new.csv", header=T) # datos por paises


#Hacer un subconjunto que sólo contenga las variables de mi interés--------------------
mdsub<- s[which(s$impactparameter=="TAreaXAgg -- Total Area" & s$commodity=="jbean"),]
#mdsub<-subset(s,s$impactparameter=="QSXAgg -- Total Production" & s$commodity=="jbean")

mdsub$impactparameter<-revalue(mdsub$impactparameter, c("TAreaXAgg -- Total Area"="Area"))
mdsub$commodity<-revalue(mdsub$commodity, c("jbean"="Bean"))
mdsub$region<-revalue(mdsub$region, c("LAC-Chile"="Chile", "LAC-Argentina"="Argentina", "LAC-Paraguay"="Paraguay"))

rownames(mdsub)<-1:nrow(mdsub)

#paises del SUR
#Argentina, Chile y Paraguay--------------------
mdsub<- mdsub[which(mdsub$region=="Chile"|mdsub$region=="Argentina"|mdsub$region=="Paraguay"),]
rownames(mdsub)<-1:nrow(mdsub)


# agregar la produccion por GCM
#qsur<-aggregate(mdsub[,"Val"],
#                   by=list(mdsub$region,,
#                           mdsub$scenario,mdsub$year), FUN=median)

qsur_wide <- reshape(mdsub, v.names = "Val", idvar = 
                       c("scenario","commodity","region","productiontype",
                         "impactparameter"),timevar = "year", direction = "wide")

# Calcular la mediana de la produccion
qsur_wide$Mean_q <- rowMeans(qsur_wide[,paste0("Val.",2020:2050)])
qsur_wide1<- qsur_wide # haciendo una copia
qsur_wide1 <- qsur_wide1[,-(5:51)] 
rownames(qsur_wide1)<- 1:nrow(qsur_wide1)




# graficos de la produccion
t <- ggplot(qsur_wide1, aes(x=region, y=Mean_q, fill=scenario))
t <- t + geom_bar(stat="summary", fun.y = "mean", position="dodge", size=.3, show.legend=TRUE)
t <- t + xlab("Paises") + ylab(" Area promedio\nhectareas")
t <- t + scale_fill_brewer(palette="Spectral", guide_legend(title = "Escenarios"))
t <- t + scale_y_continuous(limits = c(0, 1200))
t <- t + theme(axis.text.x=element_text(size=12))
t <- t + theme(axis.text.y=element_text(size=12))
t <- t + theme(axis.title.x=element_text(size=14, face='bold'))
t <- t + theme(axis.title.y=element_text(size=14, face='bold'))
t <- t + theme(plot.title=element_text(size=16, face='bold'))
t <- t + theme(legend.text = element_text(size=12))
t <- t + theme(legend.title = element_text(size=12, face = 'bold'))
t <- t + ggtitle(label = "Area promedio de frijol por paises \nde la region SUR\n Periodo 2020-2050")
t <- t + theme(plot.title = element_text(lineheight=.8, face="bold")) 
t



tiff(filename=paste(grd,"AreaSurBean.tiff",sep=""), 
     width = 10, height = 10, units = 'in', res = 200)

t

dev.off()

# proporcion 
qsur_wide1 = qsur_wide1[!qsur_wide1$scenario=="NoCC",] # para eliminar los datos de no cambio climatico
qsur_wide1_agg<-aggregate(qsur_wide1[,"Mean_q"],by=list(qsur_wide1$region),FUN=mean) 

# en promedio argentina tiene un peso del 83% en la produccion 
