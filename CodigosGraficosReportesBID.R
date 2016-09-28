#Programa para analizar los datos de evaluación de frijol con y sin tecnologia-----
#bajo distintos escenarios climaticos
#Por: Jesús Rodríguez modificado modificado por Harold y Carlos Edo

#Cargar librerias-----
library(reshape)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(xtable)
library(dplyr)
library(tidyr)
library(lattice)
library(latticeExtra)

#Limitar numero de decimales-----
options(digits=3) 

#Definir directorios de trabajo y de graficos------
#Directorio de trabajo
setwd("C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/")
#Dirrectorio de graficos
grd<-"C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/graph/"

#Cargar lista de nombres de los archivos----
nombres <- list.files(pattern ="*.csv" )
#Crear muna lista con los nombre de los marco de datos
nombres_md_list<-strsplit(nombres,".csv")
#nombres_md<-c(do.call("cbind",nombres_md_list))  
categoria<-gsub(pattern="Bean_",replacement="",x=nombres_md_list)

#Crear una lista vacia
pl<-NULL

#Cargar los datos dentro de la lista-------

for (i in 1: length(nombres_md_list)) {
  pl[[i]]<-eval(parse(text=paste(nombres_md_list[[i]],'<-read.csv(file=\'',nombres[i],'\',header=T)',sep='')))
  pl[[i]]<-eval(parse(text=paste(nombres_md_list[[i]],'<-data.frame(',nombres_md_list[i],',','"categoria"','=',
                                 "categoria[i]",')',sep='')))
  print(i)
}

#Juntar los elementos de la lista en un marco de datos
md<-do.call(rbind, pl)

#Hacer un subconjunto que sólo contenga las variables de mi interés-----
mdsub<-subset(md,md$impactparameter=="QSXAgg -- Total Production" | 
                md$impactparameter=="TAreaXAgg -- Total Area" |
                md$impactparameter== "TYldXAgg -- Total Yield" | 
                md$impactparameter== "QDXAgg -- Total Demand" |
                md$impactparameter== "QEXAgg -- Export" | 
                md$impactparameter== "QMXAgg -- Import" |
                md$impactparameter== "PWXAgg -- World Prices")

mdsub$impactparameter<-revalue(mdsub$impactparameter, c("QSXAgg -- Total Production"="Production",  
                                                        "TAreaXAgg -- Total Area"="Area",
                                                        "TYldXAgg -- Total Yield"="Yield",
                                                        "QDXAgg -- Total Demand"="Demand",
                                                        "QEXAgg -- Export"="Export",
                                                        "QMXAgg -- Import"="Import",
                                                        "PWXAgg -- World Prices"="W Prices"))


#Hacer un subconjunto que sólo contenga frijol
mdsubcrop<-droplevels(subset(mdsub,mdsub$commodity=="PUL-Beans" | mdsub$commodity=="-"))

mdsubcrop$commodity<-revalue(mdsubcrop$commodity,c("PUL-Beans"="Beans"))

#Hacer un subconjunto de md que sólo contenga los paises de LAC y SSA (Easter, Middle, Western, Southern)
lac_africa_reg<-levels(mdsubcrop$region)[c(68:96,121,127:161)]

mdsubcrop_lac_afri<-mdsubcrop[which(mdsubcrop$region %in% lac_africa_reg),]

mdsubcrop_lac_afri<-droplevels(mdsubcrop_lac_afri)

mdwide <- reshape(mdsubcrop_lac_afri, v.names = "Val", idvar = 
                    c("scenario","commodity","region","productiontype",
                      "impactparameter","categoria"),timevar = "year", direction = "wide")


#q<- mdwide

#Calcular cambio porcentual entre 2020 y 2050----
mdwide<- data.frame(mdwide,"Cambio Porcentual"=( (mdwide$Val.2050-mdwide$Val.2020)/mdwide$Val.2020)*100)

mdwide<-data.frame(mdwide[,1:6],"Cambio Porcentual"=mdwide[,13]) # eliminamos las columnas innecesarias 



# Eliminar datos agrupados innecesarios
mdwide<-mdwide[!mdwide$region=="LAC",]  
mdwide<-mdwide[!mdwide$region=="MEN",]
mdwide<-mdwide[!mdwide$region=="SSA",]

# Eliminar datos que no adoptaron tecnología 
mdwide<-mdwide[!mdwide$region=="LAC-Cuba",]
mdwide<-mdwide[!mdwide$region=="LAC-Jamaica",]
mdwide<-mdwide[!mdwide$region=="MEN-Egypt",]
mdwide<-mdwide[!mdwide$region=="LAC-Other Caribbean",]
mdwide<-mdwide[!mdwide$region=="LAC-Haiti",]
mdwide<-mdwide[!mdwide$region=="LAC-Dominican Republic",]

# importante para renombrar las columnas
rownames(mdwide) <- 1:nrow(mdwide)

#mdwide$categoria <- NULL

mdwide_t<-mdwide %>%
  spread("scenario","Cambio.Porcentual")

cambiop<-mdwide_t

mdwide$categoria <- as.character(mdwide$categoria)

testHarold <- mdwide

# testHarold <- subset(mdwide, subset=mdwide$impactparameter=='Yield')
hist(testHarold$Cambio.Porcentual[-which(testHarold$categoria=='NoCC_NoTec' | testHarold$categoria=='NoCC_Tec')])
boxplot(testHarold$Cambio.Porcentual[-which(testHarold$categoria=='NoCC_NoTec' | testHarold$categoria=='NoCC_Tec')]~testHarold$scenario[-which(testHarold$categoria=='NoCC_NoTec' | testHarold$categoria=='NoCC_Tec')])
testHarold <- testHarold[-which(testHarold$categoria=='NoCC_NoTec' | testHarold$categoria=='NoCC_Tec'),]

#reshape
mdwide_t <- testHarold %>%
  spread("scenario","Cambio.Porcentual")
mdwide_t$productiontype <- NULL
mdwide_t$CC_mean <- rowMeans(x=mdwide_t[,5:ncol(mdwide_t)], na.rm=TRUE)

# para mantener estas variables
mdwide_t <- mdwide_t[,c("impactparameter", "commodity", "region", "categoria", "CC_mean")]
# reshape
mdwide_t <- mdwide_t %>% spread(categoria, CC_mean)

# logica en las variaciones  cambios y  diferencias relativas-----
nn <- which(mdwide_t$CC_No<0 & mdwide_t$CC_Tec<0) 
pp <- which(mdwide_t$CC_No>0 & mdwide_t$CC_Tec>0)
np <- which(mdwide_t$CC_No>0 & mdwide_t$CC_Tec<0)

nn <- intersect(nn, which(mdwide_t$CC_No > mdwide_t$CC_Tec))
pp <- intersect(pp, which(mdwide_t$CC_No > mdwide_t$CC_Tec))
np <- intersect(np, which(mdwide_t$CC_No > mdwide_t$CC_Tec))

noimpact <- c(nn, pp, np)

mdwide_t$impacto<-NA

#loops ajuste inclusion de los paises sin impacto
for(j in 1:nrow(mdwide_t)){
  
  if(j %in% noimpact){
    mdwide_t$impacto[j] <- (mdwide_t$CC_Tec[j] - mdwide_t$CC_NoTec[j])/mdwide_t$CC_NoTec[j]*100
  } else {
    mdwide_t$impacto[j] <- abs(mdwide_t$CC_Tec[j] - mdwide_t$CC_NoTec[j])/unlist(lapply(1:nrow(mdwide_t), function(i){z <- max(abs(mdwide_t$CC_Tec[i]), abs(mdwide_t$CC_NoTec[i]), na.rm=TRUE); return(z)})) * 100
    
  }
  
}



#### grafico teniendo en cuenta grupos
#a) recuperación,  b) recuperación más mejoramiento  y c) mejoramiento
pn <- which(mdwide_t$CC_No<0 & mdwide_t$CC_Tec>0)

nn <- which(mdwide_t$CC_No<0 & mdwide_t$CC_Tec<0) 
pp <- which(mdwide_t$CC_No>0 & mdwide_t$CC_Tec>0)
np <- which(mdwide_t$CC_No>0 & mdwide_t$CC_Tec<0)

alivio <-intersect(nn,which(mdwide_t$CC_No < mdwide_t$CC_Tec))
mejora <-intersect(pp, which(mdwide_t$CC_NoTec < mdwide_t$CC_Tec))
ali_mejora <- intersect(pn,which(mdwide_t$CC_No < mdwide_t$CC_Tec))

# effect <- c(alivio, mejora, ali_mejora); rm(alivio, mejora, ali_mejora)

mdwide_t$trend<-NA

# vectores de posicion 
mdwide_t$trend[alivio] <- 'Alleviate'
mdwide_t$trend[mejora] <- 'Impulse'
mdwide_t$trend[ali_mejora] <- 'Alleviate+Impulse'

#loops ajuste inclusion de los paises Alivio
for(i in 1:nrow(mdwide_t)){
  
  if(i %in% alivio){
    mdwide_t$trend[i] <- "Alleviate"
    
  }
}


#loops ajuste inclusion de los paises mejora
for(i in 1:nrow(mdwide_t)){
  
  if(i %in% mejora){
    mdwide_t$trend[i] <- "Impulse"
  }
  
}

#loops ajuste inclusion de los paises alivio + mejora
for(i in 1:nrow(mdwide_t)){
  
  if(i %in% ali_mejora){
    mdwide_t$trend[i] <- "Alleviate+Impulse"
  }
  
}

# guarar base de datos correcta
write.csv(mdwide_t,"data.csv", row.names = FALSE)

# Para futuros casos iniciar desde aquí
mdwide_t<- read.csv("data.csv")

# grafica de impactos de rendimientos 
mdwide_t <- mdwide_t %>% gather( Intervencion, Value, CC_NoTec:impacto)


# Yield plot
gg <- ggplot(data=mdwide_t[mdwide_t$Intervencion!='impacto' & mdwide_t$impactparameter=='Yield',], aes(y=Intervencion, x=region, fill=Value)) 
gg <- gg + geom_tile(color="white", size=0.1) 
gg <- gg + scale_fill_gradient2(name='Average Change', low="darkblue", high="darkgreen", guide="colorbar") 
gg <- gg + coord_equal()
gg <- gg + ggtitle('Yield') + ylab('') + xlab('Countries')
gg <- gg + theme_bw()
gg <- gg + theme(axis.text.x=element_text(size=10, angle=90))
gg <- gg + theme(axis.text.y=element_text(size=10))
gg <- gg + theme(axis.title.x=element_text(size=12, face='bold'))
gg <- gg + theme(plot.title=element_text(size=15, face = 'bold'))
gg <- gg + theme(legend.text = element_text(size=10))
gg <- gg + theme(legend.title = element_text(size=10, face = 'bold'))
gg

# ggsave(filename='C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/datos procesados/yield.png', plot=gg, width=10, height=3, units='in')

# Demand plot
gg1 <- ggplot(data=mdwide_t[mdwide_t$Intervencion!='impacto' & mdwide_t$impactparameter=='Demand',], aes(y=Intervencion, x=region, fill=Value)) + geom_tile(color="white", size=0.1)# + facet_wrap(~impactparameter, scales="free", ncol=6)
gg1 <- gg1 + scale_fill_gradient2(name='Average Change', low="darkblue", high="darkgreen", guide="colorbar") # limits=c(-15, 80), 
gg1 <- gg1 + coord_equal()
gg1 <- gg1 + ggtitle('Demand') + ylab('') + xlab('Countries')
gg1 <- gg1 + theme_bw()
gg1 <- gg1 + theme(axis.text.x=element_text(size=10, angle=90))
gg1 <- gg1 + theme(axis.text.y=element_text(size=10))
gg1 <- gg1 + theme(axis.title.x=element_text(size=12, face='bold'))
gg1 <- gg1 + theme(plot.title=element_text(size=15, face = 'bold'))
gg1 <- gg1 + theme(legend.text = element_text(size=10))
gg1 <- gg1 + theme(legend.title = element_text(size=10, face = 'bold'))
# gg1

# Production plot
gg2 <- ggplot(data=mdwide_t[mdwide_t$Intervencion!='impacto' & mdwide_t$impactparameter=='Production',], aes(y=Intervencion, x=region, fill=Value)) + geom_tile(color="white", size=0.1)# + facet_wrap(~impactparameter, scales="free", ncol=6)
gg2 <- gg2 + scale_fill_gradient2(name='Average Change', low="darkblue", high="darkgreen", guide="colorbar") # limits=c(-15, 80), 
gg2 <- gg2 + coord_equal()
gg2 <- gg2 + ggtitle('Production') + ylab('') + xlab('Countries')
gg2 <- gg2 + theme_bw()
gg2 <- gg2 + theme(axis.text.x=element_text(size=10, angle=90))
gg2 <- gg2 + theme(axis.text.y=element_text(size=10))
gg2 <- gg2 + theme(axis.title.x=element_text(size=12, face='bold'))
gg2 <- gg2 + theme(plot.title=element_text(size=15, face = 'bold'))
gg2 <- gg2 + theme(legend.text = element_text(size=10))
gg2 <- gg2 + theme(legend.title = element_text(size=10, face = 'bold'))
#gg2

# Import plot
gg3 <- ggplot(data=mdwide_t[mdwide_t$Intervencion!='impacto' & mdwide_t$impactparameter=='Import',], aes(y=Intervencion, x=region, fill=Value)) + geom_tile(color="white", size=0.1)# + facet_wrap(~impactparameter, scales="free", ncol=6)
gg3 <- gg3 + scale_fill_gradient2(name='Average Change', low="darkblue", high="darkgreen", guide="colorbar") # limits=c(-15, 80), 
gg3 <- gg3 + coord_equal()
gg3 <- gg3 + ggtitle('Imports') + ylab('') + xlab('Countries')
gg3 <- gg3 + theme_bw()
gg3 <- gg3 + theme(axis.text.x=element_text(size=10, angle=90))
gg3 <- gg3 + theme(axis.text.y=element_text(size=10))
gg3 <- gg3 + theme(axis.title.x=element_text(size=12, face='bold'))
gg3 <- gg3 + theme(plot.title=element_text(size=15, face = 'bold'))
gg3 <- gg3 + theme(legend.text = element_text(size=10))
gg3 <- gg3 + theme(legend.title = element_text(size=10, face = 'bold'))
#gg3

# Export plot
gg4 <- ggplot(data=mdwide_t[mdwide_t$Intervencion!='impacto' & mdwide_t$impactparameter=='Export',], aes(y=Intervencion, x=region, fill=Value)) + geom_tile(color="white", size=0.1)# + facet_wrap(~impactparameter, scales="free", ncol=6)
gg4 <- gg4 + scale_fill_gradient2(name='Average Change', low="darkblue", high="darkgreen", guide="colorbar") # limits=c(-15, 80), 
gg4 <- gg4 + coord_equal()
gg4 <- gg4 + ggtitle('Exports') + ylab('') + xlab('Countries')
gg4 <- gg4 + theme_bw()
gg4 <- gg4 + theme(axis.text.x=element_text(size=10, angle=90))
gg4 <- gg4 + theme(axis.text.y=element_text(size=10))
gg4 <- gg4 + theme(axis.title.x=element_text(size=12, face='bold'))
gg4 <- gg4 + theme(plot.title=element_text(size=15, face = 'bold'))
gg4 <- gg4 + theme(legend.text = element_text(size=10))
gg4 <- gg4 + theme(legend.title = element_text(size=10, face = 'bold'))
# gg4

# Area plot
gg5 <- ggplot(data=mdwide_t[mdwide_t$Intervencion!='impacto' & mdwide_t$impactparameter=='Area',], aes(y=Intervencion, x=region, fill=Value)) + geom_tile(color="white", size=0.1)# + facet_wrap(~impactparameter, scales="free", ncol=6)
gg5 <- gg5 + scale_fill_gradient2(name='Average Change', low="darkblue", high="darkgreen", guide="colorbar") # limits=c(-15, 80), 
gg5 <- gg5 + coord_equal()
gg5 <- gg5 + ggtitle('Area') + ylab('') + xlab('Countries')
gg5 <- gg5 + theme_bw()
gg5 <- gg5 + theme(axis.text.x=element_text(size=10, angle=90))
gg5 <- gg5 + theme(axis.text.y=element_text(size=10))
gg5 <- gg5 + theme(axis.title.x=element_text(size=12, face='bold'))
gg5 <- gg5 + theme(plot.title=element_text(size=15, face = 'bold'))
gg5 <- gg5 + theme(legend.text = element_text(size=10))
gg5 <- gg5 + theme(legend.title = element_text(size=10, face = 'bold'))
#gg5

grid.arrange(gg, gg1, gg2, gg3, gg4, gg5)

g <- arrangeGrob(gg, gg2, gg5, nrow=3) #generates g
h <- arrangeGrob(gg1, gg4, gg3, nrow=3) #generates g
ggsave(file="C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/datos procesados/oferta.png", g, width=10, height=10.5, units='in') #saves g
ggsave(file="C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/datos procesados/comercio_demanda.png", h, width=10, height=10.5, units='in') #saves g

#Comparar todos los escenarios con cambio climatico con el escenario sin cambio climatico y sin adopcion de-----
# tecnologia


impactoDF <- subset(mdwide_t, subset=mdwide_t$Intervencion=='impacto')

impactList <- as.character(unique(impactoDF$impactparameter))
ggList <- lapply(1:length(impactList), function(i){
  
  df <- subset(impactoDF, impactoDF$impactparameter==impactList[i])
  df$region <- as.character(df$region)
  df$trend <- as.character(df$trend)
  df <- df[complete.cases(df),]  # borrar filas con NA
  df$region <- factor(df$region, levels=df$region[order(df$Value)])  # ordenar las regiones por valor de los impactos Value
  
  gg <- ggplot(df, aes(x=region, y=Value, fill=trend)) + theme_bw() + geom_bar(stat="identity") + coord_flip()
  gg <- gg + ggtitle(impactList[i]) + xlab('Countries') + ylab('Relative differences')
  gg <- gg + theme_bw()
  
  if(i==2){
    
    gg <- gg + theme(axis.text.y=element_text(size=12))
    
  } else {
    
    gg <- gg + theme(axis.text.y=element_text(size=1))
    
  }
  
  gg <- gg + theme(axis.text.y=element_text(size=12))
  gg <- gg + theme(axis.title.x=element_text(size=12, face='bold'))
  gg <- gg + theme(plot.title=element_text(size=15, face = 'bold'))
  # gg <- gg + scale_colour_gradient(name='Relative differences', low="cadetblue1", high="dodgerblue3")
  # gg <- gg + scale_fill_gradient(name='Relative differences', low="cadetblue1", high="dodgerblue3")
  
  return(gg)
  
})

grid.arrange(ggList[[1]], ggList[[2]], ggList[[3]], ggList[[4]], ggList[[5]], ggList[[6]])

g <- arrangeGrob(ggList[[6]], ggList[[4]], ggList[[5]], nrow=3) #generates g
h <- arrangeGrob(ggList[[1]], ggList[[2]], ggList[[3]], nrow=3) #generates g
ggsave(file="C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/datos procesados/impacto_oferta.png", g, width=10, height=10.5, units='in') #saves g
ggsave(file="C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/datos procesados/impacto_comercio_demanda.png", h, width=10, height=10.5, units='in') #saves g

ggsave(file="C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/datos procesados/demandaBarplot.png", ggList[[1]], width=10, height=10.5, units='in') #saves g
ggsave(file="C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/datos procesados/exportBarplot.png", ggList[[2]], width=10, height=10.5, units='in') #saves g
ggsave(file="C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/datos procesados/importBarplot.png", ggList[[3]], width=10, height=10.5, units='in') #saves g
ggsave(file="C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/datos procesados/productionBarplot.png", ggList[[4]], width=10, height=10.5, units='in') #saves g
ggsave(file="C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/datos procesados/areaBarplot.png", ggList[[5]], width=10, height=10.5, units='in') #saves g
ggsave(file="C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/datos procesados/yieldBarplot.png", ggList[[6]], width=10, height=10.5, units='in') #saves g


#Reformatear datos para realizar graficos

mdwidet_melt <- mdwide_t[mdwide_t$Intervencion=='impacto',] %>% spread(impactparameter, Value)
mdwidet_melt2 <- mdwidet_melt

mdwidet_melt$Demand <- NULL
mdwidet_melt$Export <- NULL
mdwidet_melt$Import <- NULL

mdwidet_melt <- mdwidet_melt[complete.cases(mdwidet_melt),]

#Analisis de cluster-----

x <- as.matrix(scale(mdwidet_melt[,4:6]))
rownames(x) <- mdwidet_melt$region
hclustDF <- hclust(dist(x))
plot(hclustDF)
#Guardar el archivo
tiff(filename=paste(grd,"clusterW.tiff",sep=""), 
     width = 10, height = 10, units = 'in', res = 100)

plot(hclustDF)

dev.off()

#obtener estadisticas y mejorar graficos 
ctree <- cutree(hclustDF, k=7)
rect.hclust(hclustDF, k=7, border="red")



tiff(filename=paste(grd,"clusterRred.tiff",sep=""), 
     width = 10, height = 10, units = 'in', res = 100)

plot(hclustDF)
rect.hclust(hclustDF, k=7, border="red")

dev.off()

# 

mdwidet_melt$Cluster <- NA
for(i in 1:nrow(mdwidet_melt))
{
  mdwidet_melt$Cluster[i] <- ctree[which(mdwidet_melt$region[i]==names(ctree))]
}

library(dplyr)

data<- as.data.frame(summarise(group_by(mdwidet_melt, Cluster), meanProd=mean(Production), meanArea=mean(Area), meanYi=mean(Yield)))
write.table(data, "C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/data.txt", sep="\t")
write.table(mdwidet_melt, "C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/datacompleteALC.txt", sep="\t")

# Analisis de datos sin cambio climatico-----
#data inicial
mdsubcrop # base de datos inciales
lac_africa_reg<-levels(mdsubcrop$region)[c(68:96,121,127:161)]
mdsubcrop_lac_afri<-mdsubcrop[which(mdsubcrop$region %in% lac_africa_reg),]
mdsubcrop_lac_afri<-droplevels(mdsubcrop_lac_afri)
mdwide <- reshape(mdsubcrop_lac_afri, v.names = "Val", idvar = 
                    c("scenario","commodity","region","productiontype",
                      "impactparameter","categoria"),timevar = "year", direction = "wide")

mdwide<- mdwide[!mdwide$region=="LAC",]
mdwide<- mdwide[!mdwide$region=="SSA",]
mdwide<- mdwide[!mdwide$region=="MEN",]
mdwide<-mdwide[!mdwide$region=="LAC-Cuba",]
mdwide<-mdwide[!mdwide$region=="LAC-Jamaica",]
mdwide<-mdwide[!mdwide$region=="MEN-Egypt",]
mdwide<-mdwide[!mdwide$region=="LAC-Other Caribbean",]
mdwide<-mdwide[!mdwide$region=="LAC-Haiti",]
mdwide<-mdwide[!mdwide$region=="LAC-Dominican Republic",]

write.table(mdwide, "C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/DataTotal.txt", sep="\t")
mdwide<- data.frame(mdwide,"Cambio Porcentual"=( (mdwide$Val.2050-mdwide$Val.2020)/mdwide$Val.2020)*100)
mdwide<-data.frame(mdwide[,1:6],"Cambio Porcentual"=mdwide[,13]) # eliminamos las columnas innecesarias 

mdwide_st <- mdwide

# importante para renombrar las columnas
rownames(mdwide) <- 1:nrow(mdwide)
mdwide_t<-mdwide %>%
  spread("scenario","Cambio.Porcentual")

cambiop<-mdwide_t

mdwide$categoria <- as.character(mdwide$categoria)

testHarold <- mdwide

# testHarold <- subset(mdwide, subset=mdwide$impactparameter=='Yield')
hist(testHarold$Cambio.Porcentual[which(testHarold$categoria=='NoCC_NoTec' | testHarold$categoria=='NoCC_Tec')])
boxplot(testHarold$Cambio.Porcentual[-which(testHarold$categoria=='NoCC_NoTec' | testHarold$categoria=='NoCC_Tec')]~testHarold$scenario[-which(testHarold$categoria=='NoCC_NoTec' | testHarold$categoria=='NoCC_Tec')])
testHarold <- testHarold[which(testHarold$categoria=='NoCC_NoTec' | testHarold$categoria=='NoCC_Tec'),]

#reshape
mdwide_t <- testHarold %>%
  spread("scenario","Cambio.Porcentual")
mdwide_t$productiontype <- NULL
mdwide_t$mean <- rowMeans(x=mdwide_t[,5:ncol(mdwide_t)], na.rm=TRUE)

# para mantener estas variables
mdwide_t <- mdwide_t[,c("impactparameter", "commodity", "region", "categoria", "mean")]
# reshape
mdwide_t <- mdwide_t %>% spread(categoria, mean)

# exportar datos para procesarlos en excel
write.table(mdwide_t, "C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/SinCC.txt", sep="\t")

# cambio de forma
mdwide_t <- mdwide_t %>% gather( Escenario, Value, 4:5)


#  plotting box plot yield
rcc<-ggplot(mdwide_t[which(mdwide_t$impactparameter=="Yield"),], aes(x=Value, colour=Escenario)) + geom_density(alpha=.3) 
rcc<- rcc + scale_colour_discrete(name="Scenarios\nPercent Change")
rcc<- rcc + xlab("% Change") + ggtitle("Yield change, without climate change")  
rcc<- rcc + theme(plot.title = element_text(lineheight=.1, face="bold")) 

rcc
ggsave(file="C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/YieldNoCC.png")   


#  plotting box plot Area
pn<-ggplot(mdwide_t[which(mdwide_t$impactparameter=="Area"),], aes(x=Value, colour=Escenario)) + geom_density(alpha=.3) 
pn<- pn + scale_colour_discrete(name="Scenarios\nPercent Change")
pn<- pn + xlab("% Change") + ggtitle("Area change, without climate change")  
pn<- pn + theme(plot.title = element_text(lineheight=.1, face="bold")) 

pn
ggsave(file="C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/AreaNoCC.png")    

#  plotting box plot produccion
pp<-ggplot(mdwide_t[which(mdwide_t$impactparameter=="Production"),], aes(x=Value, colour=Escenario)) + geom_density(alpha=.3) 
pp<- pp + scale_colour_discrete(name="Scenarios\nPercent Change")
pp<- pp + xlab("% Change") + ggtitle("Production change, without climate change")  
pp<- pp + theme(plot.title = element_text(lineheight=.1, face="bold")) 

pp
ggsave(file="C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/ProductionNoCC.png")    
a<-arrangeGrob(rcc, pn, pp, ncol=2, nrow =2)
ggsave(file="C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/totalgprah.png",a, width=10, height=10.5, units='in')       


# subset Solo cambio climatico 
rendicc<-subset(mdwide, categoria!="NoCC_NoTec" & categoria!="NoCC_Tec")

# Creando  las variables con tecnologia y sin tecnologia--------
rendicc_tech$adoption<-as.character(rendicc_tech$variable)
rendicc$productiontype <- NULL
rendicc$commodity<- NULL


dh<- ggplot(rendicc[which(rendicc$impactparameter=="Yield"),], aes(x=Cambio.Porcentual, colour=categoria)) + geom_density(alpha=.3)
dh<- dh + scale_colour_discrete(name="Scenarios\nPercent Change")
dh<- dh + xlab("% Change") 
dh<- dh + ggtitle("Yield, with Technology\n& without Technology by GCMs") 
dh<- dh + theme(plot.title = element_text(lineheight=.8, face="bold")) 
dh
ggsave(file="C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/HistograTechnYieldCC.png", dh, width=10, height=10.5, units='in')                                                                                       

#Calculo de produccion medio entre 2020-2050------

q # base de datos

q<- q[!q$region=="LAC",]
q<- q[!q$region=="SSA",]
q<- q[!q$region=="MEN",]
q<-q[!q$region=="LAC-Cuba",]
q<-q[!q$region=="LAC-Jamaica",]
q<-q[!q$region=="MEN-Egypt",]
q<-q[!q$region=="LAC-Other Caribbean",]
q<-q[!q$region=="LAC-Haiti",]
q<-q[!q$region=="LAC-Dominican Republic",]

write.table(mdwide, "C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/Soloproductin.txt", sep="\t")
q$q_mean <- rowMeans(x=q[,9:ncol(q)], na.rm=TRUE)
only_q<-q[which(q$impactparameter=="Production"),]
only_q$commodity<-NULL
only_q$productiontype<-NULL
only_q <- only_q[,c("impactparameter", "region", "categoria", "q_mean")]

only_q <- only_q %>% spread(categoria, q_mean)

# Yield subset
yieldDF <- impactoDF[impactoDF$impactparameter=='Yield',]

# produccion sub set
only_qTotal <- only_q %>% group_by(region,categoria) %>% summarise(promedio=mean(q_mean))
only_qTotal <- as.data.frame(only_qTotal)

only_qTotal <- only_qTotal[only_qTotal$categoria!='NoCC_NoTec',]
only_qTotal <- only_qTotal[only_qTotal$categoria!='NoCC_Tec',]


# solo produccion con tecnología.

head(only_qTotal)

yieldDF$region <- factor(yieldDF$region, levels=yieldDF$region[order(yieldDF$Value)])
only_qTotal$region <- factor(x=only_qTotal$region, levels=levels(yieldDF$region))
gg_prod <- ggplot(only_qTotal, aes(x=region, y=promedio, fill=categoria)) + theme_bw() + geom_bar(stat="identity", position = "dodge") + coord_flip()
gg_prod <- gg_prod + ggtitle('') +  ylab('Production average (ton)') + xlab(' ') + scale_fill_discrete("Categories")
gg_prod <- gg_prod + theme_bw() + ggtitle('Production')
gg_prod <- gg_prod + theme(axis.text.y=element_text(size=1))
gg_prod <- gg_prod + theme(axis.text.y=element_text(size=12))
gg_prod <- gg_prod + theme(axis.title.x=element_text(size=12, face='bold'))
gg_prod <- gg_prod + theme(plot.title=element_text(size=15, face = 'bold'))
gg_prod

ggList[[6]]
h_prod <- arrangeGrob(ggList[[6]], gg_prod, nrow=1) #generates g
ggsave(file="C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/datos procesados/production_yield.png", h_prod, width=10, height=10.5, units='in') #saves g


# grafico solo produccion con tecnologia 

only_qTotalTech <- only_qTotal[only_qTotal$categoria!="CC_NoTec",]
only_qTotalTech$categoria <- as.character(only_qTotalTech$categoria)

yieldDF$region <- factor(yieldDF$region, levels=yieldDF$region[order(yieldDF$Value)])
only_qTotalTech$region <- factor(x=only_qTotalTech$region, levels=levels(yieldDF$region))
gg_prod2 <- ggplot(only_qTotalTech, aes(x=region, y=promedio, fill=categoria)) + theme_bw() + geom_bar(stat="identity", position = "dodge") + coord_flip()
gg_prod2 <- gg_prod2 + ggtitle('') +  ylab('Production average (ton)') + xlab(' ') + scale_fill_discrete("Categories")
gg_prod2 <- gg_prod2 + theme_bw() + ggtitle('Production')
gg_prod2 <- gg_prod2 + theme(axis.text.y=element_text(size=1))
gg_prod2 <- gg_prod2 + theme(axis.text.y=element_text(size=12))
gg_prod2 <- gg_prod2 + theme(axis.title.x=element_text(size=12, face='bold'))
gg_prod2 <- gg_prod2 + theme(plot.title=element_text(size=15, face = 'bold'))
gg_prod2

ggList[[6]]
h_prod2 <- arrangeGrob(ggList[[6]], gg_prod2, nrow=1) #generates g
ggsave(file="C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/datos procesados/production_yield2.png", h_prod2, width=10, height=10.5, units='in') #saves g

#Calculo de la media de los cambios  porcentuales de los rendimientos con tecnologia ------
rendicc
meanyield<- rendicc[which(rendicc$impactparameter=="Yield"),]
meanyieldSum <- meanyield %>% group_by(region,categoria) %>% summarise(promedio=mean(Cambio.Porcentual))
table(meanyieldSum$categoria ) # forma de visualizar los datos tipo tabla

write.csv(x = meanyieldSum, file ="C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/datos procesados/meantechnolgy.csv")
