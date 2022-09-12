library(maptools)
library(maps)
library(SDMTools) # obsolete
library(adehabitatHS)
library(sp)
  
lon.min=10
lon.max=130 #d�fini la fenetre de l'oc�an indien ou les petrels peuvent potentiellement s'alimenter
lat.min=-40
lat.max=30
lon.col=55.5
lat.col=-21.25


data=read.table("TousTrajets_MIG.txt", header=T, row.names = NULL,sep="\t")
d=read.asciigrid("LBAthym.asc")
jet.colors <- colorRampPalette(c("gray55","gray75", "gray85", "gray93"))
jet.colors <- colorRampPalette(c("black","#00007F", "#007FFF", "#9ECAE1"))
te=readShapeSpatial("GSHHS_i_L1.shp",force_ring=TRUE)
colx<-rgb(0.3, 0.7, 0.7, alpha = 0.8)
coly<-rgb(1, 0.5, 0.5, alpha = 0.8)
colz<-rgb(0.5, 0.5, 1, alpha = 0.8)

###kernel 50-90 internuptial PNA
# Kernel pop 50+90 une colo R + une colo NR#####
condM=data$TYPE=="MIG"
datM=subset(data,condM) #on prend seulement les mig de data
datM=SpatialPoints(data.frame(cbind(datM$LON,datM$LAT)))                  
KUD = kernelUD(datM, h=2, grid=500) 
KUDvol <- getvolumeUD(KUD)
ver90M <- getverticeshr(KUDvol, 90)
ver50M <- getverticeshr(KUDvol, 50)
png("Kernel_general_inter-nup_5090b.png", width=1600, height=1000, units = "px", pointsize = 12, bg = "white", res = 100)
par(mai=c(1,1,0.8,0.8))
plot(lon.col,lat.col,pch=16, col="white",cex.lab=2,font.lab=2,xlab="Longitude", ylab="Latitude",cex.axis=1.5,xlim=c(lon.min,lon.max),ylim=c(lat.min,lat.max))
image(d,add=T,col=jet.colors(100))
plot(ver90M, add=TRUE, lwd=1.5, border= "#bfd1ff", col="#bfd1ff")
plot(ver50M, add=TRUE, lwd=1.5, border= "deepskyblue4", col="deepskyblue4")
plot(te,add=T,col="grey94")
points(lon.col,lat.col, pch=23, bg="yellow", cex=3)
legend("topright", legend=c("Kernel90 durant la p�riode internuptiale","Kernel50 durant la p�riode internuptiale"),fill=c("#bfd1ff","deepskyblue4"), horiz=F, cex=1.5, bg='white',inset=0.02) 
box(lwd=2)
dev.off()

###Kernel 50 compo et hors repro##### FIG1
condR=data$TYPE=="REPRO"
datR=subset(data,condR) 
datR=SpatialPoints(data.frame(cbind(datR$LON,datR$LAT)))  
KUD = kernelUD(datR, h=2, grid=500)
KUDvol <- getvolumeUD(KUD)
ver50R <- getverticeshr(KUDvol, 50)

png("FIG_repro_compoMig_grey.png", width=1450, height=1000, units = "px", pointsize = 12, bg = "white", res = 100)
par(mai=c(1,1,0.8,0.8))
plot(lon.col,lat.col,cex.lab=1.8,font.lab=2,xlab="Longitude", ylab="Latitude",cex.axis=1.5,xlim=c(lon.min,lon.max),ylim=c(lat.min,lat.max))
image(d,add=T,col=jet.colors(100))
for (i in 1:length(unique(data$Compo)))
{
  nom_du_fichier_i=unique(data$Compo)[i]
  dat=data[data$Compo==nom_du_fichier_i,]
  condM=dat$TYPE=="MIG"
  datM=subset(dat,condM)
  datM=SpatialPoints(data.frame(cbind(datM$LON,datM$LAT))) 
  KUD = kernelUD(datM, h=2, grid=500) 
  KUDvol <- getvolumeUD(KUD)
  ver50Mi <- getverticeshr(KUDvol, 50)
  plot(ver50Mi, add=TRUE, lwd=1.5, border="orange3", col="goldenrod1")
}


plot(ver50R, add=TRUE, lwd=1.5, border="olivedrab", col="olivedrab1")
plot(te,add=T,col="gray90")
text(80,-5,"A",cex=2,col="white",font=2)
text(60,-10,"B",cex=2,col="white",font=2)
text(66,15,"C",cex=2,col="white",font=2)
text(86,12,"D",cex=2,col="white",font=2)
text(120,-15,"E",cex=2,col="white",font=2)
points(lon.col,lat.col,pch=24,col="black",bg="white",cex=2.5)
legend("topright", legend = c("Breeding period","Non-breeding period"),fill=c("olivedrab1","goldenrod1"), border="black",text.font= 2, bty = "o", cex = 1.8, box.lwd=2, bg="white")
box(lwd=2)
dev.off()

##FIG KERNELS INDIVIDUELS

for (i in 1:length(unique(data$ID)))
{
  nom_du_fichier_i=unique(data$ID)[i]
  dat=data[data$ID==nom_du_fichier_i,]
  condM=dat$TYPE=="MIG"
  datM=subset(dat,condM)
  datM=SpatialPoints(data.frame(cbind(datM$LON,datM$LAT)))                    
  KUD = kernelUD(datM, h=2, grid=500)
  KUDvol <- getvolumeUD(KUD)
  ver90M <- getverticeshr(KUDvol, 90)
  ver50M <- getverticeshr(KUDvol, 50)
  png(paste(nom_du_fichier_i, "_kernelM.png", sep=""), width=1500, height=1000, units = "px", pointsize = 12, bg = "white", res = 100)
  plot(lon.col,lat.col,pch=16, col="white",cex.lab=2,font.lab=2,xlab="Longitude", ylab="Latitude",cex.axis=1.5,xlim=c(lon.min,lon.max),ylim=c(lat.min,lat.max))
  plot(ver90M, add=TRUE, lwd=1.5, border= "#bfd1ff", col="#bfd1ff")
  plot(ver50M, add=TRUE, lwd=1.5, border= "#004FFF", col="#004FFF")
  map('world', add=T, fill=T, col=gray(0.86), border=gray(0.5))
  dev.off()
  
}
