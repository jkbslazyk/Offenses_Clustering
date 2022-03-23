library("sp")
library("rgdal")
library("ggplot2")
library("dbscan")
library("repr")
library("broom")
library("opticskxi")

getwd()
data_conv<-read.csv2("zestaw9.csv",sep=";")
head(data_conv)

cord.dec = SpatialPoints(cbind(data_conv$Long, data_conv$Lat), proj4string=CRS("+proj=longlat"))
cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:2178"))

head(cord.UTM)

x<-getwd()
my_spdf <- readOGR(dsn=path.expand('C:/Users/jakub/OneDrive/Dokumenty/osiedla'), layer='osiedla' )

par(mar=c(0,0,0,0))
plot(my_spdf, col="#f2f2f2", bg="skyblue", lwd=0.25, border=0 )

spdf_fortified <- tidy(my_spdf)
cords <- as.data.frame(cord.UTM)
colnames(cords)<-c("X","Y")
head(cords)
getwd()

write_txt(cords, file = "C:/Users/jakub/OneDrive/Dokumenty/cords.txt")

ggplot() +
  geom_polygon(data =my_spdf, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  geom_point(data = cords, aes(x = X, y = Y),color="red",
             size = 1)+
  theme_void() 

set.seed(123456789)
dbscan_res <- dbscan(cords, eps = 400, minPts = 30)
color<-dbscan_res$cluster+1
plot1<-ggplot() +
  geom_polygon(data = my_spdf, aes( x = long, y = lat, group = group), fill="#E8DAB2", color="white") +
  geom_point(data = cords, aes(x = X, y = Y,color=factor(color)), size = 0.9) +
  theme_void() +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  ggtitle("DBSCAN\neps=400, minPts=30") +
  theme(plot.title = element_text(hjust = 0.5))+
  guides(colour = guide_legend(override.aes = list(size=4)))
plot1

as.data.frame(table(color))

dbscan_res <- dbscan(cords, eps = 800, minPts = 60)
color<-dbscan_res$cluster+1
plot2<-ggplot() +
  geom_polygon(data = my_spdf, aes( x = long, y = lat, group = group), fill="#E8DAB2", color="white") +
  geom_point(data = cords, aes(x = X, y = Y,color=factor(color)), size = 0.9) +
  theme_void() +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  ggtitle("DBSCAN\neps=800, minPts=60") +
  theme(plot.title = element_text(hjust = 0.5))+
  guides(colour = guide_legend(override.aes = list(size=4)))
plot2

as.data.frame(table(color))

dbscan_res <- dbscan(cords, eps = 200, minPts = 15)
color<-dbscan_res$cluster+1
plot3<-ggplot() +
  geom_polygon(data = my_spdf, aes( x = long, y = lat, group = group), fill="#E8DAB2", color="white") +
  geom_point(data = cords, aes(x = X, y = Y,color=factor(color)), size = 0.9) +
  theme_void() +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  ggtitle("DBSCAN\neps=200, minPts=15") +
  theme(plot.title = element_text(hjust = 0.5) )+
  guides(colour = guide_legend(override.aes = list(size=4)))
plot3

as.data.frame(table(color))

hdbscan_res <- hdbscan(cords, minPts = 30)
color<-hdbscan_res$cluster+1
plot4<-ggplot() +
  geom_polygon(data = my_spdf, aes( x = long, y = lat, group = group), fill="#E8DAB2", color="white") +
  geom_point(data = cords, aes(x = X, y = Y,color=factor(color)), size = 0.9) +
  theme_void() +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  ggtitle("HDBSCAN\nminPts=30") +
  theme(plot.title = element_text(hjust = 0.5) ) +
  guides(colour = guide_legend(override.aes = list(size=4)))
plot4

as.data.frame(table(color))

hdbscan_res <- hdbscan(cords, minPts = 15)
color<-hdbscan_res$cluster+1
plot5<-ggplot() +
  geom_polygon(data = my_spdf, aes( x = long, y = lat, group = group), fill="#E8DAB2", color="white") +
  geom_point(data = cords, aes(x = X, y = Y,color=factor(color)), size = 0.9) +
  theme_void() +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  ggtitle("HDBSCAN\nminPts=15") +
  theme(plot.title = element_text(hjust = 0.5) ) +
  guides(colour = guide_legend(override.aes = list(size=4)))
plot5

as.data.frame(table(color))

hdbscan_res <- hdbscan(cords, minPts = 60)
color<-hdbscan_res$cluster+1
plot6<-ggplot() +
  geom_polygon(data = my_spdf, aes( x = long, y = lat, group = group), fill="#E8DAB2", color="white") +
  geom_point(data = cords, aes(x = X, y = Y,color=factor(color)), size = 0.9) +
  theme_void() +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  ggtitle("HDBSCAN\nminPts=60") +
  theme(plot.title = element_text(hjust = 0.5) ) +
  guides(colour = guide_legend(override.aes = list(size=4)))
plot6

as.data.frame(table(color))

optic_res <- optics(cords, minPts = 40)
optic_res2 <- extractXi(optic_res, xi = 0.04)
ggplot_optics(optic_res, groups = optic_res2$cluster)
color<-optic_res2$cluster+1
plot7<-ggplot() +
  geom_polygon(data = my_spdf, aes( x = long, y = lat, group = group), fill="#E8DAB2", color="white") +
  geom_point(data = cords, aes(x = X, y = Y,color=factor(color)), size = 0.9) +
  theme_void() +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  ggtitle("OPTICS\nminPts=40, xi=0.04") +
  theme(plot.title = element_text(hjust = 0.5) ) +
  guides(colour = guide_legend(override.aes = list(size=4)))
plot7

as.data.frame(table(color))

optic_res <- optics(cords, minPts = 100)
optic_res2 <- extractXi(optic_res, xi = 0.01)
ggplot_optics(optic_res, groups = optic_res2$cluster)
color<-optic_res2$cluster+1
plot8<-ggplot() +
  geom_polygon(data = my_spdf, aes( x = long, y = lat, group = group), fill="#E8DAB2", color="white") +
  geom_point(data = cords, aes(x = X, y = Y,color=factor(color)), size = 0.9) +
  theme_void() +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  ggtitle("OPTICS\nminPts=100, xi=0.01") +
  theme(plot.title = element_text(hjust = 0.5) ) +
  guides(colour = guide_legend(override.aes = list(size=4)))
plot8

as.data.frame(table(color))
