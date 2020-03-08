####
### notes: 
### external 
### https://github.com/jdallmann/winnipegr
### censusmapper.ca
### https://drive.google.com/file/d/1e-zmzDY1O7oRJkTZrNehNQgTeTiGCTxS/view

library(sf)

#####
#### utilizing winnipegr package to download
#####
##install.packages("devtools")
##devtools::install_github("jdallmann/winnipegr")

library(winnipegr)
#neighbourhoods_2017


#########3
### load Andy's table
#########
crimeytd<-read.csv("data/YTD_crime_map_data.csv", fill=T)
#neigh@data$nam %in% crimeytd$Neighbourhoods
sum(neighbourhoods_2017$name %in% crimeytd$Neighbourhoods)

crime<-as.matrix(table(crimeytd$Neighbourhoods))
crime<-data.frame(Neighbourhoods=rownames(crime), rate=crime[,1])
write.table(crime, file="output/neighbourhood_crime.txt", row.names=F)
#criNei<-merge(neighbourhoods_2017, crimeytd,by.x="name", by.y="Neighbourhoods", all.x=T)

criNeiFreq<-merge(neighbourhoods_2017, crime, by.x="name", by.y="Neighbourhoods", all.x=T)
sum(criNeiFreq$rate, na.rm=T) 
#55252 total numbers of crimes

pdf("output/Hist_CrimeRate.pdf", width=4,height=3)
  par(mar=c(4,4,1,1))
  hist(criNeiFreq$rate, breaks=50,main="Crime rate:neighbourhood", xlab="Total Annual Crime Cases")
dev.off()


houseData<-st_read("data/house_data.csv", options=c("X_POSSIBLE_NAMES=House_Long","Y_POSSIBLE_NAMES=House_Lat"))
houseData$House_Price <- as.numeric(as.character(houseData$House_Price))
st_crs(criNeiFreq)
st_crs(houseData)
crsV<-st_crs(criNeiFreq)
houseData <-st_set_crs(houseData, 4326 )
# houseData <-st_transform(houseData, crsV)

houseData2<-houseData
houseData2$House_Price<-log10(houseData2$House_Price)
#cut(houseData2$House_Price,10, include.lowest=T)

pdf("output/Community_crimerate_house4sale.pdf")
  par(mar=c(0,0,0,0))
  plot(criNeiFreq[,-c(1,2)])
  
  par(mar=c(0,0,0,0))
  plot(neigh, border='gray')
  #plot(criNeiFreq[,-c(1,2)],add=T)### need to be added otherwise the 
  plot(houseData, add=T, pch=20, col=rev(heat.colors(10))[cut(houseData2$House_Price,10, include.lowest=T)])
  
  par(mar=c(0,0,0,0))
  plot(neigh, border='gray')
  #plot(criNeiFreq[,-c(1,2)],add=T)### need to be added otherwise the 
  plot(houseData2, add=T, pch=20, col=rev(heat.colors(10))[cut(houseData2$House_Price,10, include.lowest=T)])

dev.off()




houseNeigh<-st_within(houseData,criNeiFreq)
houseNeigh2<-sapply(houseNeigh, function(y){if(length(y)==0){ret=NA}else{ret=y};ret})
houseData$NeighbourHoods=criNeiFreq[houseNeigh2,"name"]

### getting average house price
neiPriceAvg<-aggregate(as.numeric(as.character(houseData$House_Price)), by=list(houseData$NeighbourHoods$name), function(y) mean(y,na.rm=T))
criNeiFreqHouse<-merge(criNeiFreq, neiPriceAvg, by.x="name", by.y="Group.1", all.x=T)


colnames(criNeiFreqHouse)[4]="price"

##### plot of crime rate vs price logged
library(ggplot2)
pdf("output/Scatterplot_crime_rate.pdf")
  sp<-ggplot(criNeiFreqHouse,aes(x=rate,y=price))+ geom_point()
  sp+ scale_x_continuous(trans='log10') +
    scale_y_continuous(trans='log10')+ 
    geom_smooth(method='lm', formula= y~x)+
    theme(text = element_text(size=20))
dev.off()

sp<-ggplot(criNeiFreqHouse,aes(x=rate,y=price))+ geom_point()
sp+ geom_smooth(method='loess', formula= y~x)+
  theme(text = element_text(size=20))

pri <- log10(criNeiFreqHouse$price)
rat <-log10(criNeiFreqHouse$rate)
candp<-lm(pri~rat)
summary(candp)
#log10(criNeiFreqHouse$rate) -0.09846    0.02023  -4.868 2.34e-06 ***


10^predict(candp, new=data.frame(rat=c(0,1,2,3)))

nona<-!is.na(criNeiFreqHouse$price)&!is.na(criNeiFreqHouse$rate)
cor.test(log10(criNeiFreqHouse$price)[nona],log10(criNeiFreqHouse$rate)[nona])
#-0.3314802, p-value = 2.343e-06



