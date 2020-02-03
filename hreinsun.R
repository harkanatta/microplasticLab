
#data <- read.csv2('data.csv',header = T,dec = ".",na.strings = "")
library(dplyr)
library(plyr)
library(magrittr)
library(DT)
library(data.table)
#data <- data %>% filter_all(any_vars(!is.na(.)))  # This is to get rid of the empty rows
#
#blank <- data[grep("lank",data$Sample),] # These are the blanks
#mp <- data[grep("lank",data$Sample,invert=T),] # these are the samples
#mp <- mp[!is.na(mp$Type),]
#
#for (i in 6:9) {
#print(data[grepl(" |,|[[:alpha:]]", data[,i]),i])
#} #To check for commas in the variables of columns with numerical values. And to see if they aren't all only comprised of numbers but not alphabetical letters. Then I just fixe it in the .csv document.


# The column "Sample" should be divided into two columns: 'Station' with the station number, and 'Sample' with A, B, C etc.

# The different names of colors can be simplified by having a column for the main color first and then more columns for the less prominent colors. A column with a binary could also be inserted to indicate whether the color is either striped or separated or if it's a blended color.
#data <- read.csv('mp.csv',header = T,dec = ".",na.strings = "")
data <- read.csv('MPdata2019Karin.csv',header = T,dec = ".",na.strings = "")
data$SizeClass <- ifelse(data$Length<=100,"small",ifelse( data$Length>100 & data$Length<=350,"medium","large"))
datatable(ddply(data,.(Filter),summarise,N=length(Filter))) #fjöldi agna á filter
# litir <- unique(data$Colour)
# fjoldi <- unname(table(data$Colour))
# datatable(matrix(c(litir,fjoldi),nrow = length(unique(data$Colour)),dimnames = list(c(),c("Colour","N"))))
litatafla <- ddply(data,.(Colour),summarise,N=length(Colour))
datatable(litatafla)%>% 
  formatStyle(names(litatafla)[1],
              background = styleColorBar(range(litatafla[,2],na.rm = T), 'lightblue'),
              backgroundSize = '98% 88%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center')

# The types of MP's could be simplified so that there are no in-between types. The in-betweens are so few, better just make a decision
datatable(matrix(c(levels(data$Type),unname(table(data$Type))),nrow =length(levels(data$Type)),dimnames = list(c(),c("Type","N"))))

plyr::ddply(data,.(Type),summarize,area=sum(Area,na.rm = T),width=sum(Width,na.rm = T),Length=sum(Length,na.rm = T),N=length(Type))

table(mp$Type)
table(mp$Colour)
table(blank$Type)


datatable(mp[order(mp[,6],decreasing = T),]) %>% 
  formatStyle(names(mp)[6],
              background = styleColorBar(range(mp[,6],na.rm = T), 'lightblue'),
              backgroundSize = '98% 88%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center') %>% 
  formatStyle(names(mp)[7],
              background = styleColorBar(range(mp[,7],na.rm = T), 'lightblue'),
              backgroundSize = '98% 88%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center') %>% 
  formatStyle(names(mp)[8],
              background = styleColorBar(range(mp[,8],na.rm = T), 'lightblue'),
              backgroundSize = '98% 88%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center')

tafla <- ddply(mp,.(Sample,Type,Colour),summarise,Area=mean(Area),Width=mean(Width),Length=mean(Length..um.),N=length(Sample))
tafla <- ddply(mp,.(Sample,Type),summarise,Area=mean(Area),Width=mean(Width),Length=mean(Length..um.),N=length(Sample))

grepl("/",data$Colour)

library(tidyverse)
data <- read.csv2('datab.csv',header = T,dec = ".",na.strings = "NA")
data$Station <- data$Station %>% str_replace_all("[[:alpha:]]|[[:punct:]]|[[:blank:]]","")
data$Sample <- data$Sample %>% str_replace_all("[[:digit:]]|[[:blank:]]|[[:punct:]]|St","")
blank <- data[grep("lank",data$Sample),] # These are the blanks
mp <- data[grep("lank",data$Sample,invert=T),] # these are the samples
mp <- mp[!is.na(mp$Type),]
plyr::ddply(mp,.(Station,Type),summarise,Area=mean(Area),Width=mean(Width),Length=mean(Length),N=length(Sample))










small <-  data$Length<=100
medium <- data$Length>100 & data$Length<=350
large <- data$Length>350 & data$Length<=5000
data$SizeClass <- ifelse(data$Length<=100,"small",ifelse( data$Length>100 & data$Length<=350,"medium","large"))
table(data$Length)
table(is.na(data$Length))











library(RColorBrewer)
gognin <- ddply(afli[afli$Skip=='Aðrir bátar' & afli$Ár==Árið,],.(Höfn,Mánuður = droplevels(Mánuður)),summarise,magn=sum(Magn),.drop=F)
gognin <- droplevels(gognin)
gat <- matrix(gognin$magn,nrow = length(levels(as.factor(gognin$Mánuður))),ncol = length(unique(gognin$Höfn)),dimnames=list(levels(as.factor(gognin$Mánuður)),levels(gognin$Höfn)))
#colnames(gat) <- c('Norðurfjörður','Drangsnes','Hólmavík','Hvammstangi','Skagaströnd')
colnames(gat) <- unique(gognin$Höfn)
#barplot(gat,beside = T)

litir <- colorRampPalette(c('#045579', 'white','#d75f07','seashell','#069acc'))(length(dimnames(gat)[[1]]))
#png(filename="C:/Users/BioPol VS/Documents/Vinnumappa/landanir/NedriMynd.png",12,7,"cm",pointsize=6,res=900)
png(filename="NedriMynd.png",12,7,"cm",pointsize=6,res=900)
bp <- barplot(gat,ylab='',xlab='',axes=F,beside=TRUE,ylim=c(0,max(gognin$magn)*1.1))
#grid(NA,ny = NULL)
abline(h=seq(0,roundUpNice(1.1*max(gat)),roundUpNice(1.1*max(gat))/5), col = 'lightgray', lty = 3)
barplot(gat, main=paste('Landanir í Húnaflóa',Árið,'-','Ferskt'), ylab="Magn (tonn)", beside=TRUE, axes = F, col=litir ,ylim = c(0,max(gognin$magn)*1.1),add=T);box()
axis(2,seq(0,roundUpNice(1.1*max(gat)),roundUpNice(1.1*max(gat))/5),labels = seq(0,roundUpNice(1.1*max(gat))/1000,roundUpNice(1.1*max(gat))/5000),las=2)
legend('topleft', levels(as.factor(gognin$Mánuður)), fill=litir)
dev.off()

matrix(DF$)


data$blank <- ifelse(grepl("lank",data$Sample),"bl","mp")
DF <- ddply(data,.(Station,blank),summarise,N=length(Sample))
DF <- DF[-36,]
DF <- tidyr::pivot_wider(DF,names_from = Station, values_from = N)
DF <- DF[,-1]
litir=c("#ff583f","#0032bc")
png(filename="Barchart.png",12,7,"cm",pointsize=6,res=900)
barplot(as.matrix(DF),beside = T,col = litir,ylab = "MP Particles",las=2)
legend('topright',c("Samples","Blanks"),fill=litir)
box()
dev.off()

DF$N <- ifelse(DF$blank=="bl",DF$N*(-1),DF$N*(1))
DF <- ddply(DF,.(Station),summarise,N=sum(N))







myData <- rbind(Mean,SD)
myData <- as.data.frame(myData)

par(mar = c(5, 6, 4, 5) + 0.1)

myData[3,myData[1,]== max(myData[1,],na.rm = T) ]
plotTop <- max(myData[1,],na.rm = T) +
  myData[3,  myData[1,] == max(myData[1,],na.rm = T)] * 1.5

barCenters <- barplot(as.matrix(Mean),
                      beside = T, las = 2,
                      ylim = c(0, plotTop), xaxt = "n",
                      main = "Mileage by No. Cylinders and No. Gears",
                      ylab = "Miles per Gallon",
                      border = "black", axes = TRUE)

# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
     adj = 1, labels = myData$names, xpd = TRUE)

segments(barCenters, as.matrix(Mean-SD) * 1, barCenters,
         as.matrix(Mean+SD) * 1, lwd = 1.5)

arrows(barCenters, as.matrix(Mean-SD) * 1, barCenters,
       as.matrix(Mean+SD) * 1, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
