
data <- read.csv2('data.csv',header = T,dec = ".",na.strings = "")
library(dplyr)
library(magrittr)
data <- data %>% filter_all(any_vars(!is.na(.)))  # This is to get rid of the empty rows

blank <- data[grep("lank",data$Sample),] # These are the blanks
mp <- data[grep("lank",data$Sample,invert=T),] # these are the samples
mp <- mp[!is.na(mp$Type),]

for (i in 6:9) {
print(data[grepl(" |,|[[:alpha:]]", data[,i]),i])
} #To check for commas in the variables of columns with numerical values. And to see if they aren't all only comprised of numbers but not alphabetical letters. Then I just fixe it in the .csv document.

library(DT)
# The column "Sample" should be divided into two columns: 'Station' with the station number, and 'Sample' with A, B, C etc.

# The different names of colors can be simplified by having a column for the main color first and then more columns for the less prominent colors. A column with a binary could also be inserted to indicate whether the color is either striped or separated or if it's a blended color.
datatable(matrix(c(levels(data$Colour),unname(table(data$Colour))),nrow = length(levels(data$Colour)),dimnames = list(c(),c("Colour","N"))))

# The types of MP's could be simplified so that there are no in-between types. The in-betweens are so few, better just make a decision
datatable(matrix(c(levels(data$Type),unname(table(data$Type))),nrow =length(levels(data$Type)),dimnames = list(c(),c("Type","N"))))

plyr::ddply(mp,.(Type),summarize,area=sum(Area,na.rm = T),width=sum(Width,na.rm = T),Length=sum(Length..um.,na.rm = T))

table(mp$Type)
table(mp$Colour)
table(blank$Type)

library(DT)
library(data.table)
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
ddply(mp,.(Station,Type),summarise,Area=mean(Area),Width=mean(Width),Length=mean(Length),N=length(Sample))
