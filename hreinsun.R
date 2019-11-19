
data <- read.csv('data.csv',header = T,dec = ".",na.strings = "")
library(dplyr)
library(magrittr)
data <- data %>% filter_all(any_vars(!is.na(.)))

blank <- data[grep("lank",data$Sample),]
mp <- data[grep("lank",data$Sample,invert=T),]
mp <- mp[!is.na(mp$Type),]

for (i in 6:9) {
print(data[grepl(" |,|[[:alpha:]]", data[,i]),i])
}

plyr::ddply(mp,.(Type),summarize,area=sum(Area,na.rm = T),width=sum(Width,na.rm = T),Length=sum(Length..um.,na.rm = T))

table(mp$Type)
table(blank$Type)